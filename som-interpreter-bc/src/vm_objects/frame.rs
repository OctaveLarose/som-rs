use crate::compiler::Literal;
use crate::value::Value;
use crate::vm_objects::block::{Block, CacheEntry};
use crate::vm_objects::class::Class;
use crate::vm_objects::method::Method;
use core::mem::size_of;
use som_core::bytecode::Bytecode;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::DerefMut;

pub(crate) const OFFSET_TO_STACK: usize = size_of::<Frame>();

// TODO: fix hacky conversions from u8 to usize and vice-versa, settle on a more uniform scheme (i.e. only usizes..)

/// Represents a stack frame.
pub struct Frame {
    /// The previous frame. Frames are handled as a linked list
    pub prev_frame: Gc<Frame>,

    /// The method the execution context currently is in.
    /// Interestingly, this is a Gc<Method> and not a pointer to MethodInfo, what we really need (it's never a primitive).
    /// In fact, we induce (minimal) runtime overhead by having to fetch the info from the Method enum regularly.
    /// So why do we do things that way? Because of moving GC. If we have a pointer to a MethodInfo, that's an inner pointer to a Method object. So when GC moves the frame, it can't update that pointer.
    /// It could update if Gc<MethodInfo> was a thing. And it was, at some point, and it turned out that really broke things (not sure why, I assume because MMTk didn't play well with a Gc<Enum> that had a Gc<SomethingElse> variant)  
    pub current_context: Gc<Method>,

    /// Bytecode index.
    pub bytecode_idx: u16,

    /// Stack pointer/index. Points to the NEXT element that can be written to the stack;
    /// Alternatively, can be seen as number of elements on the stack
    pub stack_ptr: u8,

    /// It's also stored in the current context, but we keep it here for faster access since we
    /// need it to calculate the offset to local variables
    pub nbr_args: u8,

    /// Needed for similar reasons as the number of arguments, for easier access to args and locals.
    pub max_stack_size: u8,

    /// markers. we don't use them directly. it's mostly a reminder that the struct looks different in memory... not the cleanest but not sure how else to go about it
    pub stack_marker: PhantomData<[Value]>,
    pub args_marker: PhantomData<[Value]>,
    pub locals_marker: PhantomData<[Value]>,
}

impl Frame {
    /// Allocates a frame for a block.
    /// We assume that the block is on the stack of the previous frame, as is the case when calling
    /// the primitive functions that create new blocks. We do this to make sure it's reachable
    /// during GC.
    pub fn alloc_from_block(nbr_args: usize, prev_frame: &mut Gc<Frame>, gc_interface: &mut GCInterface) -> Gc<Frame> {
        std::hint::black_box(&prev_frame);

        let (max_stack_size, nbr_locals) = {
            let block_value = prev_frame.stack_nth_back(nbr_args - 1);
            let block = block_value.as_block().unwrap();
            {
                let block_env = block.blk_info.get_env();
                (block_env.max_stack_size as usize, block_env.nbr_locals)
            }
        };

        let size = Frame::get_true_size(max_stack_size, nbr_args as u8, nbr_locals);
        let mut frame_ptr: Gc<Frame> = gc_interface.request_memory_for_type(size, AllocSiteMarker::BlockFrame);

        let block_value = prev_frame.stack_nth_back(nbr_args - 1);
        *frame_ptr = Frame::from_block(block_value.as_block().unwrap());

        let args = prev_frame.stack_n_last_elements(nbr_args);
        Frame::init_frame_post_alloc(frame_ptr.clone(), args, max_stack_size, prev_frame.clone());
        prev_frame.remove_n_last_elements(nbr_args);

        frame_ptr
    }

    /// Allocates the very first frame, for the `initialize:` call and tests.
    /// Special-cased because the normal case pushes the previous value on the previous frame's
    /// stack for it to be reachable: we have no previous frame in some cases, so we can't.
    pub fn alloc_initial_method(init_method: Gc<Method>, args: &[Value], gc_interface: &mut GCInterface) -> Gc<Frame> {
        let (max_stack_size, nbr_locals) = match &*init_method {
            Method::Defined(m_env) => (m_env.max_stack_size as usize, m_env.nbr_locals),
            _ => unreachable!("if we're allocating a method frame, it has to be defined."),
        };

        let size = Frame::get_true_size(max_stack_size, args.len() as u8, nbr_locals);

        let nbr_gc_runs = gc_interface.get_nbr_collections();
        let mut frame_ptr: Gc<Frame> = gc_interface.request_memory_for_type(size, AllocSiteMarker::InitMethodFrame);

        assert_eq!(
            nbr_gc_runs,
            gc_interface.get_nbr_collections(),
            "We assume we can't trigger a collection when allocating a parent-less frame"
        );

        *frame_ptr = Frame::from_method(init_method);
        Frame::init_frame_post_alloc(frame_ptr.clone(), args, max_stack_size, Gc::default());

        frame_ptr
    }

    pub(crate) fn init_frame_post_alloc(mut frame: Gc<Frame>, args: &[Value], stack_size: usize, prev_frame: Gc<Frame>) {
        unsafe {
            frame.stack_ptr = 0;

            frame.max_stack_size = stack_size as u8;
            frame.nbr_args = args.len() as u8;

            // initializing arguments from the args slice
            let args_ptr = frame.as_ptr().byte_add(OFFSET_TO_STACK + stack_size * size_of::<Value>()) as *mut Value;
            std::slice::from_raw_parts_mut(args_ptr, args.len()).copy_from_slice(args);

            // setting all locals to NIL.
            let locals_ptr = frame.as_ptr().byte_add(OFFSET_TO_STACK + (stack_size + args.len()) * size_of::<Value>()) as *mut Value;
            for idx in 0..frame.get_nbr_locals() {
                *locals_ptr.add(idx as usize) = Value::NIL;
            }

            frame.prev_frame = prev_frame; // because GC can have moved the previous frame!
        }
    }

    // Creates a frame from a block. Meant to only be called by the alloc_from_block function
    fn from_block(block: Gc<Block>) -> Self {
        Self {
            prev_frame: Gc::default(),
            current_context: block.blk_info.clone(),
            bytecode_idx: 0,
            stack_ptr: 0,
            nbr_args: 0,
            max_stack_size: 0,
            args_marker: PhantomData,
            locals_marker: PhantomData,
            stack_marker: PhantomData,
        }
    }

    // Creates a frame from a block. Meant to only be called by the alloc_from_method function
    pub(crate) fn from_method(method: Gc<Method>) -> Self {
        Self {
            prev_frame: Gc::default(),
            current_context: method,
            bytecode_idx: 0,
            stack_ptr: 0,
            nbr_args: 0,
            max_stack_size: 0,
            args_marker: PhantomData,
            locals_marker: PhantomData,
            stack_marker: PhantomData,
        }
    }

    /// Returns the true size of the `Frame`, counting the extra memory needed for its stack/locals/arguments.
    pub fn get_true_size(max_stack_size: usize, nbr_args: u8, nbr_locals: u8) -> usize {
        size_of::<Frame>() + ((max_stack_size + nbr_args as usize + nbr_locals as usize) * size_of::<Value>())
    }

    #[inline(always)]
    pub fn get_bytecode_ptr(&self) -> *const Vec<Bytecode> {
        &self.current_context.get_env().body
    }

    #[inline(always)]
    pub fn get_max_stack_size(&self) -> usize {
        self.max_stack_size as usize
    }

    /// # Safety
    /// So long as idx is a bytecode_idx, it's valid, since there's as many entries as there are bytecode. Otherwise, it could break.
    #[inline(always)]
    pub unsafe fn get_inline_cache_entry(&mut self, idx: usize) -> &mut Option<CacheEntry> {
        match self.current_context.deref_mut() {
            Method::Defined(env) => env.inline_cache.get_unchecked_mut(idx),
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    pub fn get_nbr_args(&self) -> u8 {
        self.current_context.get_env().nbr_params + 1
    }

    #[inline(always)]
    pub fn get_nbr_locals(&self) -> u8 {
        self.current_context.get_env().nbr_locals
    }

    /// Get the self value for this frame.
    pub(crate) fn get_self(&self) -> Value {
        let self_arg = self.lookup_argument(0);
        match self_arg.as_block() {
            Some(b) => {
                let block_frame = b.frame.as_ref().unwrap();
                block_frame.get_self()
            }
            None => *self_arg,
        }
    }

    /// Get the holder for this current method.
    pub(crate) fn get_method_holder(&self) -> Gc<Class> {
        // TODO: just self.current_context.holder instead? most likely.
        match self.lookup_argument(0).as_block() {
            Some(b) => {
                let block_frame = b.frame.as_ref().unwrap();
                block_frame.get_method_holder()
            }
            None => self.current_context.holder().clone(),
        }
    }

    /// Search for a local binding.
    /// This function, and its friends, is kinda ugly. That's what you get for using self-referential pointers.
    /// NOTE: fetching locals like this instead of storing a pointer in the frame to locals and arguments respectively, is a very minor slowdown.
    /// But moving GC and self-referential pointers led to some ugly bugs..
    /// I think storing self-referential pointers into `UnsafeCell`s might be enough to fix things though - TODO.
    #[inline(always)]
    pub fn lookup_local(&self, idx: usize) -> &Value {
        unsafe {
            let value_heap_ptr = (self as *const Self).byte_add(OFFSET_TO_STACK) as *mut Value;
            let locals_ptr = value_heap_ptr.add(self.get_max_stack_size() + self.nbr_args as usize);
            &*locals_ptr.add(idx)
        }
    }

    /// Assign to a local binding.
    #[inline(always)]
    pub fn assign_local(&mut self, idx: usize, value: Value) {
        unsafe {
            let value_heap_ptr = (self as *const Self).byte_add(OFFSET_TO_STACK) as *mut Value;
            let locals_ptr = value_heap_ptr.add(self.get_max_stack_size() + self.nbr_args as usize);
            *locals_ptr.add(idx) = value
        }
    }

    #[inline(always)]
    pub fn lookup_argument(&self, idx: usize) -> &Value {
        unsafe {
            let args_ptr = (self as *const Self as usize + OFFSET_TO_STACK + self.get_max_stack_size() * size_of::<Value>()) as *mut Value;
            &*args_ptr.add(idx)
        }
    }

    /// Assign to an argument.
    #[inline(always)]
    pub fn assign_arg(&mut self, idx: usize, value: Value) {
        unsafe {
            let args_ptr = (self as *const Self as usize + OFFSET_TO_STACK + self.get_max_stack_size() * size_of::<Value>()) as *mut Value;
            *args_ptr.add(idx) = value
        }
    }

    #[inline(always)]
    pub fn lookup_constant(&self, idx: usize) -> &Literal {
        self.current_context.get_env().literals.get(idx).unwrap()
    }

    pub fn nth_frame_back(current_frame: &Gc<Frame>, n: u8) -> Gc<Frame> {
        if n == 0 {
            return current_frame.clone();
        }

        let mut target_frame: Gc<Frame> = match current_frame.lookup_argument(0).as_block() {
            Some(block) => block.frame.as_ref().unwrap().clone(),
            None => panic!(
                "attempting to access a non local var/arg from a method instead of a block: self wasn't blockself but {:?}.",
                current_frame.lookup_argument(0)
            ),
        };
        for _ in 1..n {
            target_frame = match &target_frame.lookup_argument(0).as_block() {
                Some(block) => {
                    block.frame.as_ref().unwrap().clone()
                }
                None => panic!("attempting to access a non local var/arg from a method instead of a block (but the original frame we were in was a block): self wasn't blockself but {:?}.", current_frame.lookup_argument(0))
            };
        }
        target_frame
    }

    /// nth_frame_back but through prev_frame ptr. TODO: clarify why different implems are needed
    pub fn nth_frame_back_through_frame_list(current_frame: &Gc<Frame>, n: u8) -> Gc<Frame> {
        debug_assert_ne!(n, 0);
        let mut target_frame = current_frame.clone();
        for _ in 1..n {
            target_frame = target_frame.prev_frame.clone();
            if target_frame.is_empty() {
                panic!("empty target frame");
            }
        }
        target_frame
    }

    /// Gets the nth element from the stack (not in reverse order - "3" yields the 3rd element from the bottom, not the top)
    /// # Safety
    /// The caller needs to ensure this is a valid stack value. That means not outside the stack's maximum size, and not pointing to an uninitialized value.
    #[inline(always)]
    pub unsafe fn nth_stack(&self, n: u8) -> &Value {
        let stack_ptr = self as *const Self as usize + OFFSET_TO_STACK;
        let val_ptr = stack_ptr + (n as usize * size_of::<Value>());
        &*(val_ptr as *const Value)
    }

    /// Gets the nth element from the stack mutably (not in reverse order - "3" yields the 3rd element from the bottom, not the top)
    /// # Safety
    /// The caller needs to ensure this is a valid stack value. That means not outside the stack's maximum size, and not pointing to an uninitialized value.
    #[inline(always)]
    pub unsafe fn nth_stack_mut(&mut self, n: u8) -> &mut Value {
        let stack_ptr = self as *mut Self as usize + OFFSET_TO_STACK;
        let val_ptr = stack_ptr + (n as usize * size_of::<Value>());
        &mut *(val_ptr as *mut Value)
    }

    #[inline(always)]
    pub fn stack_push(&mut self, value: Value) {
        debug_assert!(
            self.stack_ptr < self.current_context.get_env().max_stack_size,
            "stack_push failed in {:?}::>{:?} (hit max stack size of {}), {:?}",
            self.current_context.holder().name,
            self.current_context.signature(),
            self.current_context.get_env().max_stack_size,
            self
        );
        // debug_assert!(self.stack_ptr < self.current_context.get_env().max_stack_size);
        unsafe {
            *self.nth_stack_mut(self.stack_ptr) = value;
            self.stack_ptr += 1;
        }
    }

    #[inline(always)]
    pub fn stack_pop(&mut self) -> Value {
        debug_assert!(self.stack_ptr > 0);
        unsafe {
            self.stack_ptr -= 1;
            *self.nth_stack_mut(self.stack_ptr)
        }
    }

    #[inline(always)]
    pub fn stack_last(&self) -> &Value {
        debug_assert!(self.stack_ptr > 0);
        unsafe { self.nth_stack(self.stack_ptr - 1) }
    }

    #[inline(always)]
    pub fn stack_last_mut(&mut self) -> &mut Value {
        debug_assert!(self.stack_ptr > 0);
        unsafe { self.nth_stack_mut(self.stack_ptr - 1) }
    }

    #[inline(always)]
    pub fn stack_nth_back(&self, n: usize) -> &Value {
        debug_assert!(self.stack_ptr >= (n + 1) as u8);
        unsafe { self.nth_stack(self.stack_ptr - (n as u8 + 1)) }
    }

    // TODO: should not be a static ref.
    #[inline(always)]
    pub fn stack_n_last_elements(&self, n: usize) -> &'static [Value] {
        unsafe {
            let slice_ptr = self.nth_stack(self.stack_ptr - n as u8);
            std::slice::from_raw_parts(slice_ptr, n)
        }
    }

    #[inline(always)]
    pub fn remove_n_last_elements(&mut self, n: usize) {
        debug_assert!(self.stack_ptr + 1 > n as u8);
        self.stack_ptr -= n as u8
    }

    /// Gets the total number of elements on the stack. Only used for debugging.
    pub fn stack_len(&self) -> usize {
        self.stack_ptr as usize
    }
}

/// Iterate over the stack for a given frame.
/// It iterates like a stack in "reverse order", returning the last-added element to the stack first.
pub struct FrameStackIter<'a> {
    frame: &'a Frame,
    stack_idx: u8,
}

impl<'a> From<&'a Frame> for FrameStackIter<'a> {
    fn from(frame: &'a Frame) -> Self {
        Self {
            frame,
            stack_idx: frame.stack_ptr,
        }
    }
}

impl<'a> Iterator for FrameStackIter<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack_idx == 0 {
            return None;
        }

        self.stack_idx -= 1;
        let val = unsafe { self.frame.nth_stack(self.stack_idx) };
        Some(val)
    }
}

impl Debug for Frame {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn stack_printer(frame: &Frame) -> String {
            let stack_elems = FrameStackIter::from(frame).map(|val| format!("{:?}", val)).collect::<Vec<_>>();
            format!("[{}]", stack_elems.join(", "))
        }

        f.debug_struct("Frame")
            .field(
                "current method",
                &format!("{}::>{}", self.current_context.holder().name(), self.current_context.signature()),
            )
            .field("bc idx", &self.bytecode_idx)
            .field("args", {
                let args: Vec<String> = (0..self.get_nbr_args()).map(|idx| format!("{:?}", self.lookup_argument(idx as usize))).collect();
                &format!("[{}]", args.join(", "))
            })
            .field("locals", {
                let locals: Vec<String> = (0..self.get_nbr_locals()).map(|idx| format!("{:?}", self.lookup_local(idx as usize))).collect();
                &format!("[{}]", locals.join(", "))
            })
            .field("stack", &stack_printer(self))
            .finish()
    }
}
