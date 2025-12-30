use crate::compiler::Literal;
use crate::gc::{visit_value, GcIdentifier};
use crate::value::Value;
use crate::vm_objects::block::{Block, CacheEntry};
use crate::vm_objects::class::Class;
use crate::vm_objects::method::Method;
use core::mem::size_of;
use som_core::bytecode::Bytecode;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, GcType, SOMAllocator};
use som_gc::gcref::Gc;
use som_gc::slot::SOMSlot;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

use super::method::MethodInfo;

pub(crate) const OFFSET_TO_VALUES: usize = size_of::<Frame>();

// TODO: fix hacky conversions from u8 to usize and vice-versa, settle on a more uniform scheme (i.e. only usizes..)

/// Represents a stack frame.
pub struct Frame {
    /// The previous frame. Frames are handled as a linked list
    pub prev_frame: Gc<Frame>,

    /// The method the execution context currently is in.
    /// So why do we do things that way? Because of moving GC. If we have a pointer to a MethodInfo, that's an inner pointer to a Method object. So when GC moves the frame, it can't update that pointer.
    pub current_context: Gc<MethodInfo>,

    /// Bytecode index, needed to know where to resume execution when returning to parent frames.
    pub bytecode_idx: u16,

    /// markers. we don't use them directly. it's mostly a reminder that the struct looks different in memory... not the cleanest but not sure how else to go about it
    pub args_marker: PhantomData<[Value]>,
    pub locals_marker: PhantomData<[Value]>,
}

impl Frame {
    /// Allocates the very first frame, for the `initialize:` call and tests.
    /// Special-cased because the normal case pushes the previous value on the previous frame's
    /// stack for it to be reachable: we have no previous frame in some cases, so we can't.
    /// TODO: if stack isn't in each frame, then change this, right?
    pub fn alloc_initial_method(init_method: Gc<Method>, args: &[Value], gc_interface: &mut GCInterface) -> Gc<Frame> {
        let size = {
            let nbr_locals = match &*init_method {
                Method::Defined(m_env) => m_env.nbr_locals,
                _ => unreachable!("if we're allocating a method frame, it has to be defined."),
            };
            Frame::get_true_size(args.len() as u8, nbr_locals)
        };

        let nbr_gc_runs = gc_interface.get_nbr_collections();

        let mut frame_ptr: Gc<Frame> = gc_interface.request_memory_for_type(size, AllocSiteMarker::InitMethodFrame);

        assert_eq!(
            nbr_gc_runs,
            gc_interface.get_nbr_collections(),
            "We assume we can't trigger a collection when allocating a parent-less frame"
        );

        *frame_ptr = Frame::from_method(init_method.get_env(), Gc::default());
        Frame::init_frame_args_locals(&mut frame_ptr, args);

        frame_ptr
    }

    /// Initializes a frame with all its expected values for its arguments and locals.
    /// Takes a slice of arguments to be copied in the frame.
    pub(crate) fn init_frame_args_locals(frame: &mut Gc<Frame>, args: &[Value]) {
        unsafe {
            // initializing arguments from the args slice
            let args_ptr = frame.as_ptr().byte_add(OFFSET_TO_VALUES) as *mut Value;
            std::slice::from_raw_parts_mut(args_ptr, args.len()).copy_from_slice(args);

            // setting all locals to NIL.
            let locals_ptr = args_ptr.byte_add(std::mem::size_of_val(args));
            for idx in 0..frame.get_nbr_locals() {
                *locals_ptr.add(idx as usize) = Value::NIL;
            }
        }
    }

    /// Initializes a frame with all its expected values for its arguments and locals.
    /// Takes a reference to the global stack to invoke `drain` to efficiently remove and copy its last `nbr_args` values.
    pub(crate) fn init_frame_args_locals_from_stack(frame: &mut Gc<Frame>, stack: &mut Vec<Value>, nbr_args: usize) {
        unsafe {
            let args = stack.drain(stack.len() - nbr_args..);
            let args_ptr = frame.as_ptr().byte_add(OFFSET_TO_VALUES) as *mut Value;
            std::slice::from_raw_parts_mut(args_ptr, nbr_args).copy_from_slice(args.as_slice());

            // setting all locals to NIL.
            let locals_ptr = args_ptr.byte_add(size_of::<Value>() * nbr_args);
            for idx in 0..frame.get_nbr_locals() {
                *locals_ptr.add(idx as usize) = Value::NIL;
            }
        }
    }

    // Creates a frame from a block. Meant to only be called by the alloc_from_block function
    pub(crate) fn from_block(block: Gc<Block>, prev_frame: Gc<Frame>) -> Self {
        Self {
            prev_frame,
            current_context: block.blk_info.clone(),
            bytecode_idx: 0,
            args_marker: PhantomData,
            locals_marker: PhantomData,
        }
    }

    // Creates a frame from a method. Called from methods that allocate different method frames
    pub(crate) fn from_method(method: Gc<MethodInfo>, prev_frame: Gc<Frame>) -> Self {
        Self {
            prev_frame,
            current_context: method,
            bytecode_idx: 0,
            args_marker: PhantomData,
            locals_marker: PhantomData,
        }
    }

    /// Returns the true size of the `Frame`, counting the extra memory needed for its locals/arguments.
    pub fn get_true_size(nbr_args: u8, nbr_locals: u8) -> usize {
        size_of::<Frame>() + ((nbr_args as usize + nbr_locals as usize) * size_of::<Value>())
    }

    #[inline(always)]
    pub fn get_bytecodes(&self) -> &Vec<Bytecode> {
        &self.current_context.body
    }

    /// # Safety
    /// So long as idx is a bytecode_idx, it's valid, since there's as many entries as there are bytecode. Otherwise, it could break.
    #[inline(always)]
    pub unsafe fn get_inline_cache_entry(&mut self, idx: usize) -> &mut Option<CacheEntry> {
        self.current_context.inline_cache.get_unchecked_mut(idx)
    }

    #[inline(always)]
    pub fn get_nbr_args(&self) -> u8 {
        self.current_context.nbr_args
    }

    #[inline(always)]
    pub fn get_nbr_locals(&self) -> u8 {
        self.current_context.nbr_locals
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
        self.current_context.base_method_info.holder.clone()
        // old logic below - not sure why that was ever needed?
        //match self.lookup_argument(0).as_block() {
        //    Some(b) => {
        //        let block_frame = b.frame.as_ref().unwrap();
        //        block_frame.get_method_holder()
        //    }
        //    None => self.current_context.holder().clone(),
        //}
    }

    /// Search for a local binding.
    /// This function, and its friends, is kinda ugly. That's what you get for using self-referential pointers.
    #[inline(always)]
    pub fn lookup_local(&self, idx: usize) -> &Value {
        unsafe {
            let value_heap_ptr = (self as *const Self).byte_add(OFFSET_TO_VALUES) as *mut Value;
            let locals_ptr = value_heap_ptr.add(self.current_context.nbr_args as usize);
            &*locals_ptr.add(idx)
        }
    }

    /// Assign to a local binding.
    #[inline(always)]
    pub fn assign_local(&mut self, idx: usize, value: Value) {
        unsafe {
            let value_heap_ptr = (self as *const Self).byte_add(OFFSET_TO_VALUES) as *mut Value;
            let locals_ptr = value_heap_ptr.add(self.current_context.nbr_args as usize);
            *locals_ptr.add(idx) = value
        }
    }

    #[inline(always)]
    pub fn lookup_argument(&self, idx: usize) -> &Value {
        unsafe {
            let args_ptr = (self as *const Self as usize + OFFSET_TO_VALUES) as *mut Value;
            &*args_ptr.add(idx)
        }
    }

    /// Assign to an argument.
    #[inline(always)]
    pub fn assign_arg(&mut self, idx: usize, value: Value) {
        unsafe {
            let args_ptr = (self as *const Self as usize + OFFSET_TO_VALUES) as *mut Value;
            *args_ptr.add(idx) = value
        }
    }

    #[inline(always)]
    pub fn lookup_constant(&self, idx: usize) -> &Literal {
        self.current_context.literals.get(idx).unwrap()
    }

    /// Returns the nth frame back in the frame list, given n and the current frame.
    /// Walks the frame list not using the back pointer `prev_frame`, but by looking up the previous frame associated with each block.
    pub fn nth_frame_back(current_frame: &Gc<Frame>, n: u8) -> Gc<Frame> {
        if n == 0 {
            return current_frame.clone();
        }

        let mut target_frame: Gc<Frame> = current_frame.clone();
        for _ in 0..n {
            target_frame = match &target_frame.lookup_argument(0).as_block() {
                Some(block) => block.frame.as_ref().unwrap().clone(),
                None => panic!(
                    "attempting to access a non local var/arg from a method instead of a block: self wasn't blockself but {:?}.",
                    current_frame.lookup_argument(0)
                ),
            };
        }
        target_frame
    }
}

impl Debug for Frame {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field(
                "current method",
                &format!(
                    "{}::>{}",
                    self.current_context.base_method_info.holder.name, self.current_context.base_method_info.signature
                ),
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
            .finish()
    }
}

impl GcType for Frame {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::Frame as u8
    }

    fn scan_object(frame: Gc<Self>, visit_fn: &mut dyn FnMut(SOMSlot)) {
        if !frame.prev_frame.is_empty() {
            visit_fn(SOMSlot::from(&frame.prev_frame));
        }

        visit_fn(SOMSlot::from(&frame.current_context));

        for i in 0..frame.get_nbr_locals() {
            let val: &Value = frame.lookup_local(i as usize);
            visit_value(val, visit_fn)
        }

        for i in 0..frame.get_nbr_args() {
            let val: &Value = frame.lookup_argument(i as usize);
            visit_value(val, visit_fn)
        }
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        Frame::get_true_size(_self.get_nbr_args(), _self.get_nbr_locals())
    }
}
