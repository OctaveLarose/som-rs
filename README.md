This commit is me (olarose) adding source info to the parser.

The main motivation was enabling inlining, since it breaks shadowing at the moment
without each variable having an identity not just defined by their name but also their position in source.

It's something that should be done to have better error reporting, so really these changes
would be valuable in the main branch; but my changes are fairly pervasive so I'm shelving them for now, out of laziness.
Instead I think I'll make it so that the lexer only stores info for symbols, and in the enum for them directly (a tad uglier? but simpler)