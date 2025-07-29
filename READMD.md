# Completed chapters
- Chapter 4: Scanning
- Chapter 5: Representing Code
- Chapter 6: Parsing Expressions
- Chapter 7: Evaluating Expressions
- Chapter 8: Statements and State

# Description
This is zLox! A Zig-based implementation of
[Crafting Interpreter's](https://craftinginterpreters.com)
tree-walking Lox language. As such, there are a few concepts that just don't carry over from Java to
Zig, so I am going to take my own spin on them.

This is my first real project in Zig, so I am still learning a lot of things.

I try to stay mostly faithful to the book with naming, functions, etc., but I put my own spin on
some of the stuff like parsing.


## A note about managing memory
Lox is canonically a garbage collected language, but Zig is a manually managed language. I have not
and will not write a GC for the jLox translation, so how does it not leak memory?

### Aside
I am using the `std.heap.GeneralPurposeAllocator` which has leak detection, so I do know when there
is a leak. I have done minimal testing, but what I have done shows all good signs.

### Explanation
Through carefully managing the flow of ownership (a lot easier to do in Rust), I can know that
certain values get destroyed at specific points while others carry on. From there its just making
sure the appropriate structures get deallocated when they are no longer needed and we are left with
no leaks.

One major example are expression intermediate values. These are actually allocated through an arena
(if needed e.g., strings). The final result of the expression computation is then reallocated with
the program "global" allocator to be used elsewhere (or immediately discarded) and the arena is
cleared.


# List of differences to the book
**NOTE:** this is NOT an exhaustive list of things that are different!

- I am passing around a struct containing `stdout`, `stdin`, an `Allocator`, and an equivalent of
    the Java `Lox.hadError`.
- Keyword lexing iterates through a list of strings and compares them instead of allocating a whole
    hash map just for keyword detection.
- The AST is represented slightly differently, but as close as possible.
- Parsing uses a combination of recursive ascent (my favorite) and recursive descent to eliminate back tracking.
- Anything with the visitor pattern doesn't exist. We can just switch on a tagged union instead.
- The interpreter has to handle memory allocation/deallocation, so I make it explicit what happens
    there.
