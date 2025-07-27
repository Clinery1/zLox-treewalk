# Completed chapters
- Chapter 4: Scanning

# Description
This is zLox! A Zig-based implementation of
[Crafting Interpreter's](https://craftinginterpreters.com)
tree-walking Lox language. As such, there are a few concepts that just don't carry over from Java to
Zig, so I am going to take my own spin on them.

This is my first real project in Zig, so I am still learning a lot of things.

I try to stay mostly faithful to the book with naming, functions, etc., but I put my own spin on
some of the stuff like parsing.


# List of differences to the book
**NOTE:** this is NOT an exhaustive list of things that are different!

- I am passing around a struct containing `stdout`, `stdin`, an `Allocator`, and an equivalent of
    the Java `Lox.hadError`.
- Keyword lexing iterates through a list of strings and compares them instead of allocating a whole
    hash map just for keyword detection.
- The AST is represented slightly differently, but as close as possible.
- Parsing uses recursive ascent (my favorite) instead of the recursive descent of the book.
- Anything with the visitor pattern doesn't exist. We can just switch on a tagged union instead.
