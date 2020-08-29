# Prosper
Prosper is a programming language that eventually will aim
to be a decent starting point as a system programming language.
It focuses on simplicity while still offering modern features and tooling.
The language is heavily Rust and C inspired.

## Usage
```sh
$ clang -c ../prstd/std.c
$ cargo run -p prosperc -- example.pr
$ clang `llvm-config --ldflags --libs core` -o example example.o std.o
$ ./example
```

## What doesn't work?
### In order of priority:
- Returning from nested blocks
- Most binary expressions
- Breaking from nested blocks
- Branching
- Strings
- Casting primitives
- Floats
- Arrays
- Tuples
- Custom types
- Name mangling

## What does?
- Ints (kind of)
- Floats (barely)
- Booleans
- Nested blocks
- Importing foreign functions (needs refinement)
- Defining functions
- Local variables
- Single expression functions
- Some Binary expressions
- Unary expressions (almost)
- Comments (single- and multi-line)

## What needs to be done?
### In order of priority:
- Fix everything that's not working
- Compiler attributes (like `no_mangle`)
- Modules
- Proper standard library
- Better tooling
- Examples and documentation
- Better FFI support
- Rewrite of everything