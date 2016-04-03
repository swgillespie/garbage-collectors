# The Art of Automatic Memory Management

This repository is a work-in-progress effort to document the behavior
and implementation of garbage collection. This repository will contain
real, functional garbage collectors that will be fully documented and
written with education in mind.

This is an educational project and is not intended to produce super-efficient
implementations of garbage collectors, although efficiency is certainly a goal
of some of the examples that will be here. 

This project consists of two parts. The first part is a compiler for a toy programming
language, called `shorty`, that provides all of the mechanisms required for precise garbage
collection by the runtime. This includes mechanisms for inserting read and write barriers, as
well as statepoint polls, although none of the existing GCs use those features yet.

The second part of this project is a number of educational garbage collectors based on
collectors described in [The Garbage Collection Handbook: The Art of Automatic Memory Management](http://www.amazon.com/The-Garbage-Collection-Handbook-Management/dp/1420082795). 
None of these collectors are implemented yet, although there does exist a runtime that does
not have a garbage collector at all and redirects calls to `shorty_gc_malloc` to `malloc`.

## About Shorty
Shorty is a simple language that allows for the writing of programs that can exercise the
garbage collector, and not a whole lot more. Shorty allows the author to define `structs`:
```
struct Node {
    left: Node,
    right: Node,
    data: int
}
```
Structs are always considered to be heap allocated and managed by the GC. Creating an instance
of a struct is done using the `new` expression:

```
let node = new Node;
```

This gets translated by the compiler into a call to `shorty_gc_malloc`, the entry point
of the allocator defined by each runtime.

Shorty allows for the definition of functions:

```
fn build_data(data: int) -> Node {
    let node = new Node;
    node.left = nil;
    node.right = nil;
    node.data = data;
    return node;
}
```

Functions can also be declared `extern`, in which case they can have no body and are resolved
by the linker:
```
extern fn shorty_print_int(data: int);
```

The Shorty compiler is spartan and provides very poor error messages, so I apologize if you run into problems :).
