# Freya
Freya is a lightweight dynamically-typed register-based virtual machine written entirely in Rust.
Register-based ISA was preferred to stack-based primarily due to the reduced instruction dispatches at the cost of increased instruction size (estimated 47% less intructions, for only 25% increase in code size). An 'assembler' whose syntax is loosely inspired by GAS-i386 is provided for quick generation of bytecode for the VM.

# Type System
All values/objects may be classified largely into two types - BaseType, and CompositeType.

There are six base types:
1. `Int`: A signed 64-bit integer (`i64`).
2. `Flt`: A 64-bit double precision floating point integer (`f64`).
3. `Chr`: A 32-bit unicode character.
4. `Alloc`: An owned allocation to an object on the Heap.
5. `ConstRef`: An immutable reference to an object.
6. `MutRef`: A mutable reference to an object.

The following are the composite types:
1. `Str`: A rust `String`
2. `Slice`: An immutable view into a parent list-like composite type.
3. `FRef`: A reference to either an extern declaration or a function declaration in a specified module.
4. `List`: A vector of base types.

# Memory Model
Registers can only store base types, and are allocated contiguous memory on function call as per the limit specified in the function declaration.
All composite types are heap allocated. Upon allocation, the register which 'holds' the composite type, contains an `Alloc` value, which is an owned reference to the object.

Values are 'dropped' or 'deallocated' as per the following:
- When a register is written to, any previous value is dropped.
- When a function returns, all associated registers except the one containing the return value (if any) will be released (i.e, their values will be dropped)
- When an `Alloc` is dropped, the corresponding composite type on Heap is dropped.

Composite types on the heap can be referred to by `ConstRef` and `MutRef` which are immutable and mutable references respectively, as per the following:
- There cannot be multiple mutable references to the same object.
- Immutable references and mutable reference of the same object cannot co-exist.

# Planned Features
- [ ] Partially applied functions
- [ ] Dictionaries or HashMaps
- [ ] Native support (FFI)
- [ ] Multithreading with message passing

# Dependencies
1. [clap](https://github.com/clap-rs/clap): Used for declarative generation of the command-line arguments parser
2. [memmap2](https://github.com/RazrFalcon/memmap2-rs): Cross-platform memory mapped I/O
3. [paste](https://github.com/dtolnay/paste): Provides macros for concatenating identifiers in declarative macros used to limit duplication of code.
