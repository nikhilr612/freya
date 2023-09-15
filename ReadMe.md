# Freya
Freya is a lightweight dynamically-typed register-based virtual machine written entirely in Rust.
Register-based ISA was preferred to stack-based primarily due to the reduced instruction dispatches at the cost of increased instruction size (estimated 47% less intructions, for only 25% increase in code size). An 'assembler' whose syntax is loosely inspired by GAS-i386 is provided for quick generation of bytecode for the VM.

# Type System
All values/objects may be classified largely into two types - BaseType, and CompositeType.

There are seven base types:
1. `Int`: A signed 64-bit integer (`i64`).
2. `Flt`: A 64-bit double precision floating point integer (`f64`).
3. `Chr`: A 32-bit unicode character.
4. `Alloc`: An owned allocation to an object on the Heap.
5. `ConstRef`: An immutable reference to an object.
6. `MutRef`: A mutable reference to an object.
7*. `OpaqueHandle`: An unsigned pointer-width integer intended for use by Native APIs to return opaque pointers / handles.

The following are the composite types:
1. `Str`: A rust `String`
2. `FRef`: A reference to either an extern declaration or a function declaration in a specified module.

# Dependencies
1. [clap](https://github.com/clap-rs/clap): Used for declarative generation of the command-line arguments parser
2. [memmap2](https://github.com/RazrFalcon/memmap2-rs): Cross-platform memory mapped I/O
3. [paste](https://github.com/dtolnay/paste): Provides macros for concatenating identifiers in declarative macros used to limit duplication of code.
