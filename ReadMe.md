# Freya
Freya is a lightweight dynamically-typed register-based virtual machine written entirely in Rust.
Register-based ISA was preferred to stack-based primarily due to the reduced instruction dispatches at the cost of increased instruction size (estimated 47% less intructions, for only 25% increase in code size). An 'assembler' whose syntax is loosely inspired by GAS-i386 is provided for quick generation of bytecode for the VM.

# Dependencies
1. [clap](https://github.com/clap-rs/clap): Used for declarative generation of the command-line arguments parser
2. [memmap2](https://github.com/RazrFalcon/memmap2-rs): Cross-platform memory mapped I/O
3. [paste](https://github.com/dtolnay/paste): Provides macros for concatenating identifiers in declarative macros used to limit duplication of code.
