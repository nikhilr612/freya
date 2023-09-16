/// Module to contain all opcode and instruction definitions.
///
/// 1. Function-call related instructions: 0x1_
/// 2. Register load, move, copy related instructions: 0x2_
/// 3. Miscellaneous procedures: 0xf_
/// 4. Arithmetic instructions: 0x3_
/// 5. Relational operators, conditional branching related instructions: 0x4_
/// 6. Debug related instructions: 0x5_
/// 7. Index-related instructions: 0x6_

use crate::utils::AsBytes;
use crate::utils::OutBuf;
use crate::types::FatalErr;
use crate::types::ErrorType;
use crate::utils::SliceView;
use paste::paste;

/// Set `r3 <- r1 + r2`
pub const ADD: u8 = 0x30;
/// Set `r3 <- r1 - r2`
pub const SUB: u8 = 0x31;
/// Set `r3 <- r1 * r2`
pub const MUL: u8 = 0x32;
/// Set `r3 <- r1 / r2` 
/// Note: r3 always has type `Flt`
pub const DIV: u8 = 0x33;
/// Set `r1 <- r1 + (i16)`
pub const IINC: u8 = 0x34;
/// Set `r1 <- r1 + 1`
pub const INC1: u8 = 0x35;
/// Integer division. `r3 <- r1 // r2`
/// Note: r3 always has type `Int`
pub const IDIV: u8 = 0x36;
/// Get remainder of division. `r3 <- r1 % r2`
pub const REM: u8 = 0x37;
/// Bit-wise NOT `r2 <- ~r1`
pub const BNOT: u8 = 0x38;
/// Bit-wise AND `r3 <- r1 & r2`
pub const BAND: u8 = 0x39;
/// Bit-wise OR `r3 <- r1 | r2`
pub const BOR: u8 = 0x3a;
/// Bit-wise exclusive or
/// `r3 <- r1 ^ r2`
pub const XOR: u8 = 0x3b;
/// Left-shift r
pub const LSHIFT: u8 = 0x3c;
/// Right-shift
pub const RSHIFT: u8 = 0x3d;
/// Exponentiation r3 <- r1 ** r2
/// Note: r3 always has type `Flt`
pub const EXP: u8 = 0x3e;
/// Get the greatest integer less than or equal to the argument.
pub const FLOOR: u8 = 0x3f;
/// Return from current function. Return value in register r1.
pub const RET: u8 = 0x10;
/// Return 
pub const VRET: u8 = 0x1f;
/// Load from Constant Pool
/// r1 <- [u16]
pub const LDC: u8 = 0x20;
/// Move value from one register to another
/// r2 <- r1
/// If value in r1, is not copy, then r1 <- None.
/// If r1 is None, then this is equivalent to dropping r2.
pub const MOV: u8 = 0x21;
/// Swap values in register
/// t <- r1, r1 <- r2, r2 <- t
pub const SWAP: u8 = 0x22;
/// Drop value in register. If the value is an Alloc, the underlying object will be deallocated.`
pub const DROP: u8 = 0x23;
/// Borrow composite type immutably.
/// r2 <- &[r1]
pub const BWI: u8 = 0x24;
/// Borow composite type mutably.
/// r2 <- &mut[r1]
pub const BWM: u8 = 0x25;
/// Load an integer into a register
/// r1 <- (i8)
pub const LDI: u8 = 0x26;

/// If r1 < r2, then r3 <- 1 else r3 <- 0
pub const RLT: u8 = 0x41;
/// If r1 <= r2, then r3 <- 1 else r3 <- 0
pub const RLE: u8 = 0x42;
/// If r1 > r2, then r3 <- 1 else r3 <- 0
pub const RGT: u8 = 0x43;
/// If r1 >= r2, then r3 <- 1 else r3 <- 0
pub const RGE: u8 = 0x44;
/// If r1 != r2, then r3 <- 1 else r3 <- 0
pub const RNEQ: u8 = 0x45;
/// If r1 == r2, then r3 <- 1 else r3 <- 0
pub const REQ: u8 = 0x40;
/// If r1 evaluates to false as a boolean, i.e, it is `'\0'`, `0`, or `0.0`, then r2 is 0 else r2 is 1
pub const ISN0: u8 = 0x47;
/// r1 <- !r1
pub const LNOT: u8 = 0x48;
/// ip <- addr if NOT r1
pub const BRANCH: u8 = 0x49;
/// ip <- addr
pub const JUMP: u8 = 0x4a;

// fstcall r1, r2, r3
// r1 is the register with callable.
// (r2-1) is the register to store return value. If r2 is zero, return value is not stored.
// r3 is the register starting from which the arguments are read, i.e, if function requires n parameters then the registers r3, r3+1, r3+2, ..., r3+(n-1) are read.
// pub const FSTCALL: u8 = 0x11;

/// No-op instruction. Does nothing.
pub const NOP: u8 = 0x50;
/// No-op instruction, used for debugging . Sets the debug line number in the current stack frame.
pub const DBGLN: u8 = 0x51;
/// if !r then raise error, otherwise no-op
pub const ASSERT: u8 = 0x52;

/// r3 <- r1[r2]
pub const GETINDEX: u8   = 0x60;
/// r1[r2] <- r3
pub const PUTINDEX: u8 = 0x62;
/// push r1, r2
/// r1.append(r2)
/// Append a value to the end of list-like item.
pub const PUSH: u8 = 0x63;
/// Remove the last value of the list and store in register.
/// r2 <- r1.pop()
pub const POP: u8 = 0x64;
/// r2 <- r1.len()
pub const LENGTH: u8 = 0x65;
/// For list-like objects, create a view for sub-list.
/// r1 <- r2[r3..r4]
pub const SLICE: u8 = 0x66;

/// stdcall r1, r2
pub const STDCALL: u8 = 0x12;
/// r1 <- [i16] Obtain FRef Alloc for function within the module.
pub const LDF: u8 = 0x13;
/// r1 <- [i16] Obtain FRef Alloc for extern defined in the module.
/// Throws UnresolvedExtern if the extern declaration could not be resolved successfully.
pub const LDX: u8 = 0x14;

/// Print value stored in register with newline.
pub const PRINT: u8 = 0xf0;
/// Put current thread to sleep for r1 seconds.
pub const SLEEP: u8 = 0xf1;
/// getline %(prompt) %r
/// r <- (line)
/// Read characters from standard input until a newline ('\n') is encountered and store in a register.
pub const GETLN: u8 = 0xf2;
/// Parse a string for an integer and store in a register.
pub const PARSEINT: u8 = 0xf3;
/// Parse a string for a floating point value and store in a register.
pub const PARSEFLT: u8 = 0xf4;

macro_rules! def_instr {
	($name: ident { $($field_name: ident : $field_type:ty),* } [$size: literal]) => {
		pub(crate) struct $name {
			$(pub(crate) $field_name: $field_type),*
		}

		impl TryFrom<&mut SliceView<'_>> for $name {
			type Error = FatalErr;

			fn try_from(sl: &mut SliceView) -> Result<Self, Self::Error> {
				let bl = sl.bytes_left();
				if bl < $size {
					return Err(crate::types::new_error(ErrorType::IncompleteInstr, format!("Expecting {} bytes, but found {} at offset {}", $size, bl, sl.offset())));
				}
				Ok(paste! {
					$name {
						$($field_name: sl.[<get_ $field_type>]()),*
					}
				})
			}
		}

		impl AsBytes for $name {
			fn write(&self, buf: &mut impl OutBuf) -> Result<(),std::io::Error> {
				paste! {
					$(buf.[<write_ $field_type>](self.$field_name)?);*
				};
				Ok(())
			}
		}
	};
}

pub(crate) struct VariadicRegst {
	pub regs: Vec<u8>
}

impl TryFrom<&mut SliceView<'_>> for VariadicRegst {
	type Error = FatalErr;
	fn try_from(sl: &mut SliceView) -> Result<Self,Self::Error> {
		if sl.bytes_left() < 1 {
			return Err(crate::types::new_error(ErrorType::IncompleteInstr, format!("Expecting size byte for variadic instruction, offset: {}", sl.offset())));
		}
		let size = sl.get_u8() as usize;
		let mut rvec = vec![0; size];
		if sl.bytes_left() < size {
			return Err(crate::types::new_error(ErrorType::IncompleteInstr, format!("Expecting {size} bytes for variadic instruction, found {} at offset {}", sl.bytes_left(), sl.offset())));
		}
		for i in 0..size {
			rvec[i] = sl.get_u8();
		}
		Ok(VariadicRegst {regs: rvec})
	}
}

impl AsBytes for VariadicRegst {
	fn write(&self, buf: &mut impl OutBuf) -> Result<(), std::io::Error> {
		buf.write_u8(self.regs.len() as u8)?;
		for i in 0..self.regs.len() {
			buf.write_u8(self.regs[i])?;
		}
		Ok(())
	}
}

// Instruction type specifying three registers.
// Used primarily for arithmetic operations.
def_instr! {
	TripleRegst {
		r1: u8,
		r2: u8,
		r3: u8
	} [3]
}

def_instr! {
	DoubleRegst {
		r1: u8,
		r2: u8
	} [2]
}

def_instr! {
	Id16Reg {
		id: u16,
		r1: u8
	} [3]
}

def_instr! {
	RegAddr {
		r1: u8,
		addr: u32
	} [5]
}

def_instr! {
	QuadrupleRegst {
		r1: u8,
		r2: u8,
		s1: u8,
		s2: u8
	} [4]
}

pub fn triplet_mnemonic_map(head: &str) -> Option<u8> {
	match head{
		"add" => Some(ADD),
		"sub" => Some(SUB),
		"div" => Some(DIV),
		"mul" => Some(MUL),
		"idv" => Some(IDIV),
		"rem" => Some(REM),
		"bt&" => Some(BAND),
		"bt|" => Some(BOR),
		"xor" => Some(XOR),
		"exp" => Some(EXP),
		"rlt" => Some(RLT),
		"rle" => Some(RLE),
		"rgt" => Some(RGT),
		"rge" => Some(RGE),
		"req" => Some(REQ),
		"rne" => Some(RNEQ),
		"bls" => Some(LSHIFT),
		"brs" => Some(RSHIFT),
		"get" => Some(GETINDEX),
		_ => None
	}
}

pub fn doublet_mnemonic_map(head: &str) -> Option<u8> {
	match head {
		"swap" => Some(SWAP),
		"&mut" => Some(BWM),
		"&imt" => Some(BWI),
		"floor" =>Some(FLOOR),
		"bnot" => Some(BNOT),
		"move" => Some(MOV),
		"isn0" => Some(ISN0),
		"assert"=>Some(ASSERT),
		"gline" =>Some(GETLN),
		"s2int" => Some(PARSEINT),
		"s2flt" => Some(PARSEFLT),
		"push" => Some(PUSH),
		"pop"  => Some(POP),
		"len"=>Some(LENGTH),
		_ => None
	}
}

pub fn singlet_mnemonic_map(head: &str) -> Option<u8> {
	match head {
		"print" => Some(PRINT),
		"sleep" => Some(SLEEP),
		"incr1" => Some(INC1),
		"lnot"  => Some(LNOT),
		"drop"  => Some(DROP),
		_ => None
	}
}