use std::io::Write;
use std::io::BufWriter;
use std::fs::File;
use crate::core::{FatalErr, ErrorType, new_error};
pub(crate) struct SliceView<'a> {
	idx: usize,
	buf: &'a [u8]
}

impl SliceView<'_> {
	pub fn wrap(st: usize, buf: &[u8]) -> SliceView {
		SliceView {
			idx: st,
			buf
		}
	}

	pub fn take(&mut self, len: usize) -> &[u8] {
		let ret = &self.buf[self.idx..(self.idx+len)];
		self.idx += len;
		ret
	}

	pub fn decode_utf8(&mut self, len_bytes: usize) -> Result<String, FatalErr> {
		let sl = self.take(len_bytes);
		let retstr = std::str::from_utf8(sl);
		match retstr {
			Ok(s) => Ok(s.to_owned()),
			Err(e) => {
				Err(new_error(ErrorType::UtfDecodeError, format!("Failed to decode slice {sl:?}\ncause:\t{e:?}")))
			}
		}
	}

	pub fn get_u16(&mut self) -> u16 {
		let val = (self.buf[self.idx] as u16) << 8 | (self.buf[self.idx+1]) as u16;
		self.idx += 2;
		val
	}

	pub fn get_u64(&mut self) -> u64 {
		let val = (self.buf[self.idx] as u64) << 56 | (self.buf[self.idx+1] as u64) << 48 | (self.buf[self.idx+2] as u64) << 40 | (self.buf[self.idx+3] as u64) << 32 | (self.buf[self.idx+4] as u64) << 24 | (self.buf[self.idx+5] as u64) << 16 | (self.buf[self.idx+6] as u64) << 8 | (self.buf[self.idx+7] as u64);
		self.idx += 8;
		val
	}

	pub fn get_u32(&mut self) ->u32 {
		let val = (self.buf[self.idx] as u32) << 24 | (self.buf[self.idx+1] as u32) << 16 | (self.buf[self.idx+2] as u32) << 8 | (self.buf[self.idx+3] as u32); 
		self.idx += 4;
		val
	}

	pub fn get_u8(&mut self) -> u8 {
		let ret = self.buf[self.idx];
		self.idx += 1;
		ret
	}

	pub fn offset(&self) -> usize {
		self.idx
	}

	pub fn bytes_left(&self) -> usize {
		self.buf.len() - self.idx
	}
}

pub type IOResult = Result<(), std::io::Error>;

pub trait OutBuf {
	fn write_u8(&mut self, v: u8) -> IOResult;
	fn write_u16(&mut self, v: u16) -> IOResult;
	fn write_u32(&mut self, v: u32) -> IOResult;
	fn write_u64(&mut self, v: u64) -> IOResult;
	fn write_str(&mut self, v: &str) -> IOResult;
}

pub trait AsBytes {
	fn write(&self, buf: &mut impl OutBuf) -> IOResult;
}

impl OutBuf for BufWriter<File> {
	fn write_u8(&mut self, v: u8) -> IOResult {
		let buf = vec![v; 1];
		self.write_all(&buf)
	}

	fn write_u16(&mut self, v: u16) -> IOResult {
		let mut buf = vec![0; 2];
		buf[0] = ((v & 0xff00) >> 8) as u8;
		buf[1] = (v & 0x00ff) as u8;
		self.write_all(&buf)
	}

	fn write_u32(&mut self, v: u32) -> IOResult {
		let mut buf = vec![0; 4];
		buf[3] = (v & 0xff) as u8;
		buf[2] = ((v >> 8) & 0xff) as u8;
		buf[1] = ((v >> 16) & 0xff) as u8;
		buf[0] = ((v >> 24) & 0xff) as u8;
		self.write_all(&buf)	
	}

	fn write_u64(&mut self, v: u64) -> IOResult {
		let mut buf = vec![0; 8];
		buf[7] = (v & 0xff) as u8;
		buf[6] = ((v >> 8) & 0xff) as u8;
		buf[5] = ((v >> 16) & 0xff) as u8;
		buf[4] = ((v >> 24) & 0xff) as u8;
		buf[3] = ((v >> 32) & 0xff) as u8;
		buf[2] = ((v >> 40) & 0xff) as u8;
		buf[1] = ((v >> 48) & 0xff) as u8;
		buf[0] = ((v >> 56) & 0xff) as u8;
		self.write_all(&buf)
	}

	fn write_str(&mut self, v: &str) -> IOResult {
		self.write_all(v.as_bytes())
	}
}

#[derive(Debug)]
pub struct BitSet {
	/// A vector of length sufficient to contain atleast `len` bits.
	data: Box<[u16]>
}

impl BitSet {
	pub fn new(len: usize) -> BitSet {
		let size = (len + 15) >> 4;
		let data = vec![0; size];
		BitSet {
			data: data.into()
		}
	}

	pub(crate) fn set_unchecked(&mut self, idx: usize, value: bool) {
		let bidx = idx >> 4;
		let mask = (1 << (idx & 15)) as u16;
		if value {
			self.data[bidx] |= mask;
		} else {
			self.data[bidx] &= !mask;
		}
	}

	pub(crate) fn get_unchecked(&self, idx: usize) -> bool {
		let bidx = idx >> 4;
		let mask = (1 << (idx & 15)) as u16;
		(self.data[bidx] & mask) != 0
	}

	pub fn capacity(&self) -> usize {
		self.data.len() * 16
	}
}

impl core::fmt::Display for BitSet {
	fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		writeln!(fmt, "bits{{")?;
		for i in 0..self.data.len() {
			writeln!(fmt, "{:016b}", self.data[i])?;
		}
		write!(fmt, "}}")
	}
}