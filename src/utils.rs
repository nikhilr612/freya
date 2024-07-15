use std::io::{Write, Read};
use std::io::Result as IoResult;
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

pub trait OutBuf {
	fn write_u8(&mut self, v: u8) -> IoResult<()>;
	fn write_u16(&mut self, v: u16) -> IoResult<()>;
	fn write_u32(&mut self, v: u32) -> IoResult<()>;
	fn write_u64(&mut self, v: u64) -> IoResult<()>;
	fn write_str(&mut self, v: &str) -> IoResult<()>;
}

pub trait AsBytes {
	fn write(&self, buf: &mut impl OutBuf) -> IoResult<()>;
}

impl<T: Write> OutBuf for T {
	fn write_u8(&mut self, v: u8) -> IoResult<()> {
		let buf = vec![v; 1];
		self.write_all(&buf)
	}

	fn write_u16(&mut self, v: u16) -> IoResult<()> {
		let mut buf = vec![0; 2];
		buf[0] = ((v & 0xff00) >> 8) as u8;
		buf[1] = (v & 0x00ff) as u8;
		self.write_all(&buf)
	}

	fn write_u32(&mut self, v: u32) -> IoResult<()> {
		let mut buf = vec![0; 4];
		buf[3] = (v & 0xff) as u8;
		buf[2] = ((v >> 8) & 0xff) as u8;
		buf[1] = ((v >> 16) & 0xff) as u8;
		buf[0] = ((v >> 24) & 0xff) as u8;
		self.write_all(&buf)	
	}

	fn write_u64(&mut self, v: u64) -> IoResult<()> {
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

	/// This method **does not** write string length; only the content as bytes.
	fn write_str(&mut self, v: &str) -> IoResult<()> {
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

const BYTEBUF_SIZE: usize = 2048;

/// Buffered Character streaming for `Read` streams.
pub struct CharStream<'a, T: Read> {
	buffer: [u8; BYTEBUF_SIZE],
	/// The offset to start writing into the byte buffer from.
	bstart: usize,
	/// The buffer to store chars.
	chars: String,
	/// The byte offset from which the next character in string can be read.
	offset: usize,
	/// The number of bytes read.
	position: usize,
	/// The number of newline characters read.
	nlines: usize,
	reader: &'a mut T
}

impl<'a, T: Read> From<&'a mut T> for CharStream<'a, T> {
	fn from(a: &'a mut T) -> Self {
		CharStream { 
			chars: String::new(), 
			offset: 0, position: 0,
			bstart: 0,
			reader: a,
			nlines: 0,
			buffer: [0; BYTEBUF_SIZE]
		}
	}
}

impl<T: Read> CharStream<'_, T> {
	/// Read next character, if possible. Returns Ok(None) when no characters are left.
	/// As with `Read`, once None is returned, subsequent calls may be __non-null__, once the stream has taken more data.
	/// Returns Err if stream contains invalid utf-8 data.
	pub fn next_char(&mut self) -> IoResult<Option<char>> {
		if self.offset == self.chars.len() {
			// All contents of buffer have been read.
			if !self.fill_buffer()? {
				// Can't refil, so we're done.
				return Ok(None);
			}
		}

		let r = self.chars[self.offset..].chars().next().inspect(|ch| {
			self.offset += ch.len_utf8();
			if *ch == '\n' {
				self.nlines += 1;
			}
		});

		Ok(r)
	}

	// Return true if any chars were translated, false otherwise.
	fn fill_buffer(&mut self) -> IoResult<bool>{
		let nread = self.reader.read(&mut self.buffer[self.bstart..])?;
		self.position += nread;
		
		let blen = nread + self.bstart;

		self.chars.clear();
		self.offset = 0;

		// No characters left.. stream has most likely reached the end.
		if blen == 0 {
			return Ok(false);
		}

		let (s, v) = match std::str::from_utf8(&self.buffer[..blen]) {
			Ok(s) => (s, blen),
			Err(e) => {
				if e.valid_up_to() == 0 {
					// Unlikely that nonthing in the buffer could be translated, unless the buffer was empty.
					// But that's handled.
					return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid UTF-8 data."));
				} else {
					// (blen - valid_up_to) bytes were not translated, so don't discard them.
					// If nothing was translated, then .. we have an error.
					(unsafe { std::str::from_utf8_unchecked(&self.buffer[..e.valid_up_to()]) }, e.valid_up_to())
				}
			}
		};

		self.chars.push_str(s);
		self.buffer.copy_within(v.., 0);
		self.bstart = blen - v;

		Ok(true)
	}

	/// The position since start of stream, of the current character.
	pub fn byte_position(&self) -> usize {
		self.position - self.chars.len() + self.offset
	}

	/// Get the number of newline characters read from this stream + 1. 
	pub fn lineno(&self) -> usize {
		self.nlines + 1
	}

	/// Read and discard characters from the stream that are whitespaces.
	/// Returns `None` if stream has reached eof.
	pub fn skip_whitespace(&mut self) -> IoResult<Option<char>> {
		loop {
			let ch = self.next_char()?;
			match ch {
				None => { return Ok(ch) },
				Some(c) if !c.is_whitespace() => { return Ok(ch); },
				_ => {}
			}
		}
	}

	/// Read and discard characters from stream until it has terminated or the target character has been read.
	/// Returns `Ok(true)` if stream reached EOF. Returns `Err` if any IoErrors occurred during the process.
	pub fn skip_till(&mut self, target: char) -> IoResult<bool>
	where T: Read {
		loop {
			let ch = self.next_char()?;
			match ch {
				None => { return Ok(true) },
				Some(c) if c == target => { return Ok(false); },
				_ => {}
			}
		}
	}
}

impl<'a, T: Read> Iterator for CharStream<'a, T> {
	type Item = IoResult<char>;
	fn next(&mut self) -> std::option::Option<<Self as std::iter::Iterator>::Item> {
		self.next_char().transpose()
	}
}