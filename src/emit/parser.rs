//! Module for SExpr parser. This parser can handle unicode characters reasonably well.
//! Builds a tree of tokens.

// TODO: Extremely verbose. Desperately needs a re-write.

use super::{TlError, TlErrorType};
use crate::emit::{Sexpr, TextualLocation};
use crate::utils::CharStream;
use std::io::Read;

const TOKEN_DELIMS: &str = ")(][ \t\r\n;";

/// Parse read `r`, and construct the `Sexpr`.
pub fn parse<T>(r: &mut T) -> Result<Sexpr, TlError>
where
    T: Read,
{
    let mut stream: CharStream<'_, T> = r.into();
    let mut v = Vec::new();
    loop {
        let ch = stream
            .skip_whitespace()
            .map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?;
        match ch {
            None => {
                break;
            } // EOF is expected here.
            Some('(') => {
                v.push(parse_sexpr(&mut stream, ')')?);
            }
            Some(';') => {
                // Using this, instead of `parse_comment`, so that EOF doesn't result in Error.
                if stream
                    .skip_till('\n')
                    .map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
                {
                    break;
                }
            }
            Some(c) => {
                return Err(TlError {
                    etype: TlErrorType::UnexpectedToken,
                    msg: format!("Root level S-expressions must use only '(', not '{c}'"),
                    loc: TextualLocation::record(&stream),
                });
            }
        }
    }
    list!(v)
}

#[inline]
/// Helper function to call `stream.next_char` and map errors to TlErrors.
fn expect_char<T>(stream: &mut CharStream<'_, T>) -> Result<char, TlError>
where
    T: Read,
{
    stream
        .next_char()
        .map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
        .ok_or_else(|| TlError::eof(stream))
}

/// Fetch characters from the stream and push into buffer, until one in the parameter string is found.
/// Returns the last character read from the stream, or any IoErrors that occured.
/// Returns `Err` if stream reaches EOF.
fn cspan<T>(
    stream: &mut CharStream<'_, T>,
    delim: &'static str,
    buf: &mut String,
) -> Result<char, TlError>
where
    T: Read,
{
    loop {
        let ch = expect_char(stream)?;
        if delim.contains(ch) {
            return Ok(ch);
        }
        buf.push(ch);
    }
}

fn int_from_str_radix_auto(s: &str, loc: &TextualLocation) -> Result<i64, TlError> {
    let radix = match s.chars().nth(1) {
        Some('x') => 16,
        Some('b') => 2,
        Some('o') => 8,
        None => {
            // We already know 0 was the first character.
            return Ok(0);
        }
        Some(c) => {
            return Err(TlError {
                etype: TlErrorType::UnexpectedToken,
                msg: format!("Invalid radix prefix '0{c}'. Expected '0x', '0b', or '0o'."),
                loc: *loc,
            })
        }
    };
    i64::from_str_radix(&s[2..], radix).map_err(|e| {
        let msg = format!("'{s}' is not an integer in base {radix},\n\tdetail: {e}");
        TlError {
            etype: TlErrorType::InvalidNumeric,
            msg,
            loc: *loc,
        }
    })
}

fn parse_unicode_hex<T>(stream: &mut CharStream<'_, T>) -> Result<char, TlError>
where
    T: Read,
{
    let s: Result<String, std::io::Error> = stream.take(4).collect();
    let loc = TextualLocation::record(stream);
    let s = s.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?;
    if s.len() < 4 {
        Err(TlError {
            etype: TlErrorType::UnexpectedEOF,
            msg: format!(
                "Unicode escape requires exactly 4 hex digits, but only '{s}' was provided."
            ),
            loc,
        })
    } else {
        u32::from_str_radix(&s, 16)
            .map_err(|e| TlError {
                etype: TlErrorType::InvalidChar,
                msg: format!(
                    "'{s}' is not a valid 4-digit hex for unicode escape sequence.\n\tdetail: {e}"
                ),
                loc,
            })
            .and_then(|v| {
                char::from_u32(v).ok_or_else(|| TlError {
                    etype: TlErrorType::InvalidChar,
                    msg: format!("Hex '{s}' does not signify a character."),
                    loc,
                })
            })
    }
}

fn parse_char_raw<T>(stream: &mut CharStream<'_, T>) -> Result<(char, bool), TlError>
where
    T: Read,
{
    let ch = expect_char(stream)?;
    if ch != '\\' {
        Ok((ch, false))
    } else {
        let ch = expect_char(stream)?;
        Ok((
            match ch {
                '\'' => '\'',
                '"' => '"',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                'u' => parse_unicode_hex(stream)?,
                a => {
                    return Err(TlError { 
						etype: TlErrorType::InvalidChar,
						msg: format!("Unknown escape sequence \\{a}. Only \\\', \\\",\\r, \\n, \\t, \\uXXXX are allowed."),
						loc: TextualLocation::record(stream) 
					});
                }
            },
            true,
        ))
    }
}

fn parse_string<T>(stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where
    T: Read,
{
    let loc = TextualLocation::record(stream);

    let mut s = String::new();
    loop {
        let (ch, esc) = parse_char_raw(stream)?;
        if !esc && ch == '\"' {
            break;
        } else {
            s.push(ch);
        }
    }

    atom!(Str(s), loc.end(stream))
    //Ok(Sexpr::Atom(Token { ttype: TokenType::Str(s), start, end: stream.byte_position() }))
}

fn parse_char<T>(stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where
    T: Read,
{
    let loc = TextualLocation::record(stream);
    // The single quote has already been read, so..
    let (ch, _esc) = parse_char_raw(stream)?;
    if expect_char(stream)? != '\'' {
        Err(TlError {
            etype: TlErrorType::InvalidChar,
            msg: format!("Character literal '{ch}' must be enclosed by single quotes."),
            loc,
        })
    } else {
        atom!(Char(ch), loc.end(stream))
    }
}

fn parse_comment<T>(stream: &mut CharStream<'_, T>) -> Result<(), TlError>
where
    T: Read,
{
    stream
        .skip_till('\n')
        .map_err(|e| TlError::from_ioerr(e, stream.byte_position()))
        .and_then(|c| if c { Err(TlError::eof(stream)) } else { Ok(()) })
}

fn match_parse_literal(ch: char, buf: String, loc: TextualLocation) -> Result<Sexpr, TlError> {
    // ---------------------------------------------------- //
    // First match the basic ones. Boolean.

    if buf == "#t" {
        // If the whole text is the TRUE boolean literal, then that's the Sexpr.
        atom!(Boolean(true), loc)
    } else if buf == "#f" {
        // Similarly for FALSE.
        atom!(Boolean(false), loc)

    // ----------------------------------------------------- //
    // If lead character is alphabetic, '_' or ':', then token is a symbol.
    } else if ch.is_alphabetic() || ch == '_' || buf == "+" || buf == "-" {
        // If the whole token text itself is just '+' or '-', then they are symbols.
        atom!(Symbol(buf), loc) // Symbol

    // ----------------------------------------------------- //
    // Match integer or floating (numeric) type tokens.

    // If lead character is '0', then the token is either the integer 0, or an integer in hex, binary, or octal
    } else if ch == '0' {
        let value = int_from_str_radix_auto(&buf, &loc)?;
        return atom!(Integer(value), loc);

    // Alternatively, if the lead character is a decimal digit, or '+', '-', the token is of numeric type.
    } else if ch.is_ascii_digit() || ch == '-' || ch == '+' {
        // Attempt to parse text as integer.
        match buf.parse() {
            Ok(i) => {
                return atom!(Integer(i), loc);
            }
            Err(_) => match buf.parse() {
                // Failed, try to parse as float.
                Ok(f) => {
                    return atom!(Float(f), loc);
                }
                Err(_) => {
                    // This token is neither an integer nor floating point literal.
                    return Err(TlError {
                        etype: TlErrorType::InvalidNumeric,
                        msg: format!("'{buf}' is neither an integer nor a floating point literal."),
                        loc,
                    });
                }
            },
        }

    // ----------------------------------------------------- //
    // Any other 'ch', i.e, one that is neither alphabetic, _, nor ascii digit, constitutes a symbol.
    } else {
        // Otherwise, it's a symbol - no doubt.
        atom!(Symbol(buf), loc)
    }
}

fn match_rule<T>(
    stream: &mut CharStream<'_, T>,
    v: &mut Vec<Sexpr>,
    ch: char,
    close_char: char,
) -> Result<bool, TlError>
where
    T: Read,
{
    match ch {
        '"' => v.push(parse_string(stream)?),
        // Note: Here, the general 'char literals in single-quotes' convention is preferred over the traditional scheme solution of `#\char`.
        // For our purposes, the special form of quote will not have the shorthand ', so it is reasonable to use the character here.
        '\'' => v.push(parse_char(stream)?),
        '(' => v.push(parse_sexpr(stream, ')')?),
        '[' => v.push(parse_sexpr(stream, ']')?),
        ';' => parse_comment(stream)?,
        _ => {
            let mut buf = String::new();
            buf.push(ch);

            let loc = TextualLocation::record(stream);

            // Keep track of the terminating character, since it can imply another expr, or termination of this one.
            let term = cspan(stream, TOKEN_DELIMS, &mut buf)?;

            let loc = loc.end(stream);

            // Parse-Match buffer with corresponding literal.
            v.push(match_parse_literal(ch, buf, loc)?);

            if term == close_char {
                return Ok(true); // We're done, this was possibly the last character of the Sexpr.
            }

            // TODO: Fix ugly nested match arm.

            match term {
                ')' | ']' => {
                    return Err(TlError {
                        etype: TlErrorType::UnexpectedToken,
                        msg: format!(
                            "Cannot end token with '{ch}', perhaps you meant '{close_char}'?"
                        ),
                        loc,
                    });
                }
                '(' => v.push(parse_sexpr(stream, ')')?),
                '[' => v.push(parse_sexpr(stream, ']')?),
                ';' => parse_comment(stream)?,
                c if c.is_whitespace() => { /* do nothing */ }
                c => {
                    panic!("Token span has ended on a character ({c}) outside TOKEN_DELIMS!");
                }
            }
        }
    }
    Ok(false)
}

fn parse_sexpr<T>(stream: &mut CharStream<'_, T>, close_char: char) -> Result<Sexpr, TlError>
where
    T: Read,
{
    let mut v = Vec::new();
    let loc = TextualLocation::record(stream);
    loop {
        let ch = stream
            .skip_whitespace()
            .map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
            .ok_or_else(|| TlError::eof(stream))?;
        if ch == close_char {
            break;
        }
        if match_rule(stream, &mut v, ch, close_char)? {
            break;
        }
    }
    list!(v, loc.end(stream))
}
