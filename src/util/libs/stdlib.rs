use crate::util::Interpreter;
use crate::util::FunctionBody;
use crate::util::structures::*;

use std::num::Wrapping;
use std::io;
use std::thread;
use std::time::{
    Duration,
    SystemTime
};

use radix_fmt::radix;

use super::make_func;

pub fn get(state: &mut Interpreter) {
    make_func!(
        state; 
        (1) "print" => |values| {
            print!("{}", values[0]);
            Ok(None)
        };
        (1) "println" => |values| {
            println!("{}", values[0]);
            Ok(None)
        };
        (3) "range" => |values| {
            if let (Value::Integer(start), Value::Integer(stop), Value::Integer(step))
                = (&values[0], &values[1], &values[2]) {
                let a = start.min(stop);
                let b = start.max(stop);
                if step.0 == 0 {
                    return Err(LangError::RuntimeError(
                        "Can't make a range with 0 step".into()
                    ))
                }
                Ok(Some(Value::Array(
                    if step.0 < 0 {
                        (a.0+1 ..= b.0).rev().step_by(-step.0 as usize)
                        .map(|i| Value::Integer(Wrapping(i)))
                        .collect()
                    } else {
                        (a.0 .. b.0).step_by(step.0 as usize)
                        .map(|i| Value::Integer(Wrapping(i)))
                        .collect()
                    }
                )))
            } else {
                return Err(LangError::RuntimeError(
                    "Can't create a non-integer range".into()
                ))
            }
        };
        (1) "join" => |values| {
            if let Value::Array(arr) = &values[0] {
                let mut out = String::new();
                for v in arr {
                    out.push_str(v.to_string().as_str());
                }
                return Ok(Some(Value::String(out)));
            }
            Err(LangError::RuntimeError(
                "Expected array to join".into()
            ))
        };
        (1) "len" => |values| {
            return Ok(Some(Value::Integer(Wrapping(
                match &values[0] {
                    Value::Array(arr) => arr.len() as i64,
                    Value::Dictionary(dict) => dict.0.len() as i64,
                    Value::Set(set) => set.0.len() as i64,
                    Value::String(string) => string.chars().count() as i64,
                    _ => return Err(
                        LangError::RuntimeError(
                            "Tried to get the length of an unsized value".into()
                        )
                    )
                }
            ))))
        };
        (0) "input" => |_| {
            let io = io::stdin();
            let mut out = String::new();
            match io.read_line(&mut out) {
                Ok(_) => {
                   Ok(Some(Value::String(out[0..out.len()-1].into()))) // Strip newline
                },
                Err(e) => Err(LangError::RuntimeError(
                    format!("Error reading input: {}", e).into()
                ))
            }
        };
        (1) "chr" => |args| {
            if let Value::Integer(i) = args[0] {
                if i.0 < 0 {
                    return Err(LangError::RuntimeError("Can't get a negative character".into()));
                }
                if let Some(c) = char::from_u32(i.0 as u32) {
                    let o = c.to_string();
                    return Ok(Some(Value::String(o)));
                } else {
                    return Err(LangError::RuntimeError(format!("Invalid character: {i}")));
                }
            } else {
                return Err(LangError::RuntimeError("Expected integer for call to chr".into()));
            }
        };
        (1) "ord" => |args| {
            if let Value::String(s) = &args[0] {
                if s.chars().count() == 1 {
                    let c = s.chars().nth(0).unwrap() as i64;
                    return Ok(Some(Value::Integer(Wrapping(c))));
                } else {
                    return Err(LangError::RuntimeError(
                        format!("Expected string of length 1 for call to ord, got length {}", s.chars().count())
                    ))
                }
            } else {
                return Err(LangError::RuntimeError("Expected string for call to ord".into()));
            }
        };
        (2) "radix" => |args| {
            if let Value::Integer(b) = &args[1] {
                if !(2..=36).contains(&b.0) {
                    return Err(LangError::RuntimeError(format!("Base for radix should be in range [2, 36], got {}", b)));
                }
                let b = b.0 as u32;
                match &args[0] {
                    Value::Integer(i) => {
                        return Ok(Some(Value::String(
                            format!("{}", radix(i.0, b as u8))
                        )))
                    },
                    Value::String(s) => {
                        if let Ok(v) = u32::from_str_radix(s.as_str(), b) {
                            return Ok(Some(Value::Integer(Wrapping(v as i64))));
                        } else {
                            return Err(LangError::RuntimeError(format!("Failed to convert {} to int of base {}", s, b)));
                        }
                    },
                    _ => {
                        return Err(LangError::RuntimeError("Expected string or integer for first value of radix".into()));
                    }
                }
            } else {
                return Err(LangError::RuntimeError("Expected an integer for base of radix".into()));
            }
        };
        (1) "sleep" => |args| {
            let t: Duration;
            if let Value::Integer(i) = args[0] {
                let i = if i.0 < 0 {0u64} else {i.0 as u64};
                t = Duration::from_secs(i);
            } else if let Value::Float(f) = args[1] {
                let f = if *f < 0.0 {0.0} else {*f};
                if !f.is_finite() {
                    return Err(LangError::RuntimeError("Non-finite value given for sleep duration".into()));
                }
                t = Duration::from_secs_f64(f);
            } else {
                return Err(LangError::RuntimeError("Expected numeric value for sleep duration".into()));
            }
            thread::sleep(t);
            Ok(None)
        };
        (0) "time" => |_| {
            let t = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).map_err(
                |_| LangError::RuntimeError("Current system time is before unix epoch (???)".into())
            )?;
            Ok(Some(Value::Float(HashableFloat(t.as_secs_f64()))))
        };
    );
}
