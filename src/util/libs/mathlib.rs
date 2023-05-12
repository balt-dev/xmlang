use crate::util::Interpreter;
use crate::util::FunctionBody;
use crate::util::structures::*;

use super::make_func;
    
use std::f64::consts::*;
use std::num::Wrapping;

macro_rules! wrap_unary {
    ($state: ident; $name: expr => $func: ident , $($names: expr => $funcs: ident),+) => {
        wrap_unary!($state; $name => $func);
        wrap_unary!($state; $($names => $funcs),+);
    };
    ($state: ident; $name: expr => $func: ident) => {
        make_func! (
            $state;
            (1) $name => |mut val| {
                if let Value::Integer(i) = val[0] {
                    val[0] = Value::Float(HashableFloat(
                        i.0 as f64
                    ));
                }
                if let Value::Float(v) = val[0] {
                    return Ok(Some(Value::Float(HashableFloat(
                        v.$func()
                    ))));
                } else {
                    return Err(LangError::RuntimeError(
                        format!("Non-number specified for operation {}", stringify!($name))
                    ));
                }
            };
        );
    };
}

macro_rules! wrap_binary {
    ($state: ident; $name: expr => $func: ident , $($names: expr => $funcs: ident),+) => {
        wrap_binary!($state; $name => $func);
        wrap_binary!($state; $($names => $funcs),+);
    };
    ($state: ident; $name: expr => $func: ident) => {
        make_func! (
            $state;
            (2) $name => |mut val| {
                if let Value::Integer(i) = val[0] {
                    val[0] = Value::Float(HashableFloat(
                        i.0 as f64
                    ));
                }
                if let Value::Integer(i) = val[1] {
                    val[1] = Value::Float(HashableFloat(
                        i.0 as f64
                    ));
                }
                if let (Value::Float(a), Value::Float(b)) = (&val[0], &val[1]) {
                    return Ok(Some(Value::Float(HashableFloat(
                        a.$func(**b)
                    ))));
                } else {
                    return Err(LangError::RuntimeError(
                        format!("Non-numbers specified for operation {}", stringify!($name))
                    ));
                }
            };
        );
    };
}

pub fn get(state: &mut Interpreter) {
    state.variables.insert(
        "pi".into(), 
        Value::Float(HashableFloat(PI))
    );
    state.variables.insert(
        "e".into(), 
        Value::Float(HashableFloat(E))
    );
    wrap_unary!(
        state;
        "sin" => sin,
        "cos" => cos,
        "tan" => tan,
        "asin" => asin,
        "acos" => acos,
        "atan" => atan,
        "sinh" => sinh,
        "cosh" => cosh,
        "tanh" => tanh,
        "asinh" => asinh,
        "acosh" => acosh,
        "atanh" => atanh,
        "exp" => exp,
        "ln" => ln,
        "log2" => log2,
        "log10" => log10,
        "sqrt" => sqrt,
        "ceil" => ceil,
        "floor" => floor,
        "round" => round,
        "trunc" => trunc,
        "fract" => fract,
        "sign" => signum,
        "deg" => to_degrees,
        "rad" => to_radians
    );
    wrap_binary!(
        state;
        "atan2" => atan2,
        "hypot" => hypot,
        "max" => max,
        "min" => min
    );
    make_func!(
        state;
        (3) "lerp" => |values| {
            if let (Value::Float(a), Value::Float(b), Value::Float(t)) = (&values[0], &values[1], &values[2]) {
                return Ok(Some(Value::Float(HashableFloat(
                    a.0 + (b.0 - a.0) * t.0
                ))));
            } else if let (Value::Integer(a), Value::Integer(b), Value::Float(t)) = (&values[0], &values[1], &values[2]) {
                return Ok(Some(Value::Integer(
                    Wrapping(((a.0 as f64 + (b.0 as f64 - a.0 as f64)) * t.0) as i64)
                )));
            } else {
                return Err(LangError::RuntimeError(
                    format!("Non-numbers specified for operation lerp")
                ));
            }
        };
    );
}