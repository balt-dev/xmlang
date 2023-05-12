use crate::util::Interpreter;
use crate::util::structures::*;

use rand::prelude::*;

use super::make_func;

pub fn get(state: &mut Interpreter) {
    make_func! {
        state;
        (0) "rand" => |_| {
            Ok(Some(Value::Float(HashableFloat(rand::random()))))
        };
        (1) "shuffle" => |values| {
            let arr = values[0].clone();
            if let Value::Array(mut arr) = arr {
                arr.shuffle(&mut rand::thread_rng());
                return Ok(Some(Value::Array(arr)));
            } else {
                return Err(LangError::RuntimeError(
                    "Expected array to shuffle".into()
                ));
            }
        };
        (1) "choice" => |values| {
            let arr = values[0].clone();
            if let Value::Array(arr) = arr {
                Ok(arr.choose(&mut rand::thread_rng()).cloned())
            } else {
                return Err(LangError::RuntimeError(
                    "Expected array to get choice from".into()
                ));
            }
        };
        (1) "sample" => |values| {
            let arr = values[0].clone();
            if let Value::Integer(count) = arr {
                let mut out = Vec::new();
                for _ in 0..count.0 {
                    out.push(Value::Float(HashableFloat(rand::random())));
                }
                return Ok(Some(Value::Array(out)));
            } else {
                return Err(LangError::RuntimeError(
                    "Expected integer dimension of sample".into()
                ));
            }
        };
    }
}
