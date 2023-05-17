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
                Ok(Some(Value::Array(arr)))
            } else {
                Err(LangError::RuntimeError(
                    "Expected array to shuffle".into()
                ))
            }
        };
        (1) "choice" => |values| {
            let arr = values[0].clone();
            if let Value::Array(arr) = arr {
                Ok(arr.choose(&mut rand::thread_rng()).cloned())
            } else {
                Err(LangError::RuntimeError(
                    "Expected array to get choice from".into()
                ))
            }
        };
        (1) "sample" => |values| {
            if let Value::Integer(count) = values[0] {
                let mut out = Vec::new();
                for _ in 0..count.0 {
                    out.push(Value::Float(HashableFloat(rand::random())));
                }
                Ok(Some(Value::Array(out)))
            } else {
                Err(LangError::RuntimeError(
                    "Expected integer dimension of sample".into()
                ))
            }
        };
    }
}
