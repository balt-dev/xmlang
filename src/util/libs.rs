pub mod stdlib;
pub mod randlib;
pub mod mathlib;
pub mod iolib;

use super::structures::{
    Interpreter,
    LangError
};

pub fn import(name: &str, state: &mut Interpreter) -> Result<(), LangError> {
    return Ok(match name {
        "std" => stdlib::get(state),
        "rand" => randlib::get(state),
        "math" => mathlib::get(state),
        "io" => iolib::get(state),
        name => {
            return Err(
                LangError::RuntimeError(format!("No built-in lib {name}"))
            );
        }
    });
}

#[macro_export]
macro_rules! make_func {
    ( $state: ident ; ( $args: literal ) $name: expr => $body: expr ; $( ( $args_l: literal ) $names: expr => $bodies: expr ; )+) => {
        make_func!( $state ; ( $args ) $name => $body ; );
        make_func!( $state ; $( ( $args_l ) $names => $bodies ; )+ );
    };
    ( $state: ident ; ( $args: literal ) $name: expr => $body: expr ; ) => {
        $state.variables.insert(
            $name.into(), 
            Value::Function(
                vec!["".into(); $args],
                FunctionBody::Native(
                    $body
                )
            )
        );
    };
}

pub use make_func;