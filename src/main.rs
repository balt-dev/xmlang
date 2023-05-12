mod util;
use util::parser;
use util::structures::*;

use std::env;
use std::path::PathBuf;

/* Yahtzee roll
<program>
    <!-- Simple yahtzee roll, no score counting -->
    <import id="[std]" />
    <import id="[random]" />
    <func id="roll">
        <args />
        <block>
            <assign id="dice">
                <arr>
                    <null />
                    <null />
                    <null />
                    <null />
                    <null />
                </arr>
            </assign>
            <for>
                <assign id="i">
                    <int>0</int>
                </assign>
                <block>
                    <lt>
                        <access id="i" />
                        <int>5</int>
                    </lt>
                </block>
                <block>
                    <assign id="i">
                        <add>
                            <access id="i" />
                            <int>1</int>
                        </add>
                    </assign>
                </block>
                <block>
                    <assign id="dice">
                        <access id="i" />
                        <add>
                            <int>
                                <mul>
                                    <call id="rand" />
                                    <int>6</int>
                                </mul>
                            </int>
                            <int>1</int>
                        </add>
                    </assign>
                </block>
            </for>
            <return>
                <access id="dice" />
            </return>
        </block>
    </func>
    <call id="print">
        <str>Rolling...</str>
    </call>
    <assign id="rolled-dice">
        <call id="roll" />
    </assign>
    <call id="print">
        <access id="rolled-dice" />
        <str>&#10;Done!</str>
    </call>
</program>
*/

fn main() -> Result<(), ()> {
    let mut cli_args = env::args();
    cli_args.next().expect("Wasn't passed filepath? This shouldn't happen");
    let program_path: PathBuf;
    if let Some(p) = cli_args.next() {
        program_path = PathBuf::from(p);
    } else {
        println!("Usage: <path> [arg1] [arg2] [arg3]...");
        return Ok(());
    }
    let args: Vec<String> = cli_args.collect();
    let raw_program = match std::fs::read_to_string(&program_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read program: {e}");
            return Err(())
        }
    };
    let tree = match parser::parse(raw_program) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Failed to parse program: {e}");
            return Err(());
        }
    };
    let path = std::env::current_dir().expect("Need read permission on current directory");
    let mut intr = Interpreter::new(path.into(), program_path);
    intr.variables.insert(
        "!args".into(),
        Value::Array(
            args.into_iter().map(|s| Value::String(s)).collect()
        )
    );
    match util::run(&tree, &mut intr) {
        Ok(_) => {
            return Ok(());
        }
        Err(e) => {
            eprintln!("{e}");
            return Err(());
        }
    }
}
