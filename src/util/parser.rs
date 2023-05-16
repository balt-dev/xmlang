use super::structures::*;
use xmltree::Element;

pub fn parse(string: String) -> Result<NodeArena, LangError> {
    let root_element = Element::parse(string.as_bytes())?;
    let tree = NodeArena::new();
    Ok(parse_element(tree, root_element, None)?)
}

/*
Notes in parentheses are using Emmet syntax
-==Root==-
[x] prog
-==Variables==-
[x] arr (arr>int*50>$)
[x] dict
[x] set
-==Operators==-
[x] add
[x] sub
[x] mul
[x] div
[x] pow
[x] and
[x] or
[x] xor
[x] shl
[x] shr
[x] mod
[x] assign
[x] access (may take form of arr[int], dict[key])
-==Unary operators==-
[x] not
[x] abs
[x] neg
-==Comparisom==-
[x] eq
[x] gt
[x] lt
[x] geq
[x] leq
[x] ne
-==Types==-
[x] int
[x] float
[x] bool
[x] str
[x] null
-==Semantic==-
[x] arg
[x] args
[x] pair
[x] block
[x] call (call.id>arg*N)
-==Control Flow==-
[x] func (func>(args>arg*N)+block)
[x] if (if>block+block)
[x] for (for>block+block+block+block)
[x] while (while>block+block)
[x] break
[x] continue
[x] return
[x] del
[x] import
 */

pub fn parse_element(
    mut tree: NodeArena,
    el: Element,
    parent: Option<usize>,
) -> Result<NodeArena, LangError> {
    let name = el.name.as_str();
    let text: String = match el.get_text() {
        Some(cow) => cow.to_string(),
        None => String::new(),
    };
    let id = el
        .attributes
        .get("id")
        .ok_or(LangError::ParsingError(
            "Named element with no id specified".to_string(),
        ))
        .cloned();
    let kind: NodeKind = match name {
        "program" => NodeKind::Root,
        "try" => NodeKind::Try,
        "assign" => {
            let name = id?;
            NodeKind::Assign(name.to_string())
        }
        kind @ ("int" | "float" | "bool" | "str") => {
            if !el
                .children
                .iter()
                .any(|child| matches!(child, xmltree::XMLNode::Element(_)))
            {
                match kind {
                    "int" => NodeKind::Literal(Literal::Integer(text.parse()?)),
                    "float" => NodeKind::Literal(Literal::Float(HashableFloat(text.parse()?))),
                    "bool" => NodeKind::Literal(Literal::Boolean(text.parse()?)),
                    "str" => NodeKind::Literal(Literal::String(text)),
                    _ => unreachable!(), // wow the match checker is kinda dumb huh
                }
            } else {
                NodeKind::Cast(kind.to_string())
            }
        }
        "null" => NodeKind::Literal(Literal::Null),
        kind @ ("arr" | "set" | "dict") => NodeKind::Literal(match kind {
            "arr" => Literal::Array,
            "set" => Literal::Set,
            "dict" => Literal::Dictionary,
            _ => unreachable!(),
        }),
        kind @ (
            "add" | "sub" | "mul" | "div" | "mod" | "pow" | "and" | "or" | "xor" | 
            "shl" | "shr" | "eq" | "ne" | "lt" | "gt" | "leq" | "geq"
        ) => NodeKind::BinOp(match kind {
            "add" => BinOp::Add,
            "sub" => BinOp::Sub,
            "mul" => BinOp::Mul,
            "div" => BinOp::Div,
            "pow" => BinOp::Pow,
            "and" => BinOp::And,
            "or" => BinOp::Or,
            "xor" => BinOp::Xor,
            "shl" => BinOp::Shl,
            "shr" => BinOp::Shr,
            "mod" => BinOp::Rem,
            "eq" => BinOp::Eq,
            "ne" => BinOp::Ne,
            "lt" => BinOp::Lt,
            "gt" => BinOp::Gt,
            "leq" => BinOp::Leq,
            "geq" => BinOp::Geq,
            _ => unreachable!(),
        }),
        "access" => {
            if let Ok(name) = id {
                NodeKind::Access(name.to_string())
            } else {
                NodeKind::BinOp(BinOp::Access)
            }
        }
        kind @ ("neg" | "not" | "abs") => NodeKind::UnOp(match kind {
            "neg" => UnOp::Neg,
            "not" => UnOp::Not,
            "abs" => UnOp::Abs,
            _ => unreachable!(),
        }),
        "args" => NodeKind::Arguments,
        kind @ ("if" | "for" | "while" | "break" | "continue" | "return" | "public") => NodeKind::Control(match kind {
            "if" => Control::If,
            "for" => Control::For(id?),
            "while" => Control::While,
            "break" => Control::Break,
            "continue" => Control::Continue,
            "return" => Control::Return,
            "public" => Control::Public(id?),
            "delete" => Control::Delete(id?),
            _ => unreachable!(),
        }),
        "arg" => NodeKind::Argument(id?),
        "call" => NodeKind::Call(id.ok()),
        "block" => NodeKind::Block,
        "func" => NodeKind::Function(id.ok()),
        "pair" => NodeKind::Pair,
        "import" => NodeKind::Import(id?),
        _ => {
            return Err(LangError::ParsingError(format!(
                "Unrecognized tag `{}`",
                name
            )))
        }
    };
    let new = tree.add_node(kind, parent).expect("Failed to add node to parent?!");
    for el in el.children {
        match el {
            xmltree::XMLNode::Element(el) => {
                tree = parse_element(tree, el, Some(new))?;
            }
            _ => {}
        }
    }
    Ok(tree)
}
