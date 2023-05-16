pub mod parser;
pub mod structures;
pub mod libs;

use std::{
    collections::{HashMap, HashSet}, 
    iter,
    num::Wrapping,
};

use substring::Substring;
use structures::*;


macro_rules! numeric_map {
    ($token: ident, $a: ident, $b: ident | $($name: ident => $op: tt),+) => {
        match $token {
            $(
                BinOp::$name => match ($a, $b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(x $op y),
                    (Value::Integer(x), Value::Float(y)) => Value::Float(HashableFloat(x.0 as f64 $op (*y))),
                    (Value::Float(x), Value::Integer(y)) => Value::Float(HashableFloat((*x) $op y.0 as f64)),
                    (Value::Float(x), Value::Float(y)) => Value::Float(HashableFloat((*x) $op (*y))),
                    (x, y) => {
                        return Err(LangError::RuntimeError(
                            format!("Can't {} {} and {}", stringify!($name), x, y)
                        )) // Can't do this with these types
                    }
                }
            )+
            _ => unreachable!()
        }
    };
}

macro_rules! bitwise_map {
    ($token: ident, $a: ident, $b: ident | $($name: ident => $op: tt),+) => {
        match $token {
            $(
                BinOp::$name => match ($a, $b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(Wrapping(x.0 $op y.0)),
                    (x, y) => 
                    return Err(LangError::RuntimeError(
                        format!("Can't {} {} and {}", stringify!($name), x, y)
                    ))
                }
            )+
            _ => unreachable!()
        }
    };
}

macro_rules! comparison_map {
    ($token: ident, $a: ident, $b: ident | $($name: ident => $op: tt),+) => {
        match $token {
            $(
                BinOp::$name => Value::Boolean(match ($a, $b) {
                    (Value::Integer(x), Value::Integer(y)) => {x $op y},
                    (Value::Integer(x), Value::Float(y)) => {(x.0 as f64) $op *y},
                    (Value::Float(x), Value::Integer(y)) => {(*x) $op (y.0 as f64)},
                    (Value::Float(x), Value::Float(y)) => {(*x) $op (*y)},
                    (Value::String(x), Value::String(y)) => {x $op y},
                    (Value::Boolean(x), Value::Boolean(y)) => {x $op y},
                    (x, y) => 
                    return Err(LangError::RuntimeError(
                        format!("Can't compare {} and {}", x, y)
                    ))
                }),
            )+
            _ => unreachable!()
        }
    };
}

pub fn run(tree: &NodeArena, state: &mut Interpreter) -> Result<Option<Value>, LangError> {
    let root = &tree.nodes[0];
    match &root.kind {
        NodeKind::Root | NodeKind::Block => {
            for child in root.children.iter() {
                let subtree = tree.subtree(*child);
                if let Some(v) = run(&subtree, state)? {
                    return Ok(Some(v));
                }
            }
        },
        NodeKind::Literal(lit) => {
            return Ok(Some(match lit {
                Literal::Null => Value::Null,
                Literal::Integer(i) => Value::Integer(Wrapping(*i)),
                Literal::Float(f) => Value::Float(*f),
                Literal::Boolean(b) => Value::Boolean(*b),
                Literal::String(s) => Value::String(s.clone()),
                Literal::Array => {
                    let mut arr: Vec<Value> = Vec::new();
                    for child_idx in root.children.iter() {
                        let value = run(&tree.subtree(*child_idx), state)?.unwrap_or(Value::Null);
                        arr.push(value);
                    }
                    return Ok(Some(Value::Array(arr)));
                },
                Literal::Set => {
                    let mut set: HashSet<Value> = HashSet::new();
                    for child_idx in root.children.iter() {
                        let value = run(&tree.subtree(*child_idx), state)?.unwrap_or(Value::Null);
                        set.insert(value);
                    }
                    return Ok(Some(Value::Set(HashableSet(set))));
                },
                Literal::Dictionary => {
                    let mut map: HashMap<Value, Value> = HashMap::new();
                    for pair_idx in root.children.iter() {
                        let pair = &tree.nodes[*pair_idx];
                        if let NodeKind::Pair = pair.kind {
                            if pair.children.len() != 2 {
                                return Err(LangError::RuntimeError(
                                    "Dictionary pair must have exactly two children".into()
                                )); // Pair must have 2 children
                            }
                            let key = run(&tree.subtree(pair.children[0]), state)?.unwrap_or(Value::Null);
                            let value = run(&tree.subtree(pair.children[1]), state)?.unwrap_or(Value::Null);
                            map.insert(key, value);
                        } else {
                            return Err(LangError::RuntimeError(
                                "Dictionary child isn't a pair".into()   
                            )); // Not a pair
                        }
                    }
                    return Ok(Some(Value::Dictionary(HashableMap(map))));
                }
            }));
        },
        NodeKind::Assign(id) => {
            let id = id.clone();
            let value: Value;
            if root.children.len() > 0 {
                let subtree = tree.subtree(root.children[0]);
                value = run(&subtree, state)?.unwrap_or(Value::Null);
            } else {
                value = Value::Null;
            }
            if root.children.len() > 1 {
                let target = run(&tree.subtree(root.children[1]), state)?.unwrap_or(Value::Null);
                let var = state.variables.get_mut(&id).ok_or(
                    LangError::RuntimeError(
                        format!("Can't assign part of {id} with no children")
                    )
                )?;
                match (var, target) {
                    (Value::Array(arr), Value::Integer(i)) => {
                        if arr.len() <= i.0 as usize || 0 > i.0 {
                            return Err(LangError::RuntimeError(
                                format!("Can't set index {} of array with length {}", i.0, arr.len())
                            ))
                        }
                        arr[i.0 as usize] = value;
                    }
                    (Value::Dictionary(dict), k) => {
                        dict.insert(k, value);
                    }
                    (Value::String(a), Value::Integer(i)) => {
                        let s = value.to_string();
                        let mut out = a.substring(0, i.0 as usize).to_string();
                        out.push_str(s.as_str());
                        out.push_str(a.substring(i.0 as usize, s.len()));
                        a.clear();
                        a.push_str(out.as_str())
                    }
                    _ => return Err(LangError::RuntimeError(
                        "Can't set attribute of atomic variable".into()
                    ))
                }
            } else {
                state.variables.insert(id, value);
            }
        },
        NodeKind::BinOp(op) => {
            if root.children.len() > 2 {
                return Err(LangError::RuntimeError(
                    "Need two children for a binary operation".into()
                ))
            }
            let a = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
            let b = run(&tree.subtree(root.children[1]), state)?.unwrap_or(Value::Null);
            return Ok(Some(match op {
                BinOp::Add => {
                    match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
                        (Value::Integer(x), Value::Float(y)) => Value::Float(HashableFloat(x.0 as f64 + *y)),
                        (Value::Float(x), Value::Integer(y)) => Value::Float(HashableFloat(*x + y.0 as f64)),
                        (Value::Float(x), Value::Float(y)) => Value::Float(HashableFloat(*x + *y)),
                        (Value::Array(mut x), y) => {
                            x.push(y);
                            Value::Array(x)
                        },
                        (Value::Set(mut x), y) => {
                            x.insert(y);
                            Value::Set(x)
                        },
                        (Value::String(x), Value::String(y)) => {
                            Value::String(format!("{}{}", x, y))
                        }
                        (x, y) => {
                            return Err(LangError::RuntimeError(
                                format!("Can't add {} and {}", x, y)
                            ))
                        }
                    }
                },
                BinOp::Sub => {
                    match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
                        (Value::Integer(x), Value::Float(y)) => Value::Float(HashableFloat(x.0 as f64 - *y)),
                        (Value::Float(x), Value::Integer(y)) => Value::Float(HashableFloat(*x - y.0 as f64)),
                        (Value::Float(x), Value::Float(y)) => Value::Float(HashableFloat(*x - *y)),
                        (Value::Array(mut x), Value::Integer(y)) => {
                            if y.0 > 0 && y.0 < x.len() as i64 {
                                x.remove(y.0 as usize);
                            }
                            Value::Array(x)
                        },
                        (Value::Set(mut x), y) => {
                            x.remove(&y);
                            Value::Set(x)
                        },
                        (Value::Dictionary(mut x), y) => {
                            x.remove(&y);
                            Value::Dictionary(x)
                        },
                        (x, y) => {
                            return Err(LangError::RuntimeError(
                                format!("Can't subtract {} and {}", x, y)
                            )) // Can't do this with these types
                        }
                    }
                },
                token @ (BinOp::Mul | BinOp::Div | BinOp::Rem) => numeric_map! {
                    token, a, b |
                    Mul => *,
                    Div => /,
                    Rem => %
                },
                BinOp::And => match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(Wrapping(x.0 & y.0)),
                    (Value::Boolean(x), Value::Boolean(y)) => Value::Boolean(x && y),
                    (Value::Set(x), Value::Set(y)) => Value::Set(HashableSet(x.intersection(&y).map(|v| v.clone()).collect())),
                    (x, y) => {
                        return Err(LangError::RuntimeError(
                            format!("Can't bitwise and {} and {}", x, y)
                        )) // Can't do this with these types
                    }
                }
                BinOp::Or => match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(Wrapping(x.0 | y.0)),
                    (Value::Boolean(x), Value::Boolean(y)) => Value::Boolean(x || y),
                    (Value::Set(x), Value::Set(y)) => Value::Set(HashableSet(x.union(&y).map(|v| v.clone()).collect())),
                    (x, y) => {
                        return Err(LangError::RuntimeError(
                            format!("Can't bitwise and {} and {}", x, y)
                        )) // Can't do this with these types
                    }
                }
                BinOp::Xor => match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(Wrapping(x.0 & y.0)),
                    (Value::Boolean(x), Value::Boolean(y)) => Value::Boolean(x && y),
                    (Value::Set(x), Value::Set(y)) => {
                        let mut v: HashSet<&Value> = x.difference(&y).collect();
                        v = v.union(&y.difference(&x).collect()).map(|v| *v).collect();
                        Value::Set(HashableSet(v.iter().map(|v| (*v).clone()).collect()))
                    },
                    (x, y) => {
                        return Err(LangError::RuntimeError(
                            format!("Can't bitwise and {} and {}", x, y)
                        )) // Can't do this with these types
                    }
                }
                token @ (BinOp::Shl | BinOp::Shr) => bitwise_map! {
                    token, a, b |
                    Shl => <<,
                    Shr => >>
                },
                BinOp::Eq => Value::Boolean(a == b),
                BinOp::Ne => Value::Boolean(a != b),
                token @ (BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq) => comparison_map! {
                    token, a, b |
                    Lt => <,
                    Gt => >,
                    Leq => <=,
                    Geq => >=
                },
                BinOp::Pow => {
                    match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(Wrapping(x.0.wrapping_pow(y.0 as u32))),
                        (Value::Integer(x), Value::Float(y)) => Value::Float(HashableFloat((x.0 as f64).powf(*y))),
                        (Value::Float(x), Value::Integer(y)) => Value::Float(HashableFloat((*x).powi(y.0 as i32))),
                        (Value::Float(x), Value::Float(y)) => Value::Float(HashableFloat((*x).powf(*y))),
                        (x, y) => 
                        return Err(LangError::RuntimeError(
                            format!("Can't raise {} to the power of {}", x, y)
                        ))
                    }
              },
                BinOp::Access => match (a, b) {
                    (Value::Array(x), Value::Integer(y)) => x.get(y.0 as usize).ok_or(
                        LangError::RuntimeError(
                            format!("Index {y} is out of bounds for array with length {}", x.len())
                        )
                    )?.clone(),
                    (Value::Set(x), y) => Value::Boolean(x.contains(&y)),
                    (Value::Dictionary(x), y) => x.get(&y).ok_or(
                        LangError::RuntimeError(
                            format!("No key {y}")
                        )
                    )?.clone(),
                    (Value::String(x), Value::Integer(y)) => {
                        if y.0 < 0 {
                            return Err(LangError::RuntimeError(
                                format!("Can't index a negative value {}", y.0)
                            )) // Cannot index negative
                        }
                        let y = y.0 as usize;
                        Value::String(x.chars().nth(y).ok_or(
                            LangError::RuntimeError(
                                format!("Index {y} is out of bounds for string with length {}", x.len())
                            )
                        )?.clone().to_string())
                     }
                    (x, y) => {
                        return Err(LangError::RuntimeError(
                            format!("Can't access member {} of {}", y, x)
                        ))
                    }
                }
            }));
        },
        NodeKind::UnOp(op) => {
            if root.children.len() > 1 {
                return Err(LangError::RuntimeError(
                    "Need exactly one child for unary operator".into()
                ))
            }
            let a = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
            return Ok(Some(match op {
                UnOp::Neg => match a {
                    Value::Integer(x) => Value::Integer(-x),
                    Value::Float(x) => Value::Float(HashableFloat(-*x)),
                    x => return Err(LangError::RuntimeError(
                        format!("Can't make {} negative", x)
                    ))
                },
                UnOp::Abs => match a {
                    Value::Integer(x) => Value::Integer(Wrapping(x.0.abs())),
                    Value::Float(x) => Value::Float(HashableFloat(x.abs())),
                    x => return Err(LangError::RuntimeError(
                        format!("Can't get the absolute value of {}", x)
                    ))
                },
                UnOp::Not => match a {
                    Value::Boolean(x) => Value::Boolean(!x),
                    Value::Integer(x) => Value::Integer(Wrapping(!x.0)),
                    x => return Err(LangError::RuntimeError(
                        format!("Can't invert {}", x)
                    ))
                },
            }));
        },
        NodeKind::Cast(c) => {
            if root.children.len() > 1 {
                return Err(LangError::RuntimeError(
                    "Can't cast nothing".into()
                ))
            }
            let a = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
            return match (c.as_str(), &a) {
                ("int", Value::Integer(_)) => Ok(Some(a)),
                ("int", Value::Float(x)) => Ok(Some(Value::Integer(Wrapping(**x as i64)))),
                ("int", Value::Boolean(x)) => Ok(Some(Value::Integer(Wrapping(*x as i64)))),
                ("int", Value::String(x)) => Ok(Some(Value::Integer(Wrapping(x.parse()?)))),
                ("int", Value::Null) => Ok(Some(Value::Integer(Wrapping(0)))),
                ("float", Value::Integer(x)) => Ok(Some(Value::Float(HashableFloat(x.0 as f64)))),
                ("float", Value::Float(_)) => Ok(Some(a)),
                ("float", Value::Boolean(x)) => Ok(Some(Value::Float(HashableFloat(*x as i64 as f64)))),
                ("float", Value::String(x)) => Ok(Some(Value::Float(HashableFloat(x.parse()?)))),
                ("float", Value::Null) => Ok(Some(Value::Float(HashableFloat(0.0)))),
                ("bool", Value::Integer(x)) => Ok(Some(Value::Boolean(x.0 != 0))),
                ("bool", Value::Float(x)) => Ok(Some(Value::Boolean(**x != 0.0))),
                ("bool", Value::Boolean(_)) => Ok(Some(a)),
                ("bool", Value::String(x)) => Ok(Some(Value::Boolean(!x.is_empty()))),
                ("bool", Value::Null) => Ok(Some(Value::Boolean(false))),
                ("str", _) => Ok(Some(Value::String(a.to_string()))),
                ("arr", Value::Array(_)) => Ok(Some(a)),
                ("arr", Value::Set(x)) => Ok(Some(Value::Array(x.iter().cloned().collect()))),
                ("arr", Value::Dictionary(x)) => Ok(Some(Value::Array(x.keys().cloned().collect()))),
                ("arr", Value::String(string)) => Ok(Some(Value::Array(string.chars().map(|chr| Value::String(chr.to_string())).collect()))),
                ("arr", Value::Null) => Ok(Some(Value::Array(Vec::new()))),
                ("set", Value::Array(x)) => Ok(Some(Value::Set(HashableSet(HashSet::from_iter(x.iter().cloned()))))),
                ("set", Value::Set(_)) => Ok(Some(a)),
                ("set", Value::Dictionary(x)) => Ok(Some(Value::Set(HashableSet(x.keys().cloned().collect())))),
                ("set", Value::Null) => Ok(Some(Value::Set(HashableSet(HashSet::new())))),
                ("dict", Value::Array(x)) => Ok(Some(Value::Dictionary(HashableMap(HashMap::from_iter(x.iter().cloned().zip(iter::repeat(Value::Null))))))),
                ("dict", Value::Set(x)) => Ok(Some(Value::Dictionary(HashableMap(HashMap::from_iter(x.iter().cloned().zip(iter::repeat(Value::Null))))))),
                ("dict", Value::Dictionary(_)) => Ok(Some(a)),
                ("dict", Value::Null) => Ok(Some(Value::Dictionary(HashableMap(HashMap::new())))),
                (x, y) => Err(LangError::RuntimeError(
                    format!("Can't cast {} to type {}", y, x)
                ))
            };
        },
        NodeKind::Call(fnc) => {
            let mut args = Vec::new();
            for i in 0..root.children.len() {
                args.push(run(&tree.subtree(root.children[i]), state)?.unwrap_or(Value::Null));
            }
            let func;
            if let Some(f) = fnc {
                func = state.variables.get(f).cloned().ok_or(
                    LangError::RuntimeError(
                        format!("Tried to call non-existent function {f}")
                    )
                )?;
            } else {
                if root.children.len() < 1 {
                    return Err(LangError::RuntimeError(
                        "Tried to call with no function specified".into()
                    ))
                }
                func = args.remove(0);
            }
            if let Value::Function(arg_names, body) = func {
                let mut state = state.clone();
                if args.len() != arg_names.len() {
                    return Err(LangError::RuntimeError(
                        "Wrong number of arguments for call".into()
                    ))
                }
                for (arg, name) in args.iter().zip(arg_names.iter()) {
                    state.variables.insert(name.clone(), arg.clone());
                }
                match body {
                    FunctionBody::Native(f) => return f(args),
                    FunctionBody::User(ref arena) => return run(arena, &mut state)
                } 
            } else {
                return Err(LangError::RuntimeError(
                    "Can't call a non-function".into()
                ))
            }
        },
        NodeKind::Function(name) => {
            if root.children.len() != 2 {
                return Err(LangError::RuntimeError(
                    "Need exactly two children while defining function".into()
                ))
            }
            let args_idx = root.children[0];
            let body_idx = root.children[1];
            let mut args = Vec::new();
            if let NodeKind::Arguments = tree.nodes[args_idx].kind {
                for i in tree.nodes[args_idx].children.iter() {
                    if let NodeKind::Argument(name) = &tree.nodes[*i].kind {
                        args.push(name.to_owned());
                    } else {
                        return Err(LangError::RuntimeError(
                            "Non-argument in argument list of function".into()
                        ))
                    }
                }
            } else {
                return Err(LangError::RuntimeError(
                    "Non-argument list as first child of function block".into()
                ));
            }
            let val = Value::Function(
                args,
                FunctionBody::User(tree.subtree(body_idx)),
            );
            if let Some(name) = name {
                state.variables.insert(name.clone(), val);
            } else {
                return Ok(Some(val));
            }
        },
        NodeKind::Control(c) => {
            match c {
                Control::Return => {
                    if root.children.len() < 1 {
                        return Err(LangError::RuntimeError(
                            "Need at least one child for return statement".into()
                        ));
                    }
                    return Ok(Some(run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null)));
                },
                Control::Delete(name) => {
                    if let Some(_) = state.public.get(name) {
                        return Err(LangError::RuntimeError(
                            "Can't delete a public variable".into()
                        ));
                    }
                    state.variables.remove(name);
                },
                Control::Break => {
                    return Ok(Some(Value::Break));
                },
                Control::Continue => {
                    return Ok(Some(Value::Continue));
                },
                Control::If => {
                    if root.children.len() < 1 {
                        return Err(LangError::RuntimeError(
                            "Need at least one child for if statement".into()
                        ));
                    }
                    // pairing of condition and body, with an optional lone body at the end being the else
                    let mut pairs = Vec::new();
                    for i in (0..root.children.len()-2).step_by(2) {
                        pairs.push((root.children[i], root.children[i + 1]));
                    }
                    for (cond, body) in pairs {
                        let cond = run(&tree.subtree(cond), state)?.unwrap_or(Value::Null);
                        if let Value::Boolean(x) = cond {
                            if x {
                                return run(&tree.subtree(body), state);
                            }
                        } else {
                            return Err(LangError::RuntimeError(
                                "Non-boolean returned for if statement".into()
                            ));
                        }
                    }
                    if root.children.len() % 2 == 0 {
                        return Ok(None);
                    } else {
                        return run(&tree.subtree(root.children[root.children.len() - 1]), state);
                    }
                },
                Control::While => {
                    if root.children.len() > 2 {
                        return Err(LangError::RuntimeError(
                            "Need two children for a while statement".into()
                        ))
                    }
                    let subtree = &tree.subtree(root.children[1]);
                    loop {
                        let cond = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
                        if let Value::Boolean(x) = cond {
                            if x {
                                match run(subtree, state)? {
                                    Some(Value::Break) => break,
                                    Some(Value::Continue) => (),
                                    Some(x) => return Ok(Some(x)),
                                    None => (),
                                }
                            } else {
                                break;
                            }
                        } else {
                            return Err(LangError::RuntimeError(
                                "Non-boolean returned for while loop condition".into()
                            ));
                        }
                    }
                },
                Control::For(name) => {
                    if root.children.len() > 2 {
                        return Err(LangError::RuntimeError(
                            "Need two children for a for statement".into()
                        ))
                    }
                    let cond = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
                    let iterator: Vec<Value>;
                    match cond {
                        Value::Array(arr) => iterator = arr.clone(),
                        Value::Set(set) => iterator = set.0.into_iter().collect(),
                        Value::Dictionary(dict) => iterator = dict.keys().cloned().collect(),
                        Value::String(s) => iterator = s.chars().map(|x| Value::String(x.to_string())).collect(),
                        Value::Integer(i) => iterator = (0..i.0).map(|x| Value::Integer(Wrapping(x))).collect(),
                        v => return Err(LangError::RuntimeError(
                            format!("Can't iterate over {v}")
                        ))
                    }
                    for i in iterator.into_iter() {
                        state.variables.insert(name.to_owned(), i);
                        match run(&tree.subtree(root.children[1]), state)? {
                            Some(Value::Break) => break,
                            Some(Value::Continue) => (),
                            Some(x) => return Ok(Some(x)),
                            None => (),
                        }
                    }
                    // Leave the last variable in scope
                },
                Control::Public(name) => {
                    if state.variables.contains_key(name) {
                        state.public.insert(name.clone());
                    } else {
                        return Err(LangError::RuntimeError(
                            format!("Tried to make non-existent variable {name} public")
                        ))
                    }
                }
            }
        },
        NodeKind::Access(name) => {
            if let Some(value) = state.variables.get(name) {
                let value = value.clone();
                if root.children.len() == 0 {
                    return Ok(Some(value));
                } else if root.children.len() == 1 {
                    // Get nth element of array/string
                    let index = run(&tree.subtree(root.children[0]), state)?.unwrap_or(Value::Null);
                    if let Value::Integer(i) = index {
                        match value {
                            Value::Array(arr) => {
                                if i.0 < 0 || i.0 >= arr.len() as i64 {
                                    return Err(LangError::RuntimeError(
                                        format!("Index {i} out of bounds for array of length {}", arr.len())
                                    ))
                                }
                                return Ok(Some(arr[i.0 as usize].clone()));
                            },
                            Value::String(s) => {
                                if i.0 < 0 || i.0 >= s.len() as i64 {
                                    return Err(LangError::RuntimeError(
                                        format!("Index {i} out of bounds for string of length {}", s.len())
                                    ))
                                }
                                return Ok(Some(Value::String(s.chars().nth(i.0 as usize).unwrap().to_string())));
                            },
                            v => return Err(LangError::RuntimeError(
                                format!("Can't index into {v}")
                            ))
                        }
                    } else {
                        return Err(LangError::RuntimeError(
                            "Non-integer index for access node".into()
                        ))
                    }
                } else {
                    return Err(LangError::RuntimeError(
                        "Too many children for access node".into()
                    ))
                }
            } else {
                return Err(LangError::RuntimeError(
                    format!("Tried to access non-existent variable {name}")
                ))
            }
        },
        NodeKind::Import(name) => {
            if name.starts_with("!") {
                libs::import(&name[1..], state)?;
            } else {
                let mut imp_intr = state.clone();
                let p = state.program_path.parent().expect("Program has no parent????").join(name);
                let raw_program = match std::fs::read_to_string(p) {
                    Ok(s) => s,
                    Err(e) => {
                        return Err(LangError::RuntimeError(
                            format!("Failed to read program: {e}")
                        ))
                    }
                };
                let tree = match parser::parse(raw_program) {
                    Ok(v) => v,
                    Err(e) => {
                        return Err(LangError::RuntimeError(
                            format!("Failed to parse program: {e}")
                        ))
                    }
                };
                run(&tree, &mut imp_intr)?;
                for k in imp_intr.public.into_iter() {
                    let v = imp_intr.variables[&k].clone();
                    state.variables.insert(
                        k,
                        v
                    );
                }
            }
        },
        NodeKind::Try => {
            if root.children.len() != 2 {
                return Err(LangError::RuntimeError(
                    "Need exactly two children for a try statement".into()
                ))
            }
            let result = run(&tree.subtree(root.children[0]), state);
            return Ok(match result {
                Ok(v) => v,
                Err(_) => run(&tree.subtree(root.children[1]), state)?
            });
        },
        kind => {
            return Err(LangError::RuntimeError(
                format!("Tag {kind:?} isn't valid in this context")
            ))
        }
    }
    Ok(None)
}
