use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::{BuildHasher, Hash},
    num::{ParseFloatError, ParseIntError, Wrapping},
    str::ParseBoolError,
    ops::{Deref, DerefMut}, 
    path::PathBuf
};
use thiserror::Error;
use xmltree::ParseError;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Literal {
    Integer(i64),
    Float(HashableFloat),
    Boolean(bool),
    String(String),
    Array,
    Dictionary,
    Set,
    Null,
}

// May cause some issues with imprecision and NaN values, but that's for the end developer to handle responsibly
#[derive(Clone, Copy, Debug)]
pub struct HashableFloat(pub f64);

impl HashableFloat {
    fn key(&self) -> u64 {
        self.0.to_bits()
    }
}

impl std::hash::Hash for HashableFloat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key().hash(state)
    }
}

impl Deref for HashableFloat {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for HashableFloat {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for HashableFloat {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for HashableFloat {}

// Kind of poor performance, but it's alright
#[derive(Clone, Debug, Eq)]
pub struct HashableMap<K: Hash, V: Hash>(pub HashMap<K, V>);

impl<K: Hash, V: Hash> std::hash::Hash for HashableMap<K, V> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for kv in self.0.iter() {
            kv.hash(state)
        }
    }
}

impl<K: Hash, V: Hash> PartialEq for HashableMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        let hasher = &mut self.0.hasher().build_hasher();
        self.hash(hasher) == other.hash(hasher)
    }
}

impl<K: Hash, V: Hash> Deref for HashableMap<K, V> {
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: Hash, V: Hash> DerefMut for HashableMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Debug, Eq)]
pub struct HashableSet<V: Hash>(pub HashSet<V>);

impl<V: Hash> std::hash::Hash for HashableSet<V> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for v in self.0.iter() {
            v.hash(state)
        }
    }
}

impl<V: Hash> PartialEq for HashableSet<V> {
    fn eq(&self, other: &Self) -> bool {
        let hasher = &mut self.0.hasher().build_hasher();
        self.hash(hasher) == other.hash(hasher)
    }
}

impl<V: Hash> Deref for HashableSet<V> {
    type Target = HashSet<V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V: Hash> DerefMut for HashableSet<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Operators

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Rem,
    Shl,
    Shr,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Lt,
    Leq,
    Gt,
    Geq,
    Access
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum UnOp {
    Neg,
    Not,
    Abs
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Control {
    If,
    While,
    For(String),
    Delete(String),
    Break,
    Continue,
    Return,
    Public(String)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NodeKind {
    Literal(Literal),
    BinOp(BinOp),
    UnOp(UnOp),
    Assign(String),
    Argument(String),
    Control(Control),
    Cast(String),
    Root,
    Block,
    Try,
    Function(Option<String>),
    Arguments,
    Call(String),
    Pair,
    Access(String),
    Import(String)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Node {
    pub idx: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub kind: NodeKind,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeArena {
    pub nodes: Vec<Node>,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl NodeArena {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn get_depth(&self, index: usize) -> usize {
        if let Some(p) = self.nodes[index].parent {
            1 + self.get_depth(p)
        } else {
            0
        }
    }

    pub fn add_node(&mut self, kind: NodeKind, parent: Option<usize>) -> Result<usize, ()> {
        let idx = self.nodes.len();
        if let Some(p) = parent {
            if let Some(par) = self.nodes.get_mut(p) {
                par.children.push(idx)
            } else {
                return Err(());
            }
        }
        self.nodes.push(Node {
            idx,
            parent,
            children: Vec::new(),
            kind,
        });
        Ok(idx)
    }

    fn add_from(&mut self, other: &NodeArena, idx: usize, keep_parent: bool, offset: usize) {
        let mut this = other.nodes.get(idx).unwrap().clone();
        if !keep_parent {
            this.parent = None;
        } else if this.parent.is_some() {
            this.parent = Some(this.parent.unwrap() - offset);
        }
        this.idx -= offset;
        for child in this.children.iter_mut() {
            *child -= offset;
        }
        self.nodes.push(this);
        for child in other.nodes.get(idx).unwrap().children.iter() {
            self.add_from(other, *child, true, offset);
        }
    }
    
    pub fn subtree(&self, idx: usize) -> NodeArena {
        let mut out = NodeArena::new();
        let mut root = self.nodes.get(idx).unwrap().to_owned();
        root.parent = None;
        out.add_from(self, idx, false, idx);
        out
    }
}

impl fmt::Display for NodeArena {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for node in self.nodes.iter() {
            write!(f, "{}", "| ".repeat(self.get_depth(node.idx)))?;
            write!(f, "{}\n", node)?;
        }
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum LangError {
    ParsingError(String),
    RuntimeError(String)
}

impl fmt::Display for LangError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LangError::ParsingError(err) => format!("Parsing error: {}", err),
                LangError::RuntimeError(err) => format!("Runtime error: {}", err)
            }
        )
    }
}

// Nah, too lazy to come up with a good solution to this

macro_rules! error_from_hack {
    ($i: ident, $($il: ident),+) => {
        error_from_hack!($i);
        error_from_hack!($($il),+);
    };
    ($i: ident) => {
        impl From<$i> for LangError {
            fn from(value: $i) -> Self {
                LangError::ParsingError(value.to_string())
            }
        }
    }
}

error_from_hack!(ParseIntError, ParseFloatError, ParseBoolError, ParseError);


// Structures for interpretation

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum FunctionBody {
    Native(fn(Vec<Value>) -> Result<Option<Value>, LangError>),
    User(NodeArena),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Integer(Wrapping<i64>),
    Float(HashableFloat),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Dictionary(HashableMap<Value, Value>),
    Set(HashableSet<Value>),
    Function(Vec<String>, FunctionBody),
    Null,
    Break,
    Continue,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", **fl),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Array(a) => write!(f, "{:?}", a),
            Value::Dictionary(d) => write!(f, "{:?}", d),
            Value::Set(s) => write!(f, "{:?}", s),
            Value::Function(args, _) => write!(f, "<func: {}>", args.join(", ")),
            Value::Null => write!(f, "null"),
            _ => unimplemented!()
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Interpreter {
    pub variables: HashMap<String, Value>,
    pub stack: Vec<usize>,
    pub public: HashSet<String>,
    pub path: PathBuf,
    pub program_path: PathBuf
}

impl Interpreter {
    pub fn new(path: PathBuf, program_path: PathBuf) -> Self {
        Self { 
            variables: HashMap::<String, Value>::new(), 
            stack: Vec::new(),
            public: HashSet::<String>::new(),
            path,
            program_path
        }
    }
}
