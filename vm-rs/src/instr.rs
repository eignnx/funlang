use std::fmt::Display;
use std::io::BufRead;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug)]
pub struct Mod {
    pub instrs: Vec<Instr>,
}

impl Mod {
    pub fn new(reader: impl BufRead) -> Self {
        let mut instrs = vec![];
        for line in reader.lines() {
            let line = line.unwrap();
            let segments: Vec<&str> = line.trim().split(" ").collect();
            let (first, segments) = segments.split_first().unwrap();
            debug_assert!(first.ends_with(':'));
            let instr = decode_instr(&segments);
            instrs.push(instr);
        }
        Self { instrs }
    }
}

pub type Ident = String;

fn decode_instr(segments: &[&str]) -> Instr {
    use Instr::*;
    match segments {
        ["Load", var] => Load(var.to_string()),
        ["Store", var] => Store(var.to_string()),
        ["Const", value @ ..] => Const(decode_value(value)),
        ["Dup"] => Dup,
        ["Over"] => Over,
        ["Rot"] => Rot,
        ["Swap"] => Swap,
        ["Pop"] => Pop,
        ["Add"] => Add,
        ["Sub"] => Sub,
        ["Mul"] => Mul,
        ["Div"] => Div,
        ["Neg"] => Neg,
        ["And"] => And,
        ["Or"] => Or,
        ["Not"] => Not,
        ["Gt"] => Gt,
        ["Lt"] => Lt,
        ["Concat"] => Concat,
        ["Alloc", n] => Alloc(n.parse().unwrap()),
        ["MemWriteDirect", idx] => MemWriteDirect(idx.parse().unwrap()),
        ["MemReadDirect", idx] => MemReadDirect(idx.parse().unwrap()),
        ["TestDiscr", discr] => TestDiscr(discr.parse().unwrap()),
        ["Nop"] => Nop,
        ["JmpIfFalse", addr] => JmpIfFalse(addr.parse().unwrap()),
        ["Jmp", addr] => Jmp(addr.parse().unwrap()),
        ["Intrinsic", intr @ ..] => Intrinsic(decode_intrinsic(intr)),
        ["Call", argc] => Call(argc.parse().unwrap()),
        ["CallDirect", addr, argc] => {
            let addr = addr.parse().unwrap();
            let argc = argc.parse().unwrap();
            CallDirect(addr, argc)
        }
        ["Ret"] => Ret,
        _ => panic!("Can't decode {:?} as an Instr!", segments),
    }
}

#[derive(Debug)]
pub enum Instr {
    Load(String),
    Store(String),
    Const(Value), // Push an immediate value onto stack
    Dup,          // Stack operations
    Over,         // "
    Rot,          // "
    Swap,         // "
    Pop,          // "
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    And,
    Or,
    Not,
    Gt,
    Lt,
    Concat,
    Alloc(usize),
    MemWriteDirect(usize),
    MemReadDirect(usize),
    TestDiscr(usize),
    Nop, // Used to replace labels
    JmpIfFalse(InstrAddr),
    Jmp(InstrAddr),
    Intrinsic(Intrinsic),
    Call(u8),
    CallDirect(InstrAddr, u8),
    Ret, // Jump back to return address
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    VInt(i64),
    VBool(bool),
    VText(String),
    VInstrAddr(InstrAddr),
    VPtr(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            VInt(x) => write!(f, "#{}", x),
            VBool(x) => write!(f, "{}", x),
            VText(x) => write!(f, "{:?}", x),
            VInstrAddr(x) => write!(f, "@{}", x),
            VPtr(x) => write!(f, "*{}", x),
        }
    }
}

fn decode_value(segments: &[&str]) -> Value {
    use Value::*;
    match segments {
        ["VInt", i] => VInt(i.parse().unwrap()),
        ["VBool", "True"] => VBool(true),
        ["VBool", "False"] => VBool(false),
        ["VText", s @ ..] => {
            let s = s.join(" ");
            let s = s[1..s.len() - 1].to_string(); // Remove quotes.
            VText(s)
        }
        ["VInstrAddr", a] => VInstrAddr(a.parse().unwrap()),
        ["VPtr", p] => VPtr(p.parse().unwrap()),
        _ => panic!("Can't decode {:?} as a Value!", segments),
    }
}

#[derive(Debug)]
pub enum Intrinsic {
    EqInt,
    EqBool,
    EqText,
    DbgInt,
    DbgBool,
    DbgText,
    Puts,
    Here(String),
    Exit,
}

fn decode_intrinsic(segments: &[&str]) -> Intrinsic {
    use Intrinsic::*;
    match segments {
        ["EqInt"] => EqInt,
        ["EqBool"] => EqBool,
        ["EqText"] => EqText,
        ["DbgInt"] => DbgInt,
        ["DbgBool"] => DbgBool,
        ["DbgText"] => DbgText,
        ["Puts"] => Puts,
        ["Here", loc @ ..] => Here(loc.join(" ")),
        ["Exit"] => Exit,
        _ => panic!("Can't decode {:?} as an Intrinsic!", segments),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct InstrAddr(pub usize);

impl FromStr for InstrAddr {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.parse()?))
    }
}

impl Display for InstrAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InstrAddr(addr) = self;
        write!(f, "@{}", addr)
    }
}
