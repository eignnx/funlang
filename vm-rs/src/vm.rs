use std::{
    collections::HashMap,
    io::{self, Write}
};

use crate::instr::{Ident, Instr, InstrAddr, Intrinsic, Value};

#[derive(Debug)]
pub struct Heap {
    mem: Vec<Value>,
}

impl Heap {
    fn new() -> Self {
        Self { mem: Vec::new() }
    }

    fn alloc(&mut self, slots: usize) -> usize {
        let idx = self.mem.len();
        // Extend and fill the new part with zeros.
        self.mem.resize_with(self.mem.len() + slots, || Value::VInt(0));
        idx
    }
}

#[derive(Debug)]
pub struct Vm {
    memory: Vec<HashMap<String, Value>>,
    stack: Vec<Value>,
    ret_addrs: Vec<InstrAddr>,
    pc: InstrAddr,
    running: bool,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            memory: Vec::new(),
            stack: Vec::new(),
            ret_addrs: Vec::new(),
            pc: InstrAddr(0),
            running: true,
        }
    }

    fn push(&mut self, x: Value) {
        self.stack.push(x);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Vm: Stack underflow!")
    }

    fn pop_bool(&mut self) -> bool {
        match self.pop() {
            Value::VBool(x) => x,
            x => panic!("Expected VBool at TOS, got {:?}!", x),
        }
    }

    fn pop_int(&mut self) -> i64 {
        match self.pop() {
            Value::VInt(x) => x,
            x => panic!("Expected VInt at TOS, got {:?}!", x),
        }
    }

    fn pop_string(&mut self) -> String {
        match self.pop() {
            Value::VString(x) => x,
            x => panic!("Expected VString at TOS, got {:?}!", x),
        }
    }

    fn pop_instr_addr(&mut self) -> InstrAddr {
        match self.pop() {
            Value::VInstrAddr(x) => x,
            x => panic!("Expected VInstrAddr at TOS, got {:?}!", x),
        }
    }

    fn pop_ptr(&mut self) -> usize {
        match self.pop() {
            Value::VPtr(x) => x,
            x => panic!("Expected VPtr at TOS, got {:?}!", x),
        }
    }

    fn load(&mut self, var: &Ident) {
        for frame in self.memory.iter().rev() {
            if let Some(v) = frame.get(var) {
                let v = v.clone();
                self.push(v);
                return;
            }
        }
        panic!("Unbound variable `{}`!", var);
    }

    fn store(&mut self, var: &Ident) {
        let val = self.pop();
        let frame = self
            .memory
            .last_mut()
            .expect("Should be at least one frame at this point");
        frame.insert(var.clone(), val);
    }

    fn incr_pc(&mut self) {
        let InstrAddr(pc) = self.pc;
        self.pc = InstrAddr(pc + 1);
    }

    fn pop_ret_addr(&mut self) -> InstrAddr {
        self.ret_addrs.pop().expect("Vm Return Stack Underflow!")
    }

    fn push_new_frame(&mut self) {
        self.memory.push(HashMap::new());
    }

    fn pop_mem_frame(&mut self) {
        self.memory.pop().expect("Memory Frame Stack Underflow!");
    }

    fn tos(&self) -> &Value {
        self.stack.last().expect("Stack underflow!")
    }

    fn run_intrinsic(&mut self, intr: &Intrinsic) {
        match intr {

            Intrinsic::EqInt => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VBool(x == y))
            }

            Intrinsic::EqBool => {
                let x = self.pop_bool();
                let y = self.pop_bool();
                self.push(Value::VBool(x == y))
            }

            Intrinsic::EqText => {
                let x = self.pop_string();
                let y = self.pop_string();
                self.push(Value::VBool(x == y))
            }

            Intrinsic::DbgInt => println!("{:?}", self.pop_int()),

            Intrinsic::DbgBool => println!("{:?}", self.pop_bool()),

            Intrinsic::DbgText => println!("{:?}", self.pop_string()),

            Intrinsic::Puts => println!("{}", self.pop()),

            Intrinsic::Here(loc) => println!("{}", loc),

            Intrinsic::Exit => self.running = false,
        }
    }

    fn call_direct(&mut self, InstrAddr(fn_addr): InstrAddr, _argc: u8) {
        // Store the return address above all the args.
        let InstrAddr(pc) = self.pc;
        self.ret_addrs.push(InstrAddr(pc + 1)); // Return to the NEXT instr.

        // Perform the jump.
        self.pc = InstrAddr(fn_addr - 1);
        self.push_new_frame();
    }

    fn step(&mut self, code: &[Instr], heap: &mut Heap, debug: bool) {
        let instr = &code[self.pc.0];

        if debug {
            eprintln!("--------------------");
            eprintln!("instr {} = {:?}", self.pc, instr);
            eprintln!("stack = {:?}", self.stack);

            eprint!(">>> ");
            io::stdout().flush().unwrap();
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).unwrap();
            if buf.to_uppercase().trim() == "Q" {
                self.running = false;
                eprintln!("EXITING...");
            }
        }

        match instr {
            Instr::Load(var) => self.load(var),

            Instr::Store(var) => self.store(var),

            Instr::Const(val) => self.push(val.clone()),

            Instr::Dup => {
                let tos = self.tos().clone();
                self.push(tos);
            }

            // [a, b, ...] -> [b, a, b, ...]
            Instr::Over => {
                let a = self.pop();
                let b = self.pop();
                self.push(b.clone());
                self.push(a);
                self.push(b);
            }

            // [a, b, c, ...] -> [c, a, b, ...]
            Instr::Rot => {
                let a = self.pop();
                let b = self.pop();
                let c = self.pop();
                self.push(c);
                self.push(a);
                self.push(b);
            }

            // [a, b, ...] -> [b, a, ...]
            Instr::Swap => {
                let a = self.pop();
                let b = self.pop();
                self.push(b);
                self.push(a);
            }

            Instr::Pop => {
                let _ = self.pop();
            }

            Instr::Add => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VInt(x + y));
            }

            Instr::Sub => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VInt(x - y));
            }

            Instr::Mul => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VInt(x * y));
            }

            Instr::Div => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VInt(x / y));
            }

            Instr::Neg => {
                let x = self.pop_int();
                self.push(Value::VInt(-x));
            }

            Instr::And => {
                let x = self.pop_bool();
                let y = self.pop_bool();
                self.push(Value::VBool(x && y));
            }

            Instr::Or => {
                let x = self.pop_bool();
                let y = self.pop_bool();
                self.push(Value::VBool(x || y));
            }

            Instr::Not => {
                let x = self.pop_bool();
                self.push(Value::VBool(!x));
            }

            Instr::Gt => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VBool(x > y));
            }

            Instr::Lt => {
                let x = self.pop_int();
                let y = self.pop_int();
                self.push(Value::VBool(x < y));
            }

            Instr::Concat => {
                let x = self.pop_string();
                let y = self.pop_string();
                self.push(Value::VString(x + &y));
            }

            Instr::Alloc(n) => {
                let idx = heap.alloc(*n);
                self.push(Value::VPtr(idx));
            }

            // BEFORE:
            // TOS -> [val: Value]
            //        [buf_start: Value::VPtr]
            //        [...]
            // AFTER:
            // TOS -> [buf_start: Value::VPtr]
            //        [...]
            Instr::MemWriteDirect(offset) => {
                let val = self.pop();
                let buf_start = self.pop_ptr();
                heap.mem[buf_start + offset] = val;
                self.push(Value::VPtr(buf_start));
            }

            Instr::MemReadDirect(offset) => {
                let buf_start = self.pop_ptr();
                let val = heap.mem[buf_start + offset].clone();
                self.push(val);
            }

            Instr::Nop => {}

            Instr::JmpIfFalse(InstrAddr(addr)) => {
                let cond = self.pop_bool();
                if !cond {
                    // We always incr pc, so goto idx - 1.
                    self.pc = InstrAddr(addr - 1)
                }
            }

            Instr::Jmp(InstrAddr(addr)) => {
                // We always incr pc, so goto idx - 1.
                self.pc = InstrAddr(addr - 1);
            }

            Instr::Intrinsic(intr) => self.run_intrinsic(intr),

            // Assume stack is set up properly beforehand.
            // Stack should look like this:
            //  | <the function's InstrAddr>   <-- TOS
            //  | <arg n>
            //  | <arg n-1>
            //  | ...
            //  | <arg 2>
            //  | <arg 1>
            // After running Lir.Call, stack should like like this:
            //  | <return address>
            //  | <arg n>
            //  | <arg n-1>
            //  | ...
            //  | <arg 2>
            //  | <arg 1>
            // Lir.Call argC -> do
            //   fnAddr <- popInstrAddr -- Get the function's entry address.
            //   callDirect fnAddr argC
            Instr::Call(argc) => {
                let fn_addr = self.pop_instr_addr();
                self.call_direct(fn_addr, *argc);
            }

            Instr::CallDirect(fn_addr, argc) => {
                self.call_direct(*fn_addr, *argc);
            }

            Instr::Ret => {
                let InstrAddr(ret_addr) = self.pop_ret_addr();
                self.pop_mem_frame();
                self.pc = InstrAddr(ret_addr - 1); // We always incr pc, so sub 1.
            }
        }
    }

    pub fn exec(&mut self, code: &[Instr], debug: bool) {
        let mut heap = Heap::new();
        while self.running {
            self.step(code, &mut heap, debug);
            self.incr_pc();
        }
    }
}
