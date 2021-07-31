use std::env;
use std::path::PathBuf;

mod instr;
mod vm;

use instr::Mod;
use vm::Vm;

fn main() {
    let file_name: PathBuf = env::args().nth(1).expect("Must provide path!").into();
    let m = Mod::from_file(&file_name);
    let mut vm = Vm::new();
    // vm.debug = true;
    vm.exec(&m.instrs);
}
