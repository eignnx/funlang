use std::fs;
use std::io::BufReader;
use std::path::PathBuf;
use structopt::StructOpt;

mod instr;
mod vm;

use instr::Mod;
use vm::Vm;

#[derive(Debug, StructOpt)]
struct Opts {
    #[structopt(name = "BYTE_CODE_FILE", parse(from_os_str))]
    bc_file_path: PathBuf,

    #[structopt(short = "T", long = "trace-vm")]
    debug: bool,
}

fn main() {
    let opts = Opts::from_args();
    let bc_file = fs::File::open(opts.bc_file_path).unwrap();
    let reader = BufReader::new(bc_file);
    let m = Mod::new(reader);
    let mut vm = Vm::new();
    vm.exec(&m.instrs, opts.debug);
}
