use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::{fs, io};
use structopt::StructOpt;

mod instr;
mod vm;

use instr::Mod;
use vm::Vm;

#[derive(Debug, StructOpt)]
struct Opts {
    #[structopt(name = "FILE", parse(from_os_str))]
    file_name: Option<PathBuf>,

    #[structopt(short = "T", long = "trace-vm")]
    debug: bool,
}

fn main() {
    let opts = Opts::from_args();
    let reader: Box<dyn BufRead> = match opts.file_name {
        Some(file_name) => Box::new(BufReader::new(
            fs::File::open(file_name).expect("file doesnt exist"),
        )),
        None => Box::new(BufReader::new(io::stdin())),
    };
    let m = Mod::new(reader);
    let mut vm = Vm::new();
    vm.exec(&m.instrs, opts.debug);
}
