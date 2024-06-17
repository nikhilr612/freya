//! A simple register-based process virtual machine written in Rust.

use std::path::Path;
use std::sync::Arc;
use std::io::Write;
use std::io::BufReader;
use std::io::BufWriter;
use std::fs::File;
use std::process::ExitCode;
use clap::Parser;

mod core;
mod args;
mod utils;
mod emit;

use core::module;
use core::exec;

pub mod native;

macro_rules! on_error_exit_gracefully {
    ($res: ident, $ev: ident,$body: block) => {
        match $res {
            Ok(()) => {},
            Err($ev) => {
                $body
                eprintln!("VM Panicked on error.");
                return ExitCode::FAILURE;
            }
        }
    };
}

fn main() -> ExitCode {
    let cli = args::MainArgs::parse();
    match cli.command {
        args::Commands::VerifyHeader {path} => {
            println!("{:#?}", module::open(&path));
        },
        args::Commands::ExecNoArg {filepath, pathlist} => {
            let mut nifp = native::InterfacePool::default();
            let mut mp = module::ModulePool::new();
            for path in pathlist {
                mp.add_path(Path::new(&path));
            }
            mp.add_path(std::env::current_dir().expect("Failed to read current working directory path").as_path());
            
            let res = mp.load(&filepath).map_err(|e| {
                eprintln!("{e:}")
            });
            on_error_exit_gracefully!(res, e, {eprintln!("{e:?}");});
            let mp = Arc::new(mp);
            let ep = cli.entry_pt.unwrap_or("main".to_string());
            let mut wt = exec::WorkerThread::new(&filepath, &ep, cli.refctl, mp.clone());
            let res = wt.begin();
            on_error_exit_gracefully!(res, e, {
                eprintln!("{}", e);
                wt.print_stack_trace();
            });
        },
        args::Commands::Assemble {path, output} => {
            let file = File::open(&path).expect("Failed to open file");
            let out = File::create(output.unwrap_or(path[0..path.len()-2].to_owned() + ".fr")).expect("Failed to open file");
            let mut b_in = BufReader::new(file);
            let mut b_out = BufWriter::new(out);
            let mut line = String::new();
            let mut lno = 0;
            match emit::asm(&mut b_out, &mut b_in, &mut line, &mut lno, cli.debug) {
                Ok(()) => {println!("Done.");},
                Err(s) => {
                    eprintln!("{}", s);
                    eprintln!("Error occured at,\n{lno}\t{line} in {path}");
                }
            }
            b_out.flush().expect("Failed to flush contents to file.");
        }
    }
    ExitCode::SUCCESS
}
