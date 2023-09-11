use std::sync::Arc;
use std::io::Write;
use std::io::BufReader;
use std::io::BufWriter;
use std::fs::File;
use clap::Parser;

mod types;
mod module;
mod args;
mod exec;
mod op;
mod utils;
mod asm;
//pub mod native;

fn main() {
    let cli = args::MainArgs::parse();
    match cli.command {
        args::Commands::VerifyHeader {path} => {
            println!("{:#?}", module::open(&path));
        },
        args::Commands::ExecNoArg {path} => {
            let mp = Arc::new(module::ModulePool::new());
            mp.load(&path).map_err(|e| {
                eprintln!("Failed to open module, {e:}")
            }).unwrap();
            let ep = cli.entry_pt.unwrap_or("main".to_string());
            let mut wt = exec::WorkerThread::new(&path, &ep, cli.refctl, mp.clone());
            let res = wt.begin().map_err(|e| {
                eprintln!("{}", e);
                wt.print_stack_trace();
            });
            match res {
                Ok(()) => {}
                Err(_) => {
                    eprintln!("VM Panicked on error.");
                }
            }
        },
        args::Commands::Assemble {path, output} => {
            let file = File::open(&path).expect("Failed to open file");
            let out = File::create(output.unwrap_or(path[0..path.len()-2].to_owned() + ".fr")).expect("Failed to open file");
            let mut b_in = BufReader::new(file);
            let mut b_out = BufWriter::new(out);
            let mut line = String::new();
            let mut lno = 0;
            match asm::asm(&mut b_out, &mut b_in, &mut line, &mut lno, cli.debug) {
                Ok(()) => {println!("Done.");},
                Err(s) => {
                    eprintln!("{}", s);
                    eprintln!("Error occured at,\n{lno}\t{line} in {path}");
                }
            }
            b_out.flush().expect("Failed to flush contents to file.");
        }
    }
}
