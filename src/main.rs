//! A simple register-based process virtual machine written in Rust.

use log::{debug, warn, error, info, trace};
use std::path::PathBuf;
use std::sync::RwLock;
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
mod multi;

use core::module;
use core::exec;

pub mod native;

// TODO: Remove horrific artifact of the past.
macro_rules! on_error_exit_gracefully {
    ($res: expr, $ev: ident,$body: block) => {
        match $res {
            Ok(()) => (),
            Err($ev) => {
                $body
                eprintln!("VM Panicked on error.");
                return ExitCode::FAILURE;
            }
        }
    };
}

fn assemble_one(path: &PathBuf, outpath: &PathBuf, debug: bool) -> Result<(), String> {
    // Almost gave in to the generics-mania here...

    let file = File::open(path)
                .map_err(|e| format!("Failed to open input file {},\n\tdetail: {e}", path.display()))?;

    if let Some(p) = outpath.parent() {
        std::fs::create_dir_all(p)
            .map_err(|e| format!("Failed to create output directory {},\n\tdetail: {e}", p.display()))?;
    }

    let out = File::create(outpath)
                .map_err(|e| format!("Failed to create output file {},\n\tdetail: {e}", outpath.display()))?;

    let mut b_in = BufReader::new(file);
    let mut b_out = BufWriter::new(out);
    let mut line = String::new();
    let mut lno = 0;

    match emit::asm(&mut b_out, &mut b_in, &mut line, &mut lno, debug) {
        Ok(()) => {
            info!("Done with {}", path.display());
        },
        Err(s) => {
            return Err(format!("{s}\nError occured at line {lno},\n{lno}| {line}\tin {}", path.display()));
        }
    }

    b_out.flush().map_err(|e| format!("Failed to flush contents to file {},\n\tdetail: {e}", outpath.display()))?;
    Ok(())
}

fn resolve_glob(path: &str, output: Option<&String>) -> Vec<(PathBuf, PathBuf)> {
    let it = glob::glob(path)
       .expect("Could not read glob from input.")
       .filter_map(|s| s.ok());

    let v: Vec<_> = match output {
        None => 
            it.map(|p| {
                let p2 = p.with_extension("fr");
                (p, p2)
            })
            .collect(),
        Some(dir) => {
            it.map(|p| {
                if p.is_absolute() {
                    warn!("{} is an absolute path, so output directory is ignored.", p.display());
                    let p3 = p.with_extension("fr");
                    (p, p3)
                } else {
                    let mut p3 = PathBuf::from(dir);
                    p3.push(p.as_path());
                    p3.set_extension("fr");
                    (p, p3)
                }
            })
            .collect()
        }
    };
    v
}

fn assemble_many(path: String, output: Option<String>, debug: bool) -> ExitCode {
    let pairs = resolve_glob(&path, output.as_ref());

    if pairs.is_empty() {
        error!("No files found.");
        return ExitCode::FAILURE;
    }

    trace!("Found files.");
    pairs.iter().for_each(|(a,b)| {
        debug!("{} => {}", a.display(), b.display());
    });

    let mut ret = ExitCode::SUCCESS;

    for result in multi::apply_parallel("assembler", pairs, move |(p, o)| {
        assemble_one(&p, &o, debug)
    }) {
        if let Err(e) = result {
            error!("{e}\n");
            ret = ExitCode::FAILURE;
        }
    }

    ret
}

fn main() -> ExitCode {
    pretty_env_logger::init();
    let cli = args::MainArgs::parse();

    match cli.command {
        args::Commands::VerifyHeader {path} => {
            println!("{:#?}", module::open(&path));
        },
        args::Commands::Exec {filepath, pathlist, nlibpath, cmdargs} => {
            trace!("Recevied cmdargs: {cmdargs:#?}");
            let to_pass = core::list_from_strings(cmdargs.into_iter());

            let mut nifp = native::InterfacePool::default();
            for path in nlibpath {
                if let Some((a,b)) = path.split_once('=') { nifp.add_path(a, b) }
                else { eprintln!("WARNING: {path} is not a valid key-value pair for specifying libraries. Expect [name]=[path] format."); }
            }

            let nifp = Arc::new(RwLock::new(nifp));

            let mut mp = module::ModulePool::new();
            for path in pathlist {
                mp.add_path(Path::new(&path));
            }
            mp.add_path(std::env::current_dir().expect("Failed to read current working directory path").as_path());
            
            let res = mp.load(&filepath);
            on_error_exit_gracefully!(res, e, {eprintln!("{e}");});
            let mp = Arc::new(mp);

            let ep = cli.entry_pt.unwrap_or("main".to_string());
            
            let mut wt = exec::WorkerThread::
                        with_args(&filepath, &ep, 
                            cli.refctl, mp.clone(), nifp.clone(),
                            std::iter::once(to_pass));

            let res = wt.begin();
            on_error_exit_gracefully!(res, e, {
                eprintln!("{}", e);
                wt.print_stack_trace();
            });
        },
        args::Commands::Assemble {path, output, is_glob} => {
            if is_glob {
                return assemble_many(path, output, cli.debug);
            } else {
                let path = PathBuf::from(path);
                let outpath = output.map(PathBuf::from).unwrap_or_else(|| path.with_extension("fr"));
                assemble_one(&path, &outpath, cli.debug).unwrap_or_else(|e| {
                    eprintln!("{e}");
                });
            }
        },
        args::Commands::MonoCompile { path } => {
            let file = File::open(path).expect("Failed to open file");
            let mut reader = BufReader::new(file);
            let ast = match emit::parse(&mut reader) {
                Ok(a) => a,
                Err(e) => {
                    error!("{:?}", e);
                    error!("Parsing failed.");
                    return ExitCode::FAILURE;
                }
            };
            debug!("AST:\n{ast:#?}");
            trace!("Walking AST..");
            match emit::walk(ast) {
                Err(e) => {
                    error!("Failed to walk AST.");
                    eprintln!("{e:#?}");
                    return ExitCode::FAILURE;
                },
                Ok(cu) => {
                    debug!("CU:\n{cu:#?}");
                }
            }
        }
    }
    ExitCode::SUCCESS
}
