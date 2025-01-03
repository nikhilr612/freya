//! A simple register-based process virtual machine written in Rust.

use clap::Parser;
use log::{debug, error, info, trace, warn};
use std::fs::File;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitCode;
use std::sync::Arc;
use std::sync::RwLock;

mod args;
mod core;
mod emit;
mod multi;
mod utils;

use core::exec;
use core::module;

pub mod native;

const ENTRY_POINT: &str = "main";

fn assemble_one(path: &PathBuf, outpath: &PathBuf, debug: bool) -> Result<(), String> {
    // Almost gave in to the generics-mania here...

    let file = File::open(path).map_err(|e| {
        format!(
            "Failed to open input file {},\n\tdetail: {e}",
            path.display()
        )
    })?;

    if let Some(p) = outpath.parent() {
        std::fs::create_dir_all(p).map_err(|e| {
            format!(
                "Failed to create output directory {},\n\tdetail: {e}",
                p.display()
            )
        })?;
    }

    let out = File::create(outpath).map_err(|e| {
        format!(
            "Failed to create output file {},\n\tdetail: {e}",
            outpath.display()
        )
    })?;

    let mut b_in = BufReader::new(file);
    let mut b_out = BufWriter::new(out);
    let mut line = String::new();
    let mut lno = 0;

    match emit::asm(&mut b_out, &mut b_in, &mut line, &mut lno, debug) {
        Ok(()) => {
            info!("Done with {}", path.display());
        }
        Err(s) => {
            return Err(format!(
                "{s}\nError occured at line {lno},\n{lno}| {line}\tin {}",
                path.display()
            ));
        }
    }

    b_out.flush().map_err(|e| {
        format!(
            "Failed to flush contents to file {},\n\tdetail: {e}",
            outpath.display()
        )
    })?;
    Ok(())
}

fn resolve_glob(path: &str, output: Option<String>) -> Vec<(PathBuf, PathBuf)> {
    let it = glob::glob(path)
        .expect("Could not read glob from input.")
        .filter_map(Result::ok);

    let v: Vec<_> = match output {
        None => it
            .map(|p| {
                let p2 = p.with_extension("fr");
                (p, p2)
            })
            .collect(),
        Some(dir) => it
            .map(|p| {
                if p.is_absolute() {
                    warn!(
                        "{} is an absolute path, so output directory is ignored.",
                        p.display()
                    );
                    let p3 = p.with_extension("fr");
                    (p, p3)
                } else {
                    let mut p3 = PathBuf::from(&dir);
                    p3.push(p.as_path());
                    p3.set_extension("fr");
                    (p, p3)
                }
            })
            .collect(),
    };
    v
}

fn assemble_many(path: &str, output: Option<String>, debug: bool) -> ExitCode {
    let pairs = resolve_glob(path, output);

    if pairs.is_empty() {
        error!("No files found.");
        return ExitCode::FAILURE;
    }

    trace!("Found files.");
    for (a, b) in &pairs {
        debug!("{} => {}", a.display(), b.display());
    }

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

#[allow(clippy::too_many_lines)]
fn main() -> ExitCode {
    pretty_env_logger::init();
    let cli = args::MainArgs::parse();

    match cli.command {
        args::Commands::VerifyHeader { path } => {
            println!("{:#?}", module::open(&path));
        }
        args::Commands::Exec {
            filepath,
            pathlist,
            nlibpath,
            cmdargs,
        } => {
            trace!("Recevied cmdargs: {cmdargs:#?}");
            let to_pass = core::list_from_strings(cmdargs.into_iter());

            let mut nifp = native::InterfacePool::default();
            for path in nlibpath {
                if let Some((a, b)) = path.split_once('=') {
                    nifp.add_path(a, b);
                } else {
                    eprintln!("WARNING: {path} is not a valid key-value pair for specifying libraries. Expect [name]=[path] format.");
                }
            }

            let nifp = Arc::new(RwLock::new(nifp));

            let mut mp = module::ModulePool::new();
            for path in pathlist {
                mp.add_path(Path::new(&path));
            }
            mp.add_path(
                std::env::current_dir()
                    .expect("Failed to read current working directory path")
                    .as_path(),
            );

            if let Err(e) = mp.load(&filepath) {
                error!("Failed to load module {filepath} into the module pool.");
                return ExitCode::FAILURE;
            }

            let mp = Arc::new(mp);

            let ep = cli.entry_pt.unwrap_or(ENTRY_POINT.to_string());

            let mut wt = exec::WorkerThread::with_args(
                &filepath,
                &ep,
                cli.refctl,
                mp.clone(),
                nifp.clone(),
                std::iter::once(to_pass),
            );

            if let Err(e) = wt.begin() {
                eprintln!("{e}");
                wt.print_stack_trace();
                error!("VM Panicked on error.");
                return ExitCode::FAILURE;
            }
        }
        args::Commands::Assemble {
            path,
            output,
            is_glob,
        } => {
            if is_glob {
                return assemble_many(&path, output, cli.debug);
            }

            let path = PathBuf::from(path);
            let outpath = output.map_or_else(|| path.with_extension("fr"), PathBuf::from);
            assemble_one(&path, &outpath, cli.debug).unwrap_or_else(|e| {
                eprintln!("{e}");
            });
        }
        args::Commands::MonoCompile { path, output } => {
            let output =
                output.map_or_else(|| Path::new(&path).with_extension("fr"), PathBuf::from);
            trace!("Input: {path}, Output: {}", output.display());
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
                }
                Ok(cu) => {
                    debug!("CU:\n{cu:#?}");
                    if let Err(e) = cu.finish(output) {
                        error!("Failed to finish code generation.");
                        eprintln!("{e}");
                        return ExitCode::FAILURE;
                    }
                    info!("Sucessfully completed codegen.");
                }
            }
        }
    }
    ExitCode::SUCCESS
}
