use clap::ValueEnum;
use clap::{Parser, Subcommand};

#[derive(Debug, Clone)]
#[repr(u8)]
#[derive(ValueEnum)]
pub enum RefPolicy {
	/// All reference counting operations become no-op.
	Inactive = 0,
	/// Invalid referencing will result in warnings, but are otherwise no-ops.
	WarnOnly = 1,
	/// Emit hard error on invalid referencing.
	Strict	 = 2
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct MainArgs {
	#[command(subcommand)]
	/// What subcommand to execute
	pub command: Commands,
	/// Flag to indicate if debug mode must be active.
	#[arg(short, long)]
	pub debug: bool,
	/// Ref-counting policiy adopted during execution.
	#[arg(short, long, default_value = "strict")]
	pub refctl: RefPolicy,
	/// Specify the entry-point of the program, which must be a function with atmost one argument.
	#[arg(short, long)]
	pub entry_pt: Option<String>
}

#[derive(Subcommand)]
#[derive(Debug)]
pub enum Commands {
	/// Check if the provided module file header is malformed.
	#[clap(alias = "vh")]
	VerifyHeader { path: String },
	/// Load and execute the given file.
	#[clap(alias = "x")]
	ExecNoArg {
		/// Path to the file which needs to be executed.
		filepath: String,
		#[arg(short, long)]
		/// List of directories to include in search path for resolving modules.
		pathlist: Vec<String>
	},
	#[clap(alias = "asm")]
	/// Assemble a source file to bytecode. If debug flag is set, line numbers are emitted.
	Assemble { 
		/// Path to the assembly file
		path: String, 
		/// Path to the output file.
		output: Option<String>
	},
	/// Emit bytecode for a single source file 
	MonoCompile {
		/// Path to the source file.
		path: String
	}
}