use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct MainArgs {
	#[command(subcommand)]
	/// What subcommand to execute
	pub command: Commands,
	/// Flag to indicate if debug mode must be active.
	#[arg(short, long)]
	pub debug: bool,
	/// Indicate whether reference counting should be active and in what mode; 
	/// 0 - reference counting is disabled, 
	/// 1 - reference counting is enabled, but violations such as multiple mutable aliasing result in warnings
	/// 2 - reference counting is enabled, and all violations result in panics.
	/// any other value - same as 1.
	#[arg(short, long, default_value_t = 1)]
	pub refctl: u8,
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
	#[clap(alias = "ena")]
	ExecNoArg {path: String},
	#[clap(alias = "asm")]
	/// Assemble a source file to bytecode. If debug flag is set, then line numbers are emitted.
	Assemble { 
		/// Path to the assembly file
		path: String, 
		/// Path to the output file.
		output: Option<String>
	},
}