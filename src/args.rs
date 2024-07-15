use clap::builder::Styles;
use clap::builder::styling::AnsiColor;
use clap::ValueEnum;
use clap::{Parser, Subcommand};

const STYLE: Styles = Styles::styled()
        .header(AnsiColor::BrightMagenta.on_default().bold().underline())
        .usage(AnsiColor::Yellow.on_default())
        .literal(AnsiColor::Blue.on_default().bold())
        .placeholder(AnsiColor::Green.on_default());

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
#[command(author, version, about, long_about = None, styles = STYLE)]
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
	#[clap(visible_alias = "x")]
	Exec {
		/// Path to the file which needs to be executed.
		filepath: String,
		#[arg(short, long)]
		/// List of directories to include in search path for resolving modules.
		pathlist: Vec<String>,
		#[arg(short, long)]
		/// List of native libraries that should be loaded on demand.
		nlibpath: Vec<String>,
		#[arg(trailing_var_arg = true, allow_hyphen_values = true)]
		/// Command line args passed to main.
		cmdargs: Vec<String>
	},
	#[clap(visible_alias = "asm")]
	/// Assemble a source file to bytecode. If debug flag is set, line numbers are emitted.
	Assemble { 
		/// Path to the assembly file, or a glob matching assembly files (requires --all flag)
		path: String, 
		/// Path to the output file (or directory).
		output: Option<String>,
		#[arg(short = 'a', long = "all")]
		/// Flag to be set if input path is a glob matching assembly files.
		is_glob: bool
	},
	/// Emit bytecode for a single source file.
	#[clap(alias = "mc")]
	MonoCompile {
		/// Path to the source file.
		path: String,
		/// Path to the output file.
		output: Option<String>
	}
}