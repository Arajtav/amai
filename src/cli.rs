use clap::Parser;

#[derive(Debug, Parser)]
#[command(name = "amai", bin_name = "amai", about = "The Amai toolchain")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Parser)]
pub enum Command {
    Run {
        input: Option<String>,

        #[arg(short, long)]
        show_bytecode: bool,
    },
}
