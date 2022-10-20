use calima::{compile, CompilerArguments};
use clap::Parser;

#[derive(Parser)]
#[command(author = "SpacialCircumstances", version = "0.2.0")]
struct Options {
    main_file: String,
    #[arg(short, long, value_name = "Module paths")]
    module_paths: Vec<String>,
    #[arg(short, long, value_name = "Output name")]
    output_name: Option<String>,
}

fn main() -> Result<(), ()> {
    let args = Options::parse();

    compile(CompilerArguments::new(
        &args.main_file,
        &args.roots,
        &args.output_name,
    ))
}
