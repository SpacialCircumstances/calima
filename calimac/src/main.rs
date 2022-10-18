use calima::{compile, CompilerArguments};
use clap::Parser;

#[derive(Parser)]
#[command(author = "SpacialCircumstances", version = "0.2.0")]
struct Options {
    #[arg(short, long, value_name = "Main file")]
    main_file: String,
    #[arg(short, long, value_name = "Module roots")]
    roots: Vec<String>,
    #[arg(short, long, value_name = "Project name")]
    project_name: Option<String>,
}

fn main() -> Result<(), ()> {
    let args = Options::parse();

    compile(CompilerArguments::new(&args.main_file, &args.roots, None))
}
