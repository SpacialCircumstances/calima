use calima::compile;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author = "SpacialCircumstances", version = "0.2.0")]
struct Options {
    main_file: PathBuf,
    #[arg(short, long, value_name = "Module paths")]
    module_paths: Vec<PathBuf>,
    #[arg(short, long, value_name = "Output file")]
    output_file: Option<PathBuf>,
}

fn main() -> Result<(), ()> {
    let args = Options::parse();

    compile(&args.main_file, &args.module_paths, &args.output_file)
}
