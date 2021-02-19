use calima::{compile, CompilerArguments};
use clap::Clap;

#[derive(Clap)]
#[clap(version = "0.1.0", author = "SpacialCircumstances")]
struct Options {
    input_file: String,
    #[clap(short, long)]
    module_paths: Vec<String>,
    #[clap(short, long)]
    project_name: Option<String>,
}

fn main() -> Result<(), ()> {
    let options: Options = Options::parse();
    let file_name = &options.input_file;
    let args = CompilerArguments::new(
        file_name,
        &options.module_paths,
        options.project_name.as_ref().map(|s| s.as_str()),
    );
    compile(args)
}
