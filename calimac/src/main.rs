use clap::Clap;
use calima::{compile, CompilerArguments};

#[derive(Clap)]
#[clap(version = "0.1.0", author = "SpacialCircumstances")]
struct Options {
    input_file: String,
    #[clap(short, long)]
    module_paths: Vec<String>
}

fn main() {
    let options: Options = Options::parse();
    let file_name = &options.input_file;
    let module_paths = options.module_paths.iter().map(AsRef::as_ref).collect();
    let args = CompilerArguments::new(file_name, module_paths);
    compile(args).expect("Compiler error");
}
