use clap::Clap;
use calima::compile;

#[derive(Clap)]
#[clap(version = "0.1.0", author = "SpacialCircumstances")]
struct Options {
    input_file: String
}

fn main() {
    let options: Options = Options::parse();
    let file_name = &options.input_file;
    compile(file_name).expect("Compiler error");
}
