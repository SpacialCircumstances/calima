use std::error::Error;
use std::io::Write;
use crate::token::{Token, Location};
use crate::compiler::ModuleIdentifier;
use std::path::PathBuf;
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};
use codespan_reporting::diagnostic::{Diagnostic, Severity};

#[derive(Debug)]
pub enum CompilerError<'a> {
    GeneralError(Option<Box<dyn Error>>, String),
    ParserError(lalrpop_util::ParseError<Location, Token<'a>, crate::lexer::Error>, ModuleIdentifier, PathBuf),
    ImportError { importing_mod: ModuleIdentifier, importing_mod_path: PathBuf, imported: ModuleIdentifier, search_dirs: Vec<PathBuf> },
}

#[derive(Debug)]
pub enum CompilerWarning {}

pub struct ErrorContext<'a> {
    errors: Vec<CompilerError<'a>>,
    warnings: Vec<CompilerWarning>,
}

impl<'a> ErrorContext<'a> {
    pub fn new() -> Self {
        ErrorContext {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn add_error(&mut self, err: CompilerError<'a>) {
        self.errors.push(err);
    }

    pub fn add_warning(&mut self, warning: CompilerWarning) {
        self.warnings.push(warning);
    }

    pub fn handle_errors(&mut self) -> Result<(), ()> {
        if self.errors.is_empty() && self.warnings.is_empty() {
            return Ok(())
        }

        let mut stdout = StandardStream::stdout(ColorChoice::Auto);

        //First, print the general errors.
        let mut general_errors = Vec::new();

        for err in &self.errors {
            match err {
                CompilerError::GeneralError(source, descr) => general_errors.push((source, descr)),
                CompilerError::ParserError(parser_err, module, path) => {
                },
                CompilerError::ImportError { importing_mod, importing_mod_path, imported, search_dirs } => {

                }
            }
        }

        if !general_errors.is_empty() {
            stdout.set_color(&ColorSpec::new().set_fg(Some(Color::Red))).expect("Error setting output color");

            for (source, descr) in general_errors {
                match source {
                    None => writeln!(&mut stdout, "Error occured: {}", descr).expect("Error writing to stdout"),
                    Some(source) => {
                        writeln!(&mut stdout, "Error occured: {}", descr).expect("Error writing to stdout");
                        writeln!(&mut stdout, "Caused by: {}", source).expect("Error writing to stdout")
                    }
                }
            }

            stdout.reset().expect("Error resetting output color");
        }

        if !self.errors.is_empty() {
            self.errors.clear();
            Err(())
        } else {
            Ok(())
        }
    }
}