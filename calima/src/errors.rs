use std::error::Error;
use std::io::Write;
use crate::token::{Token, Location};
use crate::compiler::ModuleIdentifier;
use std::path::{PathBuf, Path};
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};
use codespan_reporting::diagnostic::{Diagnostic, Severity, Label};
use codespan_reporting::files::{SimpleFiles, Files};
use std::ops::Range;
use std::collections::HashMap;
use std::fs::read_to_string;

#[derive(Debug)]
pub enum CompilerError<'a> {
    GeneralError(Option<Box<dyn Error>>, String),
    ParserError(lalrpop_util::ParseError<Location, Token<'a>, crate::lexer::Error>, ModuleIdentifier, PathBuf),
    ImportError { importing_mod: ModuleIdentifier, importing_mod_path: PathBuf, imported: ModuleIdentifier, search_dirs: Vec<PathBuf> },
}

struct CompilerFile {
    path: PathBuf,
    content: String
}

struct CompilerFiles {
    file_map: HashMap<u32, CompilerFile>,
    file_id: u32
}

impl<'a> CompilerFiles {
    fn new() -> Self {
        CompilerFiles {
            file_map: HashMap::new(),
            file_id: 0
        }
    }

    fn add(&mut self, path: PathBuf) -> Option<u32> {
        let content = match read_to_string(&path) {
            Err(_) => return None,
            Ok(s) => s
        };
        let file = CompilerFile {
            path,
            content
        };
        let id = self.file_id;
        self.file_id += 1;
        self.file_map.insert(id, file);
        Some(id)
    }

    fn get(&'a self, id: u32) -> Option<&CompilerFile> {
        self.file_map.get(&id)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for CompilerFiles {
    type FileId = u32;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
        self.get(id).map(|f| f.path.display().to_string())
    }

    fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
        self.get(id).map(|f| f.content.as_str())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        unimplemented!()
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        unimplemented!()
    }
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