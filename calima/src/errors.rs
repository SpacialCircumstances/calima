use crate::common::ModuleName;
use crate::parsing::token::{Location, Span, Token};
use crate::typechecker::TypeError;
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{
    Color, ColorChoice, ColorSpec, StandardStream, WriteColor,
};
use lalrpop_util::ParseError;
use std::collections::HashMap;
use std::error::Error;
use std::io::Write;
use std::iter::once;
use std::ops::Range;
use std::path::PathBuf;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MainFunctionErrorKind {
    Missing,
    SignatureWrong,
}

//Basically a copy of lalrpop::ParseError, but with formatted tokens to avoid lifetime issues
#[derive(Debug, Clone)]
pub enum ParserError {
    InvalidToken {
        location: Location,
    },
    UnrecognizedEOF {
        location: Location,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: (Location, String, Location),
        expected: Vec<String>,
    },

    ExtraToken {
        token: (Location, String, Location),
    },
    User {
        error: crate::parsing::lexer::Error,
    },
}

impl From<lalrpop_util::ParseError<Location, Token, crate::parsing::lexer::Error>> for ParserError {
    fn from(pe: ParseError<Location, Token, crate::parsing::lexer::Error>) -> Self {
        match pe {
            ParseError::InvalidToken { location } => ParserError::InvalidToken { location },
            ParseError::UnrecognizedEOF { location, expected } => {
                ParserError::UnrecognizedEOF { location, expected }
            }
            ParseError::UnrecognizedToken {
                token: (l1, t, l2),
                expected,
            } => ParserError::UnrecognizedToken {
                token: (l1, t.to_string(), l2),
                expected,
            },
            ParseError::ExtraToken { token: (l1, t, l2) } => ParserError::ExtraToken {
                token: (l1, t.to_string(), l2),
            },
            ParseError::User { error } => ParserError::User { error },
        }
    }
}

#[derive(Debug)]
pub enum CompilerError {
    GeneralError(Option<Box<dyn Error>>, String),
    ParserError(ParserError, ModuleName),
    ImportError {
        importing_mod: ModuleName,
        location: Span,
        imported: ModuleName,
        search_dirs: Vec<PathBuf>,
    },
    TypeError(TypeError<Span>, ModuleName),
    MainFunctionError(ModuleName, MainFunctionErrorKind),
}

struct CompilerFile {
    path: PathBuf,
    content: String,
    lines: Vec<usize>,
}

impl CompilerFile {
    fn new(path: PathBuf, content: String) -> Self {
        let lines = once(0)
            .chain(content.match_indices('\n').map(|(idx, _)| idx + 1))
            .collect();
        CompilerFile {
            path,
            content,
            lines,
        }
    }
}

struct CompilerFiles {
    file_map: HashMap<u32, CompilerFile>,
    path_map: HashMap<ModuleName, u32>,
    file_id: u32,
}

impl CompilerFiles {
    fn new() -> Self {
        CompilerFiles {
            file_map: HashMap::new(),
            path_map: HashMap::new(),
            file_id: 0,
        }
    }

    fn add_module(&mut self, module: &ModuleName, path: &PathBuf, code: String) -> u32 {
        let id = self.file_id;
        self.file_id += 1;
        let cf = CompilerFile::new(path.clone(), code);
        self.file_map.insert(id, cf);
        self.path_map.insert(module.clone(), id);
        id
    }

    fn get_module(&self, module: &ModuleName) -> Option<u32> {
        self.path_map.get(module).copied()
    }

    fn get(&self, id: u32) -> Option<&CompilerFile> {
        self.file_map.get(&id)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for CompilerFiles {
    type FileId = u32;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.get(id)
            .map(|f| f.path.display().to_string())
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.get(id)
            .map(|f| f.content.as_str())
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.get(id)
            .map(|f| f.lines.binary_search(&byte_index).unwrap_or_else(|x| x - 1))
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, codespan_reporting::files::Error> {
        let file = self
            .get(id)
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)?;
        let start = *file
            .lines
            .get(line_index)
            .or(Some(&file.content.len()))
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)?;
        let end = *file
            .lines
            .get(line_index + 1)
            .or(Some(&file.content.len()))
            .ok_or_else(|| codespan_reporting::files::Error::FileMissing)?;
        Ok(start..end)
    }
}

#[derive(Debug)]
pub enum CompilerWarning {}

pub struct ErrorContext {
    errors: Vec<CompilerError>,
    warnings: Vec<CompilerWarning>,
    files: CompilerFiles,
}

impl ErrorContext {
    pub fn new() -> Self {
        ErrorContext {
            warnings: Vec::new(),
            errors: Vec::new(),
            files: CompilerFiles::new(),
        }
    }

    pub fn add_file(&mut self, module: &ModuleName, path: &PathBuf, code: String) {
        self.files.add_module(module, path, code);
    }

    pub fn add_error(&mut self, err: CompilerError) {
        self.errors.push(err);
    }

    pub fn add_warning(&mut self, warning: CompilerWarning) {
        self.warnings.push(warning);
    }

    pub fn handle_errors(&mut self) -> Result<(), ()> {
        if self.errors.is_empty() && self.warnings.is_empty() {
            return Ok(());
        }

        let mut stderr = StandardStream::stderr(ColorChoice::Auto);

        let mut diagnostics = Vec::new();
        let mut general_errors = Vec::new();

        for err in &self.errors {
            match err {
                CompilerError::GeneralError(source, descr) => general_errors.push((source, descr)),
                CompilerError::ParserError(parser_err, module) => {
                    let file_id = self.files.get_module(module).expect("Error loading file");
                    match parser_err {
                        ParserError::User { error } => {
                            let message = format!("{}", error.kind);
                            let e = Diagnostic::new(Severity::Error)
                                .with_labels(vec![Label::primary(
                                    file_id,
                                    error.location.to_range(),
                                )
                                .with_message(&message)])
                                .with_message(&message);
                            diagnostics.push(e);
                        }
                        ParserError::ExtraToken {
                            token: (s, token, e),
                        } => {
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser Error")
                                .with_labels(vec![Label::primary(file_id, s.pos..e.pos + 1)
                                    .with_message(format!("Extra token {} found", token))])
                                .with_notes(vec![format!(
                                    "Parser found unexpected token: {}",
                                    token
                                )]);
                            diagnostics.push(e);
                        }
                        ParserError::InvalidToken { location } => {
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser Error")
                                .with_labels(vec![Label::primary(
                                    file_id,
                                    location.pos..location.pos + 1,
                                )
                                .with_message("Invalid token found here".to_string())])
                                .with_notes(vec!["Parser found invalid token".to_string()]);
                            diagnostics.push(e);
                        }
                        ParserError::UnrecognizedEOF { location, expected } => {
                            let expected = expected.join(", ");
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser error")
                                .with_labels(vec![Label::primary(
                                    file_id,
                                    location.pos - 1..location.pos,
                                )
                                .with_message(format!(
                                    "Unrecognized EOF, expected one of: {{{}}}",
                                    expected
                                ))])
                                .with_notes(vec!["Parser encountered unexpected EOF".to_string()]);
                            diagnostics.push(e);
                        }
                        ParserError::UnrecognizedToken {
                            token: (s, token, e),
                            expected,
                        } => {
                            let expected = expected.join(", ");
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser error")
                                .with_labels(vec![Label::primary(file_id, s.pos..e.pos + 1)
                                    .with_message(format!(
                                        "Unrecognized token {}, expected one of: {{{}}}",
                                        token, expected
                                    ))])
                                .with_notes(vec![format!(
                                    "Parser found unrecognized token {}",
                                    token
                                )]);
                            diagnostics.push(e);
                        }
                    }
                }
                CompilerError::ImportError {
                    importing_mod,
                    location,
                    imported,
                    search_dirs,
                    ..
                } => {
                    let file_id = self
                        .files
                        .get_module(importing_mod)
                        .expect("Error loading file");
                    let mut notes = vec![
                        format!(
                            "Importing module {} into {} failed.",
                            imported, importing_mod
                        ),
                        "Reason: Module not found in search paths:".to_string(),
                    ];
                    notes.extend(search_dirs.iter().map(|p| match std::fs::canonicalize(p) {
                        Ok(p) => p.display().to_string(),
                        Err(_) => p.display().to_string(),
                    }));
                    let e = Diagnostic::new(Severity::Error)
                        .with_message(format!("Error importing module {}", imported))
                        .with_labels(vec![Label::primary(file_id, location.to_range())
                            .with_message(format!("Module {} not found", imported))])
                        .with_notes(notes);
                    diagnostics.push(e);
                }
                CompilerError::TypeError(te, modname) => {
                    let file_id = self.files.get_module(modname).expect("Error loading file");
                    let e = Diagnostic::new(Severity::Error)
                        .with_message(format!("Type error in module {}", modname))
                        .with_labels(vec![Label::primary(file_id, te.location.to_range())
                            .with_message(te.message.clone())]);
                    diagnostics.push(e);
                }
                CompilerError::MainFunctionError(name, err) => {
                    let message = match err {
                        MainFunctionErrorKind::Missing => {
                            format!("Missing or non-public main function in module {}", name)
                        }
                        MainFunctionErrorKind::SignatureWrong => format!(
                            "Main function in module {} must have a signature of Unit -> Unit",
                            name
                        ),
                    };
                    diagnostics.push(Diagnostic::new(Severity::Error).with_message(message));
                }
            }
        }

        //Warnings should be added here as soon as we have them

        if !general_errors.is_empty() {
            stderr
                .set_color(&ColorSpec::new().set_fg(Some(Color::Red)))
                .expect("Error setting output color");

            for (source, descr) in general_errors {
                match source {
                    None => writeln!(&mut stderr, "Error occured: {}", descr)
                        .expect("Error writing to stderr"),
                    Some(source) => {
                        writeln!(&mut stderr, "Error occured: {}", descr)
                            .expect("Error writing to stderr");
                        writeln!(&mut stderr, "Caused by: {}", source)
                            .expect("Error writing to stderr")
                    }
                }
            }

            stderr.reset().expect("Error resetting output color");
        }

        if !diagnostics.is_empty() {
            let config = codespan_reporting::term::Config::default();
            for di in diagnostics {
                emit(&mut stderr.lock(), &config, &self.files, &di)
                    .expect("Error writing to stderr");
            }
        }

        if !self.errors.is_empty() {
            self.errors.clear();
            Err(())
        } else {
            Ok(())
        }
    }
}
