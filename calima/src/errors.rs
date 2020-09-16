use std::error::Error;
use std::io::Write;
use crate::token::{Token, Location, Span};
use crate::common::ModuleIdentifier;
use std::path::{PathBuf, Path};
use codespan_reporting::term::termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};
use codespan_reporting::diagnostic::{Diagnostic, Severity, Label};
use std::ops::Range;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::iter::once;
use lalrpop_util::ParseError;
use codespan_reporting::term::emit;

#[derive(Debug)]
pub enum CompilerError<'a> {
    GeneralError(Option<Box<dyn Error>>, String),
    ParserError(lalrpop_util::ParseError<Location, Token<'a>, crate::lexer::Error>, ModuleIdentifier, PathBuf),
    ImportError { importing_mod: ModuleIdentifier, importing_mod_path: PathBuf, location: Span, imported: ModuleIdentifier, search_dirs: Vec<PathBuf> },
}

struct CompilerFile {
    path: PathBuf,
    content: String,
    lines: Vec<usize>
}


impl CompilerFile {
    fn new(path: PathBuf, content: String) -> Self {
        let lines = once(0).chain(content.match_indices('\n').map(|(idx, _)| idx + 1)).collect();
        CompilerFile {
            path,
            content,
            lines
        }
    }
}

struct CompilerFiles {
    file_map: HashMap<u32, CompilerFile>,
    path_map: HashMap<PathBuf, u32>,
    file_id: u32
}

impl<'a> CompilerFiles {
    fn new() -> Self {
        CompilerFiles {
            file_map: HashMap::new(),
            path_map: HashMap::new(),
            file_id: 0
        }
    }

    fn add_or_get(&mut self, path: &Path) -> Option<u32> {
        match self.path_map.get(path) {
            Some(id) => Some(*id),
            None => {
                let content = match read_to_string(&path) {
                    Err(_) => return None,
                    Ok(s) => s
                };
                let file = CompilerFile::new(path.to_path_buf(), content);
                let id = self.file_id;
                self.file_id += 1;
                self.path_map.insert(path.to_path_buf(), id);
                self.file_map.insert(id, file);
                Some(id)
            }
        }
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
        self.get(id).map(|f| f.lines.binary_search(&byte_index).unwrap_or_else(|x| x - 1))
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        let file = self.get(id)?;
        let start = *file.lines.get(line_index).or(Some(&file.content.len()))?;
        let end = *file.lines.get(line_index + 1).or(Some(&file.content.len()))?;
        Some(start..end)
    }
}

#[derive(Debug)]
pub enum CompilerWarning {}

pub struct ErrorContext<'a> {
    errors: Vec<CompilerError<'a>>,
    warnings: Vec<CompilerWarning>,
    files: CompilerFiles
}

impl<'a> ErrorContext<'a> {
    pub fn new() -> Self {
        ErrorContext {
            warnings: Vec::new(),
            errors: Vec::new(),
            files: CompilerFiles::new()
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

        let mut stderr = StandardStream::stderr(ColorChoice::Auto);

        let mut diagnostics = Vec::new();
        let mut general_errors = Vec::new();

        for err in &self.errors {
            match err {
                CompilerError::GeneralError(source, descr) => general_errors.push((source, descr)),
                CompilerError::ParserError(parser_err, _, path) => {
                    let file_id = self.files.add_or_get(path).expect("Error loading file");
                    match parser_err {
                        ParseError::User { error } => {
                            //TODO: Location  info etc.
                            let e = Diagnostic::new(Severity::Error)
                                .with_message(format!("Parser Error: {}", error));
                            diagnostics.push(e);
                        },
                        ParseError::ExtraToken { token: (s, token, e) } => {
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser Error")
                                .with_labels(vec![
                                    Label::primary(file_id, s.pos..e.pos).with_message(format!("Extra token {} found", token))
                                ])
                                .with_notes(vec![ format!("Parser found unexpected token: {}", token) ]);
                            diagnostics.push(e);
                        },
                        ParseError::InvalidToken { location } => {
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser Error")
                                .with_labels(vec![
                                    Label::primary(file_id, location.pos..location.pos+1).with_message(format!("Invalid token found here"))
                                ])
                                .with_notes(vec![ format!("Parser found invalid token") ]);
                            diagnostics.push(e);
                        },
                        ParseError::UnrecognizedEOF { location, expected } => {
                            let expected = expected.join(", ");
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser error")
                                .with_labels(vec![
                                    Label::primary(file_id, location.pos-1..location.pos).with_message(format!("Unrecognized EOF, expected one of: {{{}}}", expected))
                                ])
                                .with_notes(vec![ "Parser encountered unexpected EOF".to_string() ]);
                            diagnostics.push(e);
                        },
                        ParseError::UnrecognizedToken { token: (s, token, e), expected } => {
                            let expected = expected.join(", ");
                            let e = Diagnostic::new(Severity::Error)
                                .with_message("Parser error")
                                .with_labels(vec![
                                    Label::primary(file_id, s.pos..e.pos).with_message(format!("Unrecognized token {}, expected one of: {{{}}}", token, expected))
                                ])
                                .with_notes(vec![ format!("Parser found unrecognized token {}", token) ]);
                            diagnostics.push(e);
                        }
                    }
                },
                CompilerError::ImportError { importing_mod, importing_mod_path, location, imported, search_dirs } => {
                    let file_id = self.files.add_or_get(importing_mod_path).expect("Error loading file");
                    let mut notes = vec![
                        format!("Importing module {} into {} failed.", imported, importing_mod),
                        format!("Reason: Module not found in search paths:"),
                    ];
                    notes.extend(search_dirs.iter().map(|p| {
                        match std::fs::canonicalize(p) {
                            Ok(p) => p.display().to_string(),
                            Err(_) => p.display().to_string()
                        }
                    }));
                    let e = Diagnostic::new(Severity::Error)
                        .with_message(format!("Error importing module {}", imported))
                        .with_labels(vec![
                            Label::primary(file_id, location.left.pos..location.right.pos).with_message(format!("Module {} not found", imported))
                        ])
                        .with_notes(notes);
                    diagnostics.push(e);
                }
            }
        }

        //Warnings should be added here as soon as we have them

        if !general_errors.is_empty() {
            stderr.set_color(&ColorSpec::new().set_fg(Some(Color::Red))).expect("Error setting output color");

            for (source, descr) in general_errors {
                match source {
                    None => writeln!(&mut stderr, "Error occured: {}", descr).expect("Error writing to stderr"),
                    Some(source) => {
                        writeln!(&mut stderr, "Error occured: {}", descr).expect("Error writing to stderr");
                        writeln!(&mut stderr, "Caused by: {}", source).expect("Error writing to stderr")
                    }
                }
            }

            stderr.reset().expect("Error resetting output color");
        }

        if !diagnostics.is_empty() {
            let config = codespan_reporting::term::Config::default();
            for di in diagnostics {
                emit(&mut stderr.lock(), &config, &self.files, &di).expect("Error writing to stderr");
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