use crate::ast::TopLevelBlock;
use crate::ast_common::Name;
use crate::common::ModuleIdentifier;
use crate::parsing::token::Span;
use crate::symbol_names::SymbolName;
use crate::typechecker::env::ModuleEnvironment;
use crate::typechecker::substitution::Substitution;
use crate::typed_ast::TBlock;
use crate::types::Type;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub struct UntypedModuleData {
    pub(crate) name: ModuleIdentifier,
    pub(crate) ast: TopLevelBlock<Name<Span>, SymbolName, Span>,
    pub(crate) dependencies: Vec<UntypedModule>,
    pub(crate) path: PathBuf,
}

pub struct UntypedModule(pub Rc<UntypedModuleData>);

impl Clone for UntypedModule {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct UntypedModuleTree {
    pub(crate) search_dirs: Vec<PathBuf>,
    pub(crate) main_module: UntypedModule,
    pub(crate) lookup: HashMap<ModuleIdentifier, UntypedModule>,
}

pub struct TypedModuleData {
    pub(crate) name: ModuleIdentifier,
    pub(crate) path: PathBuf,
    pub(crate) deps: Vec<TypedModule>,
    pub(crate) ir_block: TBlock,
    pub(crate) subst: Substitution<Type>,
    pub(crate) env: Rc<ModuleEnvironment>,
}

pub struct TypedModule(pub Rc<TypedModuleData>);

impl Clone for TypedModule {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct TypedModuleTree {
    pub(crate) main_module: TypedModule,
    pub(crate) lookup: HashMap<ModuleIdentifier, TypedModule>,
}
