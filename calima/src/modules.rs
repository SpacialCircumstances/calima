use crate::ast::TopLevelBlock;
use crate::common::ModuleIdentifier;
use crate::parsing::token::Span;
use crate::typechecker::substitution::Substitution;
use crate::typed_ast::TBlock;
use crate::types::{Exports, Type};
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub struct UntypedModuleData<'input> {
    pub(crate) name: ModuleIdentifier,
    pub(crate) ast: TopLevelBlock<'input, Span>,
    pub(crate) dependencies: Vec<UntypedModule<'input>>,
    pub(crate) path: PathBuf,
}

pub struct UntypedModule<'input>(pub Rc<UntypedModuleData<'input>>);

impl<'input> Clone for UntypedModule<'input> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct UntypedModuleTree<'input> {
    pub(crate) search_dirs: Vec<PathBuf>,
    pub(crate) main_module: UntypedModule<'input>,
    pub(crate) lookup: HashMap<ModuleIdentifier, UntypedModule<'input>>,
}

pub struct TypedModuleData<'input> {
    pub(crate) name: ModuleIdentifier,
    pub(crate) path: PathBuf,
    pub(crate) deps: Vec<TypedModule<'input>>,
    pub(crate) ir_block: TBlock<'input>,
    pub(crate) subst: Substitution<Type>,
    pub(crate) exports: Exports<'input>,
}

pub struct TypedModule<'input>(pub Rc<TypedModuleData<'input>>);

impl<'input> Clone for TypedModule<'input> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct TypedModuleTree<'input> {
    pub(crate) main_module: TypedModule<'input>,
    pub(crate) lookup: HashMap<ModuleIdentifier, TypedModule<'input>>,
}
