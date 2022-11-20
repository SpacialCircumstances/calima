use crate::ast::Name;
use crate::ast::TopLevelBlock;
use crate::common::{ModuleId, ModuleName};
use crate::ir;
use crate::parsing::token::Span;
use crate::symbol_names::IText;
use crate::typechecker::environment::ClosedEnvironment;
use crate::typechecker::substitution::Substitution;
use crate::typechecker::ValueTypeContext;
use crate::types::Type;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub struct UntypedModuleData {
    pub(crate) name: ModuleName,
    pub(crate) ast: TopLevelBlock<Name<Span>, IText, Span>,
    pub(crate) dependencies: Vec<ModuleName>,
    pub(crate) path: PathBuf,
    pub(crate) id: ModuleId,
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
    pub(crate) lookup: HashMap<ModuleName, UntypedModule>,
}

pub struct TypedModuleData {
    pub(crate) name: ModuleName,
    pub(crate) path: PathBuf,
    pub(crate) deps: Vec<TypedModule>,
    pub(crate) ir_module: ir::Module,
    pub(crate) subst: Substitution<Type>,
    pub(crate) vtc: ValueTypeContext,
    pub(crate) env: ClosedEnvironment<Span>,
    pub(crate) id: ModuleId,
}

pub struct TypedModule(pub Rc<TypedModuleData>);

impl Clone for TypedModule {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct TypedModuleTree {
    pub(crate) main_module: TypedModule,
    pub(crate) lookup: HashMap<ModuleName, TypedModule>,
}
