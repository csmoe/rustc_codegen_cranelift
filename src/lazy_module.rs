use std::collections::HashSet;

use cranelift_codegen::binemit::{NullStackMapSink, NullTrapSink};
use cranelift_codegen::ir::Function;
use cranelift_codegen::Context;
use cranelift_module::{
    DataContext, DataId, FuncId, Module, ModuleCompiledFunction, ModuleError, ModuleResult,
};

/// A wrapper around a [`Module`] which compiles [`Function`]s only once
/// [`finalize`](LazyModule::finalize) is called.
///
/// This can be used to for example codegen cranelift ir on the main thread and then compile all
/// functions of a codegen unit on a background thread by calling [`finalize`](LazyModule::finalize).
pub(crate) struct LazyModule<M: Module> {
    module: M,
    defined_functions: HashSet<FuncId>,
    functions: Vec<(FuncId, Function)>,
    eager: bool,
}

impl<M: Module> LazyModule<M> {
    pub(crate) fn new(module: M) -> Self {
        LazyModule {
            module,
            defined_functions: HashSet::new(),
            functions: Vec::new(),
            eager: false,
        }
    }

    /// Alternative constructor which forces function compilation to happen eagerly.
    ///
    /// This is useful during debugging by allowing all compilation errors to be reported at the
    /// point of defining the function rather than during `finalize`.
    pub(crate) fn new_eager(module: M) -> Self {
        LazyModule { module, defined_functions: HashSet::new(), functions: Vec::new(), eager: true }
    }

    pub(crate) fn finalize(self) -> ModuleResult<M> {
        // Reduce memory usage by eagerly deallocating unnecessary data.
        drop(self.defined_functions);

        let mut module = self.module;

        let mut ctx = Context::new();
        for (func_id, func) in self.functions {
            ctx.clear();
            ctx.func = func;
            module.define_function(
                func_id,
                &mut ctx,
                &mut NullTrapSink {},
                &mut NullStackMapSink {},
            )?;
        }

        Ok(module)
    }
}

impl<M: Module> Module for LazyModule<M> {
    fn isa(&self) -> &dyn cranelift_codegen::isa::TargetIsa {
        self.module.isa()
    }

    fn declarations(&self) -> &cranelift_module::ModuleDeclarations {
        self.module.declarations()
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        signature: &cranelift_codegen::ir::Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        self.module.declare_function(name, linkage, signature)
    }

    fn declare_anonymous_function(
        &mut self,
        signature: &cranelift_codegen::ir::Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        self.module.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<DataId> {
        self.module.declare_data(name, linkage, writable, tls)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<DataId> {
        self.module.declare_anonymous_data(writable, tls)
    }

    fn define_function(
        &mut self,
        func: FuncId,
        ctx: &mut cranelift_codegen::Context,
        trap_sink: &mut dyn cranelift_codegen::binemit::TrapSink,
        stack_map_sink: &mut dyn cranelift_codegen::binemit::StackMapSink,
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        if self.eager {
            return self.module.define_function(func, ctx, trap_sink, stack_map_sink);
        }

        let decl = self.module.declarations().get_function_decl(func);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(decl.name.clone()));
        }

        if !self.defined_functions.insert(func) {
            return Err(ModuleError::DuplicateDefinition(decl.name.to_owned()));
        }

        self.functions.push((func, ctx.func.clone()));

        Ok(ModuleCompiledFunction { size: 0 })
    }

    fn define_function_bytes(
        &mut self,
        func: FuncId,
        bytes: &[u8],
        relocs: &[cranelift_module::RelocRecord],
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        if self.eager {
            return self.module.define_function_bytes(func, bytes, relocs);
        }

        let decl = self.module.declarations().get_function_decl(func);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(decl.name.clone()));
        }

        if !self.defined_functions.insert(func) {
            return Err(ModuleError::DuplicateDefinition(decl.name.to_owned()));
        }

        let res = self.module.define_function_bytes(func, bytes, relocs);

        if res.is_err() {
            self.defined_functions.remove(&func);
        }

        res
    }

    fn define_data(
        &mut self,
        data: DataId,
        data_ctx: &DataContext,
    ) -> cranelift_module::ModuleResult<()> {
        self.module.define_data(data, data_ctx)
    }
}
