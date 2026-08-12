use derive_generic_visitor::*;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use crate::{ast::*, name_matcher::NamePattern};

use crate::transform::utils::remove_clauses;
use crate::transform::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct RemoveLastParamVisitor {
    types: HashSet<TypeDeclId>,
}

impl VisitAstMut for RemoveLastParamVisitor {
    fn enter_type_decl_ref(&mut self, x: &mut TypeDeclRef) {
        if self.types.contains(&x.id) {
            // Remove the last param.
            x.generics.types.pop();
        }
    }
}

/// Collects the item-level clauses that a value refers to, and the ones that constrain a given
/// type parameter.
#[derive(Visitor)]
struct ClauseUsesVisitor {
    param: TypeVarId,
    binder_depth: DeBruijnId,
    /// Whether we saw the parameter itself.
    mentions_param: bool,
    /// The clauses that were named by a `TraitRefKind::Clause`.
    referenced_clauses: HashSet<TraitClauseId>,
}

impl VisitorWithBinderDepth for ClauseUsesVisitor {
    fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
        &mut self.binder_depth
    }
}

impl VisitAst for ClauseUsesVisitor {
    fn visit<T: AstVisitable>(&mut self, x: &T) -> ControlFlow<Self::Break> {
        VisitWithBinderDepth::new(self).visit(x)
    }
    fn enter_ty_kind(&mut self, kind: &TyKind) {
        if let TyKind::TypeVar(var) = kind
            && var.bound_at_depth(self.binder_depth) == Some(self.param)
        {
            self.mentions_param = true;
        }
    }
    fn enter_trait_ref_kind(&mut self, kind: &TraitRefKind) {
        if let TraitRefKind::Clause(var) = kind
            && let Some(clause_id) = var.bound_at_depth(self.binder_depth)
        {
            self.referenced_clauses.insert(clause_id);
        }
    }
}

impl ClauseUsesVisitor {
    fn new(param: TypeVarId) -> Self {
        ClauseUsesVisitor {
            param,
            binder_depth: DeBruijnId::ZERO,
            mentions_param: false,
            referenced_clauses: Default::default(),
        }
    }
}

/// The clauses of `decl` that constrain the parameter we're about to remove, and that nothing in
/// the declaration refers to. Those we can drop along with the parameter.
fn removable_clauses_on_param(decl: &TypeDecl, param: TypeVarId) -> HashSet<TraitClauseId> {
    let mut on_param: HashSet<TraitClauseId> = decl
        .generics
        .trait_clauses
        .iter()
        .filter_map(|clause| {
            let mut visitor = ClauseUsesVisitor::new(param);
            let _ = visitor.visit(clause);
            visitor.mentions_param.then_some(clause.clause_id)
        })
        .collect();
    let mut visitor = ClauseUsesVisitor::new(param);
    let _ = visitor.visit(decl);
    on_param.retain(|clause_id| !visitor.referenced_clauses.contains(clause_id));
    on_param
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if !ctx.options.hide_allocator {
            return;
        }
        let types = &[
            "alloc::boxed::Box",
            "alloc::vec::Vec",
            "alloc::rc::Rc",
            "alloc::sync::Arc",
        ];

        let types: Vec<NamePattern> = types
            .iter()
            .map(|s| NamePattern::parse(s).unwrap())
            .collect_vec();
        let types: HashSet<TypeDeclId> = ctx
            .translated
            .item_names
            .iter()
            .filter(|(_, name)| types.iter().any(|p| p.matches(&ctx.translated, name)))
            .filter_map(|(id, _)| id.as_type())
            .copied()
            .collect();

        let mut removed_clauses: HashMap<ItemId, HashSet<TraitClauseId>> = Default::default();
        for &id in &types {
            if let Some(tdecl) = ctx.translated.type_decls.get_mut(id) {
                if tdecl.generics.types.is_empty() {
                    // We monomorpohized this type.
                    let args = tdecl.item_meta.name.mono_args_mut().unwrap();
                    args.types.pop().unwrap();
                } else {
                    struct SubstWithErrorVisitor(TypeVarId);
                    impl VarsVisitor for SubstWithErrorVisitor {
                        fn visit_type_var(&mut self, v: TypeDbVar) -> Option<Ty> {
                            if let DeBruijnVar::Bound(DeBruijnId::ZERO, var_id) = v
                                && var_id == self.0
                            {
                                Some(
                                    TyKind::Error("removed allocator parameter".to_owned())
                                        .into_ty(),
                                )
                            } else {
                                None
                            }
                        }
                    }
                    let tvar = tdecl.generics.types.pop().unwrap();
                    removed_clauses
                        .insert(id.into(), removable_clauses_on_param(tdecl, tvar.index));
                    tdecl.visit_vars(&mut SubstWithErrorVisitor(tvar.index));
                }
            }
        }

        let _ = ctx
            .translated
            .drive_mut(&mut RemoveLastParamVisitor { types });
        remove_clauses(&mut ctx.translated, &removed_clauses);
    }
}
