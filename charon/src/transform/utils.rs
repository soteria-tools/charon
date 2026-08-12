use crate::ast::*;
use crate::formatter::{AstFormatter, FmtCtx};
use crate::ids::IndexVec;
use crate::pretty::FmtWithCtx;
use derive_generic_visitor::*;
use macros::EnumIsA;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug};

/// Each `GenericArgs` is meant for a corresponding `GenericParams`; this describes which one.
#[derive(Debug, Clone, Eq, PartialEq, Hash, EnumIsA, Drive, DriveMut)]
pub enum GenericsSource {
    /// A top-level item.
    Item(ItemId),
    /// A trait method.
    Method(TraitDeclId, TraitMethodId),
    /// A trait associated type.
    TraitType(TraitDeclId, AssocTypeId),
    /// A builtin item like `Box`.
    Builtin,
    /// Some other use of generics outside the main Charon ast.
    Other,
}

impl GenericsSource {
    pub fn item<I: Into<ItemId>>(id: I) -> Self {
        Self::Item(id.into())
    }

    /// Return a path that represents the target item.
    pub fn item_name(&self, translated: &TranslatedCrate, fmt_ctx: &FmtCtx) -> String {
        match self {
            GenericsSource::Item(id) => translated.item_name(*id).to_string_with_ctx(fmt_ctx),
            GenericsSource::Method(trait_id, method_name) => format!(
                "{}::{method_name}",
                translated.item_name(*trait_id).to_string_with_ctx(fmt_ctx),
            ),
            GenericsSource::TraitType(trait_id, type_id) => {
                let type_name =
                    fmt::from_fn(|f| fmt_ctx.format_assoc_type_name(f, *trait_id, *type_id));
                format!(
                    "{}::{type_name}",
                    translated.item_name(*trait_id).to_string_with_ctx(fmt_ctx),
                )
            }
            GenericsSource::Builtin => "<built-in>".to_string(),
            GenericsSource::Other => "<unknown>".to_string(),
        }
    }
}

impl TypeDeclId {
    pub fn generics_target(&self) -> GenericsSource {
        GenericsSource::item(*self)
    }
}
impl FunId {
    pub fn generics_target(&self) -> GenericsSource {
        match *self {
            FunId::Regular(fun_id) => GenericsSource::item(fun_id),
            FunId::Builtin(..) => GenericsSource::Builtin,
        }
    }
}
impl FnPtrKind {
    pub fn generics_target(&self) -> GenericsSource {
        match self {
            FnPtrKind::Fun(fun_id) => fun_id.generics_target(),
            FnPtrKind::Trait(trait_ref, name) => {
                GenericsSource::Method(trait_ref.trait_decl_ref.skip_binder.id, *name)
            }
        }
    }
}

/// Remove the given trait clauses from the items that declare them. The remaining clauses of each
/// item are renumbered to stay contiguous, and every reference to them in the crate is updated to match.
pub fn remove_clauses(
    translated: &mut TranslatedCrate,
    to_remove: &HashMap<ItemId, HashSet<TraitClauseId>>,
) {
    // For each item, a map from old clause ids to new ones. The new ones are in the same order,
    // just skipping the removed ones.
    let remaps: HashMap<ItemId, IndexVec<TraitClauseId, Option<TraitClauseId>>> = translated
        .all_items_mut()
        .filter_map(|mut item| {
            let item_id = item.as_ref().id();
            let clauses_to_remove = to_remove.get(&item_id)?;
            if clauses_to_remove.is_empty() {
                return None;
            }
            let item_clauses = &mut item.generic_params().trait_clauses;
            let remap: IndexVec<TraitClauseId, Option<TraitClauseId>> =
                std::mem::take(item_clauses).map_indexed(|old_id, mut clause| {
                    if clauses_to_remove.contains(&old_id) {
                        None
                    } else {
                        let new_id = item_clauses.push_with(|new_id| {
                            clause.clause_id = new_id;
                            clause
                        });
                        Some(new_id)
                    }
                });
            Some((item_id, remap))
        })
        .collect();

    for mut item in translated.all_items_mut() {
        let item_id = item.as_ref().id();
        item.drive_mut(&mut RemoveClausesVisitor {
            remaps: &remaps,
            current_item: item_id,
            binder_depth: DeBruijnId::ZERO,
        });
    }

    // updated translated names
    let names = translated
        .item_names
        .iter_mut()
        .chain(translated.short_names.iter_mut());
    for (&item_id, name) in names {
        let _ = RemoveClausesVisitor {
            remaps: &remaps,
            current_item: item_id,
            binder_depth: DeBruijnId::ZERO,
        }
        .visit(name);
    }
}

#[derive(Visitor)]
struct RemoveClausesVisitor<'a> {
    remaps: &'a HashMap<ItemId, IndexVec<TraitClauseId, Option<TraitClauseId>>>,
    current_item: ItemId,
    binder_depth: DeBruijnId,
}

impl VisitorWithBinderDepth for RemoveClausesVisitor<'_> {
    fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
        &mut self.binder_depth
    }
}

impl VisitorWithItemRefMut for RemoveClausesVisitor<'_> {
    fn enter_item_ref(&mut self, item_id: ItemId, args: &mut GenericArgs) {
        if let Some(remap) = self.remaps.get(&item_id) {
            for (old_id, trait_ref) in std::mem::take(&mut args.trait_refs).into_iter_enumerated() {
                if remap[old_id].is_some() {
                    args.trait_refs.push(trait_ref);
                }
            }
        }
    }
}

impl VisitAstMut for RemoveClausesVisitor<'_> {
    fn visit<T: AstVisitable>(&mut self, x: &mut T) -> ControlFlow<Self::Break> {
        VisitWithBinderDepth::new(VisitWithItemRef::new(self)).visit(x)
    }

    fn visit_trait_ref_kind(&mut self, x: &mut TraitRefKind) -> ControlFlow<Self::Break> {
        if let TraitRefKind::Clause(var) = x
            && let Some(clause_id) = var.bound_at_depth_mut(self.binder_depth)
            && let Some(remap) = self.remaps.get(&self.current_item)
        {
            *clause_id = remap[*clause_id].expect("mismatch while trying to remove clauses");
        }
        self.visit_inner(x)
    }
}
