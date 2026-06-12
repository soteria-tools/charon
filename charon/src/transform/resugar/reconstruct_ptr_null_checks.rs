//! # Micro-pass: reconstruct pointer null-checks.
//!
//! When rustc lowers a pointer null-check (e.g. `<*const T>::is_null`), it reinterprets the
//! pointer's bits as an integer and tests that integer against `0`. Depending on the MIR level, the
//! test takes one of two shapes:
//! ```text
//! // optimized MIR: the comparison is a `switch` terminator
//! _23 := transmute<*mut u8, usize>(copy raw_ptr_6);
//! switch copy _23 -> [0: bb20, otherwise: bb21]
//!
//! // built MIR: the comparison is a `== 0` / `!= 0` statement
//! _3 := transmute<*const u8, usize>(copy _2);
//! _0 := move _3 == const 0usize;
//! ```
//!
//! Some tools would like to avoid reasoning about the pointer-to-integer transmutation.
//! Instead we replace it with a call to the [`BuiltinFunId::ZeroIfNull`] builtin,
//! which abstracts away the actual address and only keeps
//! the null-vs-non-null information that the comparison needs:
//! ```text
//! _23 = @ZeroIfNull<*mut u8>(copy raw_ptr_6) -> bb_new
//! bb_new: switch copy _23 -> [0: bb20, otherwise: bb21]
//! ```
//! Because this introduces a new call, this means creating a new block as well.
//!
//! The transformation is only fired when:
//! - we transmute a *sized* (thin) pointer to `usize`/`isize`;
//! - the result feeds either a switch with a single `0` case and an otherwise branch, or a
//!   `== 0` / `!= 0` comparison against the null address;
//! - the transmuted value is used nowhere else (necessary for soundness)

use derive_generic_visitor::Visitor;
use std::ops::ControlFlow::{self, Continue};

use crate::ids::IndexVec;
use crate::transform::TransformCtx;
use crate::transform::ctx::UllbcPass;
use crate::ullbc_ast::*;

/// Count how many times each local appears in the body, ignoring `StorageLive`/`StorageDead`
/// statements (which mention a local but don't actually use its value).
#[derive(Visitor)]
struct LocalUsageCounter {
    counts: IndexVec<LocalId, usize>,
}

impl VisitBody for LocalUsageCounter {
    fn enter_local_id(&mut self, lid: &LocalId) {
        self.counts[*lid] += 1;
    }
    fn visit_ullbc_statement(&mut self, st: &Statement) -> ControlFlow<Self::Break> {
        match &st.kind {
            StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => Continue(()),
            _ => self.visit_inner(st),
        }
    }
}

fn count_local_usages(b: &ExprBody) -> IndexVec<LocalId, usize> {
    let mut visitor = LocalUsageCounter {
        counts: b.locals.locals.map_ref(|_| 0),
    };
    let _ = b.body.drive_body(&mut visitor);
    visitor.counts
}

/// Whether `ty` is `usize` or `isize`.
fn is_usize_or_isize(ty: &Ty) -> bool {
    matches!(
        ty.kind(),
        TyKind::Literal(LiteralTy::UInt(UIntTy::Usize) | LiteralTy::Int(IntTy::Isize))
    )
}

/// Whether `local`'s non-defining use is a pointer null-check, i.e. either:
/// - a `switch local -> [0: _, otherwise: _]` terminator (the shape rustc emits in *optimized*
///   MIR), or
/// - a `_ = (local == 0)` / `_ = (local != 0)` comparison statement (the shape rustc emits in
///   *built* MIR, where `<*const T>::is_null` is `transmute::<*T, usize>(p) == 0`).
///
/// The caller guarantees `local` is used exactly twice, so whichever use we find here is its unique
/// non-defining use.
fn feeds_null_check(block: &BlockData, local: LocalId) -> bool {
    // The result feeds a `switch [0, otherwise]` terminator.
    if let TerminatorKind::Switch {
        discr,
        targets: SwitchTargets::SwitchInt(_, cases, _),
    } = &block.terminator.kind
        && let [(case, _)] = cases.as_slice()
        && case.is_zero()
        && discr.as_local() == Some(local)
    {
        return true;
    }
    // The result feeds a `== 0` / `!= 0` comparison against the null address.
    block.statements.iter().any(|st| {
        matches!(
            &st.kind,
            StatementKind::Assign(_, Rvalue::BinaryOp(BinOp::Eq | BinOp::Ne, lhs, rhs))
                if (lhs.as_local() == Some(local) && rhs.is_zero_constant())
                    || (rhs.as_local() == Some(local) && lhs.is_zero_constant())
        )
    })
}

pub struct Transform;

impl UllbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        options.reconstruct_null_checks
    }

    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        let usages = count_local_usages(b);

        // We push new blocks during the loop, so snapshot the ids first.
        let block_ids: Vec<BlockId> = b.body.indices().collect();
        for block_id in block_ids {
            // Match the `transmute` + null-check pattern, extracting what we need to build the call.
            let src_ty;
            let operand;
            let dest;
            // Index of the `transmute` statement in the block.
            let transmute_idx;
            {
                let block = &b.body[block_id];
                // Find a `transmute` of a sized (thin) pointer into a `usize`/`isize` whose result
                // is used exactly twice and whose unique other use is a null-check (a
                // `switch [0, _]` or a `== 0` / `!= 0`).
                let Some((idx, place, s_ty, op)) =
                    block.statements.iter().enumerate().find_map(|(idx, st)| {
                        let StatementKind::Assign(
                            place,
                            Rvalue::UnaryOp(UnOp::Cast(CastKind::Transmute(s_ty, t_ty)), op),
                        ) = &st.kind
                        else {
                            return None;
                        };
                        let result = place.as_local()?;
                        if !is_usize_or_isize(t_ty) || usages[result] != 2 {
                            return None;
                        }
                        let (TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)) = s_ty.kind()
                        else {
                            return None;
                        };
                        if !matches!(pointee.get_ptr_metadata(&ctx.translated), PtrMetadata::None) {
                            return None;
                        }
                        if !feeds_null_check(block, result) {
                            return None;
                        }
                        Some((idx, place, s_ty, op))
                    })
                else {
                    continue;
                };

                src_ty = s_ty.clone();
                operand = op.clone();
                dest = place.clone();
                transmute_idx = idx;
            }

            // Split the block at the `transmute`.
            let (tail, terminator) = {
                let block = &mut b.body[block_id];
                let tail = block.statements.split_off(transmute_idx + 1);
                block.statements.truncate(transmute_idx); // We get rid of the transmute statement
                (tail, block.terminator.clone())
            };
            let new_block_id = b.body.push_with(|_| BlockData {
                statements: tail,
                terminator,
            });

            // `ZeroIfNull` never unwinds, so the unwind edge is unreachable; we point it at the
            // successor block to avoid introducing a spurious cleanup block.
            let generics = GenericArgs::new([].into(), [src_ty].into(), [].into(), [].into());
            let block = &mut b.body[block_id];
            block.terminator.kind = TerminatorKind::Call {
                call: Call {
                    func: FnOperand::Regular(FnPtr::new(
                        FnPtrKind::mk_builtin(BuiltinFunId::ZeroIfNull),
                        generics,
                    )),
                    args: vec![operand],
                    dest,
                },
                target: new_block_id,
                on_unwind: new_block_id,
            };
        }
    }
}
