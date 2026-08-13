//@ charon-args=--remove-associated-types=*
#![feature(associated_type_defaults)]

//! When a defaulted associated type is instantiated for an impl, the proofs it uses become
//! self-referential. This can cause `--remove-associated-types` to loop forever, if we're
//! not careful.

trait ProveWithParentClause<'a, T: 'a> {
    type X: 'a;
    type Item = &'a (T, Self::X);
}

impl<'a, T: 'a> ProveWithParentClause<'a, Option<T>> for () {
    type X = &'a ();
}

trait ProveWithItemClause<'a, T: 'a> {
    type X: 'a;
    type Item = &'a (Self::X, T);
}

impl<'a, T: 'a> ProveWithItemClause<'a, Option<T>> for () {
    type X = &'a ();
}

trait ProveForNormalAdt<'a, T: 'a> {
    type X: 'a;
    type Item = &'a Result<T, Self::X>;
}

impl<'a, T: 'a> ProveForNormalAdt<'a, Option<T>> for () {
    type X = &'a ();
}

struct NeedsClone<T: Clone>(T);

trait ProveWithLocalClause<T: Copy> {
    type Item = NeedsClone<T>;
}

impl<T: Copy> ProveWithLocalClause<T> for () {}
