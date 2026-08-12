//@ charon-arg=--monomorphize-mut
//! `--monomorphize-mut` specializes the tuple declaration, so the tuple type's arguments are no
//! longer its field types. It must still print as `(&'a mut u32, T)`.

fn id<T>(x: T) -> T {
    x
}

fn call<'a, T>(x: (&'a mut u32, T)) -> (&'a mut u32, T) {
    id(x)
}
