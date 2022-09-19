# sort_by_derive

This crate provides the `#[derive(SortBy)]` derive macro that implements the traits `Ord`, `PartialOrd`, `Eq`, `PartialEq` and `Hash` on structs that can't automatically derive those traits because they contain unorderable fields such as `f32`.

Fields that must be included in the sorting are explicitly marked with the attribute `#[sort_by]`, unmarked fields will be ignored.

As with `#[derive(Ord)]` and `#[derive(PartialEq)]`, order of sorting is the same as the order of appearance of fields.

## Example

```rust
#[derive(SortBy)]
struct Toto {
    #[sort_by]
    a: u16,
    #[sort_by]
    c: u32,
    b: f32
}
```

expands to

```rust
struct Toto {
    a: u16,
    c: u32,
    b: f32
}

impl std::hash::Hash for Toto {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.a.hash(state);
        self.c.hash(state);
    }
}
impl core::cmp::Eq for Toto {}
impl core::cmp::PartialEq<Self> for Toto {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl core::cmp::PartialOrd<Self> for Toto {
    fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.a.cmp(&other.a).then_with(|| self.c.cmp(&other.c))
    }
}
```

From there `sort` can be called on a `Vec<Toto>`.

## Limitations

Only structs with named fields are supported.
