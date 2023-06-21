[![GitHub](https://img.shields.io/badge/github-valsteen/sort_by_derive-8da0cb?labelColor=555555&logo=github)](https://github.com/valsteen/sort_by_derive)
[![Crates.io](https://img.shields.io/crates/v/sort_by_derive)](https://crates.io/crates/sort_by_derive)
[![docs.rs](https://img.shields.io/docsrs/sort_by_derive)](https://docs.rs/sort_by_derive)
[![Continuous integration](https://github.com/tlaferriere/sort_by_derive/actions/workflows/rust.yml/badge.svg)](https://github.com/valsteen/sort_by_derive/actions/workflows/rust.yml)
# sort_by_derive

<!-- TOC -->
* [Usage](#usage)
    * [SortBy](#sortby)
    * [EnumAccessor](#enumaccessor)
        * [Field accessor](#field-accessor)
        * [EnumSequence](#enumsequence)
    * [Example](#example)
<!-- TOC -->

This crate provides 3 derive macros `SortBy`, `EnumAccessor` and `EnumSequence`.

- `SortBy` derives the traits `Ord`, `PartialOrd`, `Eq`, `PartialEq` and `Hash` on structs that can't automatically derive those traits because they contain unorderable fields such as `f32`.
- On enums and structs, `SortBy` can also implement a `Ord` trait that calls arbitrary methods - this is particularly useful in combination with enum variant accessor methods derived by `EnumAccessor` an `EnumSequence`
- `EnumAccessor` derives accessor methods to common fields in variants - so you don't need to write yourself `match` statements to access a field with the same name and type on different variants. This feature is similar to [enum_dispatch](https://crates.io/crates/enum_dispatch), but takes a different approach where structs don't need to implement a trait.
- `EnumSequence` provides a `enum_sequence` method where the first variant returns `0`, the second `1`, etc. This is useful is you want to implement a custom sorting, while the order of declaration of variant is still relevant as a secondary ordering criteria.


## Usage

### SortBy

Fields that should be used for sorting are marked with the attribute `#[sort_by]`.
Other fields will be ignored.

```rust
use std::cmp::Ordering;
use sort_by_derive::SortBy;

#[derive(SortBy)]
struct Something {
    #[sort_by]
    a: u16,
    b: u16
}

assert_eq!(Something{a: 2, b: 0}.cmp(&Something{a: 1, b: 1}), Ordering::Greater); // a is compared
assert_eq!(Something{a: 1, b: 0}.cmp(&Something{a: 1, b: 1}), Ordering::Equal); // b is ignored
```

### EnumAccessor

This derive macro is similar to [enum_dispatch](https://crates.io/crates/enum_dispatch).
`enum_dispatch` requires structs to implement a common trait, which can be useful if a common set of functions applies to all variants.
`EnumAccessor` takes the opposite approach: common fields and methods are declared at enum level, and you can have variants that don't have a given field or method.
This may be more practical if there is a large amount of variants and your only concern is accessing fields, because individual structs just hold data.
This is typical for events - they represent a state change and are generally consumed as a whole, individual structs have no code of their own.

#### Field accessor

After adding `derive(EnumAccessor)` to the enum, fields are declared as `accessor(field: type)` attributes.
Aliases can be created to rename the accessor in order to avoid name clashes with the `as` keyword: `accessor(field as alias: type)`.

This will derive the accessor methods `fn name(&self) -> &type;` and`fn name_mut(&mut self) -> &mut type;`, and return a reference to the field of the same name or alias on any variant.

```rust
use sort_by_derive::EnumAccessor;

#[derive(EnumAccessor)]
#[accessor(a: u16)]
#[accessor(b as my_b: u16)]
enum E {
    Variant1{a: u16, b: u16},
    Variant2{a: u16, b: u16, c: u32},
}

let v1 = E::Variant1{a: 1, b: 1};
let mut v2 = E::Variant2{a: 1, b: 1, c: 2};

// Accessor methods are generated for the specified members
assert_eq!(*v1.a(), 1);
assert_eq!(*v2.my_b(), 1);

// Mutable accessors are also generated
*v2.a_mut() = 2;
assert_eq!(*v2.a(), 2);
```

So you can take any `E`, all variants will have `a`, `a_mut`, `my_b`, `my_b_mut`

### EnumSequence

Simply derive `EnumSequence`, and you get `enum_sequence(&self)` which returns a `usize`, starting from `0` and incrementing for each variant.

When using enums of enums, creating an accessor to the inner enum's sequence may create a method name ambiguity. To mitigate this, a custom accessor name can be chosen by using `as`, for instance `#[accessor(enum_sequence() as inner_sequence: usize)]`

**Note**: this will create an extension trait `{TypeName}EnumSequence` ( i.e. the type `T` will get a new trait `TEnumSequence` ). This trait will have the same visibility as the type. When using this type from another module, make sure to bring the trait in scope with `use {TypeName}EnumSequence`.

#### Example

```rust
use sort_by_derive::EnumSequence;

#[derive(EnumSequence)]
enum ABC {
    A(u8),
    B(String),
    C { f: String, g: usize }
}

assert_eq!(ABC::B("hi!".into()).enum_sequence(), 1);
```

