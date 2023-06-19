#![doc = include_str!("../README.md")]
//! ## All together
//!
//! Imagine the following :
//!
//! ```rust
//! # use sort_by_derive::SortBy;
//! # use sort_by_derive::EnumAccessor;
//! # use sort_by_derive::EnumSequence;
//! # use std::cmp::Ordering;
//! #
//! #[derive(EnumSequence, EnumAccessor, SortBy, Debug)]
//! #[accessor(global_time: usize)]
//! #[accessor(channel: u8, except(CC))]
//! #[accessor(pitch: u8, except(CC, Unsupported))]
//! #[sort_by(global_time(), channel(), pitch(), enum_sequence())]
//! enum Note {
//! // ...
//! #    NoteOn {
//! #        global_time: usize,
//! #        pitch: u8,
//! #        channel: u8
//! #    },
//! #    NoteOff {
//! #        global_time: usize,
//! #        pitch: u8,
//! #        channel: u8
//! #    },
//! #    CC {
//! #        global_time: usize
//! #    },
//! #    Unsupported {
//! #        global_time: usize,
//! #        raw_data: Vec<u8>,
//! #        channel: u8
//! #    }
//! }
//!
//! assert_eq!(
//!     Note::NoteOn {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 0,
//!     }.cmp(&Note::NoteOn {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 0,
//!     }),
//!     Ordering::Equal
//! );
//! assert_eq!(
//!     Note::NoteOn {
//!         global_time: 0,
//!         pitch: 2,
//!         channel: 2,
//!     }.cmp(&Note::NoteOff {
//!         global_time: 2,
//!         pitch: 0,
//!         channel: 0,
//!     }),
//!     Ordering::Less
//! );
//! assert_eq!(
//!     Note::NoteOn {
//!         global_time: 0,
//!         pitch: 2,
//!         channel: 0,
//!     }.cmp(&Note::NoteOff {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 2,
//!     }),
//!     Ordering::Less
//! );
//! assert_eq!(
//!     Note::NoteOn {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 0,
//!     }.cmp(&Note::NoteOff {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 2,
//!     }),
//!     Ordering::Less
//! );
//! assert_eq!(
//!     Note::NoteOn {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 0,
//!     }.cmp(&Note::NoteOff {
//!         global_time: 0,
//!         pitch: 0,
//!         channel: 0,
//!     }),
//!     Ordering::Less
//! );
//! ```
//!
//! Now I have a `Note` enum that will sort by `global_time`, `channel`, `pitch`, and lastly by variant order ( `enum_sequence` ). Note that `None` is always less than `Some`.
//!
//! Conversely, separate structs such as `NoteOn` may derive from `SortBy` in order to ignore some fields ( ex: `velocity` may be a `f32`, so we can't directly derive `Ord` ).
use syn::{parse_macro_input, DeriveInput};

mod enum_sequence;
mod enum_variant_accessor;
mod sort_by;

/// Fields that should be used for sorting are marked with the attribute `#[sort_by]`.
/// Other fields will be ignored.
///
/// ```rust
/// # use std::cmp::Ordering;
/// use sort_by_derive::SortBy;
///
/// #[derive(SortBy)]
/// struct Something {
///     #[sort_by]
///     a: u16,
///     b: u16
/// }
///
/// assert_eq!(Something{a: 2, b: 0}.cmp(&Something{a: 1, b: 1}), Ordering::Greater); // a is compared
/// assert_eq!(Something{a: 1, b: 0}.cmp(&Something{a: 1, b: 1}), Ordering::Equal); // b is ignored
/// ```
/// You can use it the same way with tuple structs:
///
/// ```rust
/// # use std::cmp::Ordering;
/// # use sort_by_derive::SortBy;
/// #
/// #[derive(SortBy)]
/// struct Something(
///     #[sort_by]
///     u16,
///     #[sort_by]
///     u32,
///     f32
/// );
///
/// assert_eq!(Something(1, 0, 1.0).cmp(&Something(1, 0, 2.0)), Ordering::Equal); // Compares only specified fields
/// assert_eq!(Something(2, 0, 1.0).cmp(&Something(1, 0, 2.0)), Ordering::Greater); // Compares only specified fields
/// ```
///
///
/// Alternatively, or in combination with, a struct-level or enum-level `#[sort_by(method1(),method2(),attr1,nested.attr)]` can be declared.
/// This top-level declaration takes precedence, fields comparison will be considered if top-level comparisons are all `eq`.
/// The top-level `sort_by` attribute takes a list of attributes or method calls; items will be prepended with `self.`.
///
/// ```rust
/// # use std::cmp::Ordering;
/// # use sort_by_derive::SortBy;
/// #
/// #[derive(SortBy)]
/// #[sort_by(product())]
/// struct Something {
///     #[sort_by]
///     a: u16,
///     b: u16,
/// }
///
/// impl Something {
///     fn product(&self) -> u16 {
///         self.a * self.b
///     }
/// }
///
/// assert_eq!(Something{a: 1, b: 1}.cmp(&Something{a: 1, b: 2}), Ordering::Less); // method comparison precedes member comparison
/// assert_eq!(Something{a: 2, b: 0}.cmp(&Something{a: 1, b: 0}), Ordering::Greater); // method comparison is equal (0 = 0) so fall back to member comparison
/// ```
///
/// ## Limitation
///
/// - struct-level `sort_by` attribute always comes before field-level attributes lexicographically.
#[proc_macro_derive(SortBy, attributes(sort_by))]
pub fn sort_by_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    sort_by::impl_sort_by_derive(ast).into()
}

/// This derive macro is similar to [enum_dispatch](https://crates.io/crates/enum_dispatch).
/// `enum_dispatch` requires structs to implement a common trait, which can be useful if a common set of functions applies to all variants.
/// `EnumAccessor` takes the opposite approach: common fields and methods are declared at enum level, and you can have variants that don't have a given field or method.
/// This may be more practical if there is a large amount of variants and your only concern is accessing fields, because individual structs just hold data.
/// This is typical for events - they represent a state change and are generally consumed as a whole, individual structs have no code of their own.
///
/// #### Field accessor
///
/// After adding `derive(EnumAccessor)` to the enum, fields are declared as `accessor(field: type)` attributes:
///
/// This will derive the accessor methods `fn name(&self) -> &type;` and`fn name_mut(&mut self) -> &mut type;`, and return a reference to the field of the same name on any variant.
///
/// ```rust
/// use sort_by_derive::EnumAccessor;
///
/// #[derive(EnumAccessor)]
/// #[accessor(a: u16)]
/// #[accessor(b: u16)]
/// enum E {
///     Variant1{a: u16, b: u16},
///     Variant2{a: u16, b: u16, c: u32},
/// }
///
/// let v1 = E::Variant1{a: 1, b: 1};
/// let mut v2 = E::Variant2{a: 1, b: 1, c: 2};
///
/// // Accessor methods are generated for the specified members
/// assert_eq!(*v1.a(), 1);
/// assert_eq!(*v2.b(), 1);
///
/// // Mutable accessors are also generated
/// *v2.a_mut() = 2;
/// assert_eq!(*v2.a(), 2);
/// ```
///
/// So you can take any `E`, all variants will have `a`, `a_mut`, `b`, `b_mut`
///
/// ```rust
/// # use sort_by_derive::EnumAccessor;
/// #
/// #[derive(EnumAccessor)]
/// #[accessor(a: u16)]
/// #[accessor(b: u16)]
/// enum E {
///     Variant1{a: u16, b: u16},
///     Variant2{a: u16, b: u16, c: u32},
/// }
///
/// fn do_something(e: &mut E) -> u16 {
///     let field_value = *e.a(); // take the value of that field, whatever variant it is
///     *e.a_mut() = 42; // use the accessor method returning a &mut to the field
///     field_value
/// }
///
/// let mut v1 = E::Variant1{a: 11, b: 0};
/// assert_eq!(do_something(&mut v1), 11);
/// assert_eq!(*v1.a(), 42);
///
/// let mut v2 = E::Variant2{a: 11, b: 0, c: 32};
/// assert_eq!(do_something(&mut v2), 11);
/// assert_eq!(*v2.a(), 42);
/// ```
///
/// Use `except` or `only` if not all variants have a given field:
///
/// ```rust
/// # use sort_by_derive::EnumAccessor;
/// #
/// #[derive(EnumAccessor)]
/// #[accessor(a: u16, only(Variant1, Variant2))]
/// #[accessor(c: u32, except(Variant1, Variant3))]
/// enum E {
///     Variant1 { a: u16, b: u16 },
///     Variant2 { a: u16, b: u16, c: u32 },
///     Variant3 { d: u64 },
///     Variant4 { c: u32 },
/// }
///
/// assert_eq!(E::Variant1 { a: 1, b: 2 }.a(), Some(&1));
/// assert_eq!(E::Variant2 { a: 1, b: 2, c: 0 }.a(), Some(&1));
/// assert_eq!(E::Variant3 { d: 0 }.a(), None);
/// assert_eq!(E::Variant2 { a: 0, b: 0, c: 2 }.c(), Some(&2));
/// assert_eq!(E::Variant1 { a: 0, b: 0 }.c(), None);
/// ```
/// This derives the same accessor methods, but the return type will be `Option<&type>` and `Option<&mut type>`.
/// The provided comma-separated list of variants in `except` will return `None`.
/// The provided comma-separated list of variants in `only` must have the given field and will return `Some`.
///
/// Methods without arguments ( i.e. only `&self` are also supported ).
/// It takes the form: `#[accessor(method_name(): type)]`.
/// If `type` is a `&mut`, the generated method will take `&mut self` instead of `&self`.
/// This can be useful for accessing mutable derived methods of nested enums.
///
/// To avoid name clashes, accessors can be given an alias by using `as`:
///
/// ```rust
/// # use sort_by_derive::EnumAccessor;
/// #
/// #[derive(EnumAccessor)]
/// #[accessor(a as a_attr: u16, except(Variant3))]
/// enum E {
///     Variant1 { a: u16 },
///     Variant2 { a: u16, c: u32 },
///     Variant3 { b: u32 },
/// }
///
/// impl E {
///     fn a(&self) -> bool {
///         // Unrelated work
///         #        true
///     }
/// }
///
/// assert_eq!(E::Variant1 { a: 1 }.a(), true);
/// assert_eq!(E::Variant1 { a: 1 }.a_attr(), Some(&1));
/// assert_eq!(E::Variant2 { a: 1, c: 0 }.a_attr(), Some(&1));
/// assert_eq!(E::Variant3 { b: 0 }.a_attr(), None);
/// ```
///
/// **Caveat**: Aliasing doesn't prevent *Duplicate accessor* error:
///
/// ```compile_fail
/// # use sort_by_derive::EnumAccessor;
/// #
/// #[derive(EnumAccessor)]
/// #[accessor(a: u16, except(Variant3))]
/// #[accessor(a as a_big: u32, except(Variant1,Variant2))] // error: Duplicate accessor a
/// enum E {
///     Variant1 { a: u16 },
///     Variant2 { a: u16, c: u32 },
///     Variant3 { a: u32 },
/// }
///
/// assert_eq!(E::Variant1 { a: 1 }.a(), Some(&1));
/// assert_eq!(E::Variant2 { a: 1, c: 0 }.a(), Some(&1));
/// assert_eq!(E::Variant3 { a: 0 }.a(), None);
/// assert_eq!(E::Variant3 { a: 2 }.a_big(), Some(&2));
/// assert_eq!(E::Variant1 { a: 0 }.a_big(), None);
/// ```
///
/// **Note**: this will create an extension trait `{TypeName}Accessor` ( i.e. the type `T` will get a new trait `TAccessor` ).
/// This trait will have the same visibility as the type.
/// When using this type from another module, make sure to bring the trait in scope with `use {TypeName}Accessor`.
///
/// #### Example
///
/// Say we have a series of midi events, they are very similar but with slight variations - they always have some timing information but they may not always have a pitch or channel.
///
/// Using `#[accessor(global_time: usize)]`, a `global_time(&self)` method is derived, along with a `global_time_mut(&mut self)`, so without any boilerplate you can access the timing.
///
/// By declaring `#[accessor(channel: u8, except(CC))]`, `channel(&self)` and `channel_mut(&mut self)` are derived, but they return `Some` for `NoteOn` and `NoteOff`, and `None` for `CC` and `Unsupported`.
///
/// ```rust
/// # use sort_by_derive::EnumAccessor;
/// #
/// #[derive(EnumAccessor)]
/// #[accessor(global_time: usize)]
/// #[accessor(channel: u8, except(CC))]
/// #[accessor(pitch: u8, except(CC, Unsupported))]
/// enum Note {
///     NoteOn {
///         global_time: usize,
///         pitch: u8,
///         channel: u8
///     },
///     NoteOff {
///         global_time: usize,
///         pitch: u8,
///         channel: u8
///     },
///     CC {
///         global_time: usize
///     },
///     Unsupported {
///         global_time: usize,
///         raw_data: Vec<u8>,
///         channel: u8
///     }
/// }
///
/// assert!(
///     [
///         *Note::NoteOn {
///             global_time: 42,
///             // ...
///             #            pitch: 5,
///             #            channel: 3,
///         }.global_time(),
///         *Note::NoteOff {
///             global_time: 42,
///             // ...
///             #            pitch: 2,
///             #            channel: 1,
///         }.global_time(),
///         *Note::CC {
///             global_time: 42,
///         }.global_time(),
///         *Note::Unsupported {
///             global_time: 42,
///             // ...
///             #            raw_data: vec![1, 2, 4, 8],
///             #            channel: 4,
///         }.global_time()
///     ].into_iter().all(|t| t == 42)
/// );
///
/// assert_eq!(
///     Note::NoteOn {
///         // ...
///         #        global_time: 42,
///         pitch: 2,
///         // ...
///         #        channel: 0,
///     }.pitch(),
///     Some(&2)
/// );
///
/// assert_eq!(
///     Note::CC {
///         global_time: 42,
///     }.pitch(),
///     None
/// );
/// ```
///
/// #### Method accessor
///
/// The general form is `#[accessor(method():type)]`.
///
/// As for field access, declaring an exception will make the actual return type an `Option<type>`.
///
/// Named fields is supported, it will consider that the named field is of type `Fn() -> type`, and call it.
///
/// An intricate example:
///
/// ```rust
/// # use sort_by_derive::EnumAccessor;
/// # use std::sync::Arc;
/// # use std::sync::atomic::{AtomicU8, Ordering};
/// #
/// struct A {
///     f1: u8,
///     f2: u8
/// }
///
/// impl A {
///     fn sum(&self) -> u8 {
///         // ...
///         #        self.f1 + self.f2
///     }
///     fn set(&mut self) -> &mut u8 {
///         // ...
///         #        &mut self.f1
///     }
/// }
///
/// struct B {
///     values: Vec<u8>
/// }
///
/// impl B {
///     fn sum(&self) -> u8 {
///         // ...
///         #        self.values.iter().sum()
///     }
/// }
///
/// #[derive(EnumAccessor)]
/// #[accessor(sum():u8)]
/// #[accessor(set(): &mut u8, except(B,C))]
/// enum E<Get: Fn() -> u8> {
///     A(A),
///     B(B),
///     C{sum: Get}
/// }
///
/// let factor = Arc::new(AtomicU8::new(1));
///
/// let [mut a, b, c] = [
/// E::A(A { f1: 10, f2: 22 }),
/// E::B(B { values: vec![9, 4, 3, 2] }),
/// {
/// let factor = factor.clone();
/// E::C {
/// sum: move || 21 * factor.load(Ordering::Relaxed),
/// }
/// }];
///
/// assert_eq!(32, a.sum()); // sum() is available without matching against E::A, E::B or E::C
/// if let Some(value) = a.set() { // set() is only available for E::A and returns a &mut u8, so we get a Option<&mut u8>
/// *value = 0;
/// }
/// assert_eq!(22, a.sum());
/// assert_eq!(18, b.sum());
/// assert_eq!(21, c.sum());
/// factor.store(2, Ordering::Relaxed);
/// assert_eq!(42, c.sum());
/// ```
///
/// ## Limitation
///
/// - On unnamed variants, `EnumAccessor` only considers the first parameter.
#[proc_macro_derive(EnumAccessor, attributes(accessor))]
pub fn enum_variant_accessor_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    enum_variant_accessor::impl_enum_accessor(ast).into()
}

/// Simply derive `EnumSequence`, and you get `enum_sequence(&self)` which returns a `usize`, starting from `0` and incrementing for each variant.
///
/// When using enums of enums, creating an accessor to the inner enum's sequence may create a method name ambiguity. To mitigate this, a custom accessor name can be chosen by using `as`, for instance `#[accessor(enum_sequence() as inner_sequence: usize)]`
///
/// **Note**: this will create an extension trait `{TypeName}EnumSequence` ( i.e. the type `T` will get a new trait `TEnumSequence` ). This trait will have the same visibility as the type. When using this type from another module, make sure to bring the trait in scope with `use {TypeName}EnumSequence`.
///
/// #### Example
///
/// ```rust
/// use sort_by_derive::EnumSequence;
///
/// #[derive(EnumSequence)]
/// enum ABC {
///     A(u8),
///     B(String),
///     C { f: String, g: usize }
/// }
///
/// assert_eq!(ABC::B("hi!".into()).enum_sequence(), 1);
/// ```
#[proc_macro_derive(EnumSequence)]
pub fn enum_sequence_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    enum_sequence::impl_enum_sequence(ast).into()
}
