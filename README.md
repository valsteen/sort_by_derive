# sort_by_derive

This crate provides 3 derive macros `SortBy`, `EnumAccessor` and `EnumSequence`.

- `SortBy` derives the traits `Ord`, `PartialOrd`, `Eq`, `PartialEq` and `Hash` on structs that can't automatically derive those traits because they contain unorderable fields such as `f32`.
- On enums and structs, `SortBy` can also implement a `Ord` trait that calls arbitrary methods - this is particularly useful in combination with enum variant accessor methods derived by `EnumAccessor` an `EnumSequence`
- `EnumAccessor` derives accessor methods to common fields in variants - so you don't need to write yourself `match` statements to access a field with the same name and type on different variants.
- `EnumSequence` provides a `enum_sequence` method where the first variant returns `0`, the second `1`, etc. This is useful is you want to implement a custom sorting, while the order of declaration of variant is still relevant as a secondary ordering criteria.

## Usage

### SortBy

Fields that should be used for sorting are marked with the attribute `#[sort_by]`. Other fields will be ignored.

Alternatively, or in combination with, a struct-level or enum-level `#[sort_by(method1(),method2(),attr1,nested.attr)]` can be declared. This top-level declaration takes precedence,
fields comparison will be considered if top-level comparisons are all `eq`. The top-level `sort_by` attribute takes a list of attributes or method calls; items will be prepended with `self.`.

#### Example

```rust
#[derive(SortBy)]
#[sort_by(somemethod())]
struct Something {
    #[sort_by]
    a: u16,
    #[sort_by]
    c: u32,
    b: f32
}
```

will expand to:

```rust
impl std::hash::Hash for Something {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.somemethod().hash(state);
        self.a.hash(state);
        self.c.hash(state);
    }
}
impl core::cmp::Eq for Something {}
impl core::cmp::PartialEq<Self> for Something {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl core::cmp::PartialOrd<Self> for Something {
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> core::option::Option<core::cmp::Ordering> {
        std::option::Option::Some(self.cmp(other))
    }
}
impl core::cmp::Ord for Something {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.somemethod(), &other.somemethod())
            .then_with(|| self.a.cmp(&other.a))
            .then_with(|| self.c.cmp(&other.c))
    }
}
```

### EnumAccessor

Attributes are declared at top-level.

- `#[accessor(name: type)]` will derive the accessor methods `fn name(&self) -> &type;` and`fn name_mut(&mut self) -> &mut type;`, and return a reference to the field of the same name on any variant.
- `#[accessor(name: type, (Exception1,Exception2))]` derive the same accessor methods, but the return type will be `Option<&type>` and `Option<&mut type>`. The provided comma-separated list of variants are exceptions and will return `None`.

Methods without arguments ( i.e. only `&self` are also supported ). It takes the form: `#[accessor(method_name(): type)]`.

#### Example

Say we have a series of midi events, they are very similar but with slight variations - they always have some timing information but they may not always have a pitch or channel. 

Using `#[accessor(global_time: usize)]`, a `global_time(&self)` method is derived, along with a `global_time_mut(&mut self)`, so without any boilerplate you can access the timing.

By declaring `#[accessor(channel: u8, (CC))]`, `channel(&self)` and `channel_mut(&mut self)` are derived, but they return `Some` for `NoteOn` and `NoteOff`, and `None` for `CC` and `Unsupported`.   


```rust
#[derive(EnumAccessor)]
#[accessor(global_time: usize)]
#[accessor(channel: u8, (CC))]
#[accessor(pitch: u8, (CC, Unsupported))]
enum Note {
    NoteOn(NoteOn),
    NoteOff(NoteOff),
    CC(CC),
    Unsupported {
        global_time: usize,
        rawdata: Vec<u8>
    }
}
```

expands to:

```rust
pub trait NoteAccessor {
    fn global_time(&self) -> &usize;
    fn global_time_mut(&mut self) -> &mut usize;
    fn channel(&self) -> std::option::Option<&u8>;
    fn channel_mut(&mut self) -> std::option::Option<&mut u8>;
    fn pitch(&self) -> std::option::Option<&u8>;
    fn pitch_mut(&mut self) -> std::option::Option<&mut u8>;
}
impl NoteAccessor for Note {
    fn global_time(&self) -> &usize {
        match self {
            Self::NoteOn(x) => &x.global_time,
            Self::NoteOff(x) => &x.global_time,
            Self::CC(x) => &x.global_time,
            Self::Unsupported { global_time, .. } => global_time,
        }
    }
    fn global_time_mut(&mut self) -> &mut usize {
        match self {
            Self::NoteOn(x) => &mut x.global_time,
            Self::NoteOff(x) => &mut x.global_time,
            Self::CC(x) => &mut x.global_time,
            Self::Unsupported { global_time, .. } => global_time,
        }
    }
    fn channel(&self) -> std::option::Option<&u8> {
        match self {
            Self::NoteOn(x) => std::option::Option::Some(&x.channel),
            Self::NoteOff(x) => std::option::Option::Some(&x.channel),
            Self::CC(x) => std::option::Option::Some(&x.channel),
            Self::Unsupported { .. } => std::option::Option::None,
        }
    }
    fn channel_mut(&mut self) -> std::option::Option<&mut u8> {
        match self {
            Self::NoteOn(x) => std::option::Option::Some(&mut x.channel),
            Self::NoteOff(x) => std::option::Option::Some(&mut x.channel),
            Self::CC(x) => std::option::Option::Some(&mut x.channel),
            Self::Unsupported { .. } => std::option::Option::None,
        }
    }
    fn pitch(&self) -> std::option::Option<&u8> {
        match self {
            Self::NoteOn(x) => std::option::Option::Some(&x.pitch),
            Self::NoteOff(x) => std::option::Option::Some(&x.pitch),
            Self::CC(_) => std::option::Option::None,
            Self::Unsupported { .. } => std::option::Option::None,
        }
    }
    fn pitch_mut(&mut self) -> std::option::Option<&mut u8> {
        match self {
            Self::NoteOn(x) => std::option::Option::Some(&mut x.pitch),
            Self::NoteOff(x) => std::option::Option::Some(&mut x.pitch),
            Self::CC(_) => std::option::Option::None,
            Self::Unsupported { .. } => std::option::Option::None,
        }
    }
}
```

#### Example of method call

The General form is `#[accessor(method():type)]` :

```rust
#[derive(EnumAccessor)]
#[accessor(method():type)]
enum E {

}
```

As for field access, declaring an exception will make the actual return type an `Option<type>`.

Named fields is supported, it will consider that the named field is of type `Fn() -> type`, and call it.

An intricate example:

```rust
struct A {
    f1: u8,
    f2: u8
}

impl A {
    fn sum(&self) -> u8 {
        self.f1 + self.f2
    }
}

struct B {
    values: Vec<u8>
}

impl B {
    fn sum(&self) -> u8 {
        self.values.iter().sum()
    }
}

#[derive(EnumAccessor)]
#[accessor(sum():u8)]
enum E {
    A(A),
    B(B),
    C{sum: Box<dyn Fn() -> u8>}
}

#[test]
fn test_sum() {
    let a = E::A(A{ f1: 10, f2: 22 });
    let b = E::B(B{ values: vec![9,4,3,2] });
    let factor = Arc::new(AtomicU8::new(1));

    let c = {
        let factor = factor.clone();
        E::C { sum: Box::new(move || 21 * factor.load(Ordering::Relaxed)) }
    };

    assert_eq!(32, a.sum());
    assert_eq!(18, b.sum());
    assert_eq!(21, c.sum());
    factor.store(2, Ordering::Relaxed);
    assert_eq!(42, c.sum());
}
```

### EnumAccessor

Simply derive `EnumSequence`, and you get `enum_sequence(&self)` which returns a `usize`, starting from `0` and incrementing for each variant.

#### Example

```rust
#[derive(EnumSequence)]
enum ABC {
    A(u8),
    B(String),
    C{f: String, g: usize}
}
```

expands to


```rust
pub trait ABCEnumSequence {
    fn enum_sequence(&self) -> usize;
}
impl ABCEnumSequence for ABC {
    fn enum_sequence(&self) -> usize {
        match self {
            Self::A(..) => 0usize,
            Self::B(..) => 1usize,
            Self::C { .. } => 2usize,
        }
    }
}
```

## All together

Imagine the following :

```rust
#[derive(EnumSequence, EnumAccessor, SortBy, Debug)]
#[accessor(global_time: usize)]
#[accessor(channel: u8, (CC))]
#[accessor(pitch: u8, (CC,SomethingElse))]
#[sort_by(global_time(), channel(), pitch(), enum_sequence())]
enum Note {
    NoteOn(NoteOn),
    NoteOff(NoteOff),
    CC(CC),
    SomethingElse {
        global_time: usize,
        channel: u8,
    }
}
```

Now I have a `Note` enum that will sort by `global_time`, `channel`, `pitch`, and lastly by variant order ( `enum_sequence` ). Note that `None` is always less than `Some`.

Conversely, separate structs such as `NoteOn` may derive from `SortBy` in order to ignore some fields ( ex: `velocity` may be a `f32`, so we can't directly derive `Ord` ).

## Limitations

- On unnamed variants, `EnumAccessor` only considers the first parameter.
- struct-level `sort_by` attribute always come before field-level attributes.
- Generated trait impl do not yet carry the generic arguments, and as a consequence generic struct and enums are not yet supported.