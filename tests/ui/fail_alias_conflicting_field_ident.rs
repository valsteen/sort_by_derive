use sort_by_derive::EnumAccessor;

#[derive(EnumAccessor)]
#[accessor(b: u16, except(Variant3))]
#[accessor(a as b: u32, except(Variant1,Variant2))]
enum E {
    Variant1 { b: u16 },
    Variant2 { b: u16, c: u32 },
    Variant3 { a: u32 },
}

fn main() {}
