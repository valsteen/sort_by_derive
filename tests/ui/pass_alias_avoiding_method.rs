use sort_by_derive::EnumAccessor;

#[derive(EnumAccessor)]
#[accessor(a: u16, except(Variant3))]
#[accessor(b as b_field: u32, except(Variant1,Variant2))]
enum E {
    Variant1 { a: u16 },
    Variant2 { a: u16, c: u32 },
    Variant3 { b: u32 },
}

impl E {
    fn b(&self) {}
}

fn main() {}
