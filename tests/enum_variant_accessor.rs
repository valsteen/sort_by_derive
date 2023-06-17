use sort_by_derive::EnumAccessor;

#[test]
fn test_alias() {
    #[derive(EnumAccessor)]
    #[accessor(a: u16, except(Variant3))]
    #[accessor(a as a_big: u32, except(Variant1,Variant2))]
    enum E {
        Variant1 { a: u16 },
        Variant2 { a: u16, c: u32 },
        Variant3 { a: u32 },
    }

    assert_eq!(E::Variant1 { a: 1 }.a(), Some(&1));
    assert_eq!(E::Variant2 { a: 1, c: 0 }.a(), Some(&1));
    assert_eq!(E::Variant3 { a: 0 }.a(), None);
    assert_eq!(E::Variant3 { a: 2 }.a_big(), Some(&2));
    assert_eq!(E::Variant1 { a: 0 }.a_big(), None);
}
