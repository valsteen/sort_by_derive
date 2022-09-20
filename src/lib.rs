use syn::{parse_macro_input, DeriveInput};

mod enum_sequence;
mod enum_variant_accessor;
mod sort_by;

#[proc_macro_derive(SortBy, attributes(sort_by))]
pub fn sort_by_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    sort_by::impl_sort_by_derive(ast).into()
}

#[proc_macro_derive(EnumAccessor, attributes(accessor))]
pub fn enum_variant_accessor_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    enum_variant_accessor::impl_enum_accessor(ast).into()
}

#[proc_macro_derive(EnumSequence)]
pub fn enum_sequence_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    enum_sequence::impl_enum_sequence(ast).into()
}
