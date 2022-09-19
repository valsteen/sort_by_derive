use proc_macro2::TokenStream;

use syn::{self, parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Fields};

#[proc_macro_derive(SortBy, attributes(sort_by))]
pub fn sort_by_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    impl_sort_by_derive(ast).into()
}

fn impl_sort_by_derive(input: DeriveInput) -> TokenStream {
    let input_span = input.span().clone();
    let struct_name = input.ident.clone();
    let fields = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(n),
            ..
        }) => n,
        _ => {
            return syn::Error::new(input.span(), r#"expected a struct with named fields"#)
                .into_compile_error()
                .into();
        }
    };

    let mut sortable_fields = Vec::new();

    for field in fields.named {
        let mut i = field.attrs.iter();
        if let Some(ident) = i.next() {
            if ident.path.get_ident().filter(|i| *i == "sort_by").is_none() || i.next().is_some() {
                return syn::Error::new(
                    field.span(),
                    r#"expected at most one `sort_by` attribute"#,
                )
                .into_compile_error()
                .into();
            }
            sortable_fields.push(field.ident.unwrap())
        } else {
            continue;
        };
    }
    let mut iter_fields = sortable_fields.iter();
    let ord_statement = if let Some(field_name) = iter_fields.next() {
        quote::quote! {
            self.#field_name.cmp(&other.#field_name)
        }
    } else {
        return syn::Error::new(
            input_span,
            r#"no field to sort on. Mark fields to sort on with #[sort_by]"#,
        )
        .into_compile_error()
        .into();
    };

    let ord_statement = iter_fields.fold(ord_statement, |ord_statement, field_name| {
        quote::quote! {
            #ord_statement.then_with(|| self.#field_name.cmp(&other.#field_name))
        }
    });

    quote::quote! {
        impl std::hash::Hash for #struct_name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                #(self.#sortable_fields.hash(state));*;
            }
        }

        impl core::cmp::Eq for #struct_name {}

        impl core::cmp::PartialEq<Self> for #struct_name {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other).is_eq()
            }
        }

        impl core::cmp::PartialOrd<Self> for #struct_name {
            fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl core::cmp::Ord for #struct_name {
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                #ord_statement
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::impl_sort_by_derive;
    use rust_format::Formatter;

    #[test]
    fn test_this() {
        let input = quote::quote! {
            #[derive(SortBy)]
            struct Toto {
                #[sort_by]
                a: u16,
                #[sort_by]
                c: u32,
                b: f32
            }
        };

        let output = impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl std::hash::Hash for Toto {
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
"#
        );
    }
}
