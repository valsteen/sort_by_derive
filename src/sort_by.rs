use proc_macro2::TokenStream;

use syn::{
    self, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput, Expr, Fields, Lit, Meta,
    NestedMeta,
};

pub fn impl_sort_by_derive(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let struct_name = input.ident.clone();
    let mut sortable_fields = Vec::new();

    for attr in input
        .attrs
        .iter()
        .filter(|i| i.path.get_ident().map(|i| i == "sort_by").is_some())
    {
        match parse_meta(attr) {
            Ok(mut vec) => sortable_fields.append(&mut vec),
            Err(Some(e)) => {
                return e.into_compile_error();
            }
            Err(None) => {
                return syn::Error::new(input.span(), r#"SortBy: invalid sort_by attribute, expected list form i.e #[sort_by(attr1, attr2, ...)]"#)
                    .into_compile_error();
            }
        }
    }

    let fields = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(n),
            ..
        }) => n,
        _ => {
            return syn::Error::new(
                input.span(),
                r#"SortBy: expected a struct with named fields"#,
            )
            .into_compile_error();
        }
    };

    for field in fields.named {
        let mut i = field.attrs.iter();
        if let Some(attr) = i.next() {
            if attr.path.get_ident().filter(|i| *i == "sort_by").is_none() || i.next().is_some() {
                return syn::Error::new(
                    field.span(),
                    r#"SortBy: expected at most one `sort_by` attribute"#,
                )
                .into_compile_error();
            }
            let expr: Expr = syn::parse_str(field.ident.unwrap().to_string().as_str()).unwrap();
            sortable_fields.push(expr)
        } else {
            continue;
        };
    }
    let mut iter_fields = sortable_fields.iter();
    let ord_statement = if let Some(field_name) = iter_fields.next() {
        quote::quote! {
            core::cmp::Ord::cmp(&self.#field_name, &other.#field_name)
        }
    } else {
        return syn::Error::new(
            input_span,
            r#"SortBy: no field to sort on. Mark fields to sort on with #[sort_by]"#,
        )
        .into_compile_error();
    };

    let ord_statement = iter_fields.fold(ord_statement, |ord_statement, field_name| {
        quote::quote! {
            #ord_statement.then_with(|| self.#field_name.cmp(&other.#field_name))
        }
    });

    quote::quote_spanned! {input_span =>
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
                std::option::Option::Some(self.cmp(other))
            }
        }

        impl core::cmp::Ord for #struct_name {
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                #ord_statement
            }
        }
    }
}

fn parse_meta(attr: &Attribute) -> Result<Vec<Expr>, Option<syn::Error>> {
    let mut sortable_fields = Vec::new();
    match attr.parse_meta() {
        Ok(Meta::List(list)) => {
            for name in list.nested {
                match name {
                    NestedMeta::Meta(Meta::Path(p)) => {
                        let expr: Expr =
                            syn::parse_str(p.get_ident().unwrap().to_string().as_str()).unwrap();
                        sortable_fields.push(expr)
                    }
                    NestedMeta::Lit(Lit::Str(l)) => {
                        let expr: Expr = syn::parse_str(l.value().as_str()).unwrap();
                        sortable_fields.push(expr);
                    }
                    _ => return Err(None),
                }
            }
        }
        Ok(_) => return Err(None),
        Err(err) => return Err(Some(err)),
    }
    Ok(sortable_fields)
}

#[cfg(test)]
mod test {
    use rust_format::Formatter;

    #[test]
    fn test_this() {
        let input = syn::parse_quote! {
            #[sort_by("embed.otherfield")]
            struct Toto {
                #[sort_by]
                a: u16,
                #[sort_by]
                c: u32,
                b: f32,
                embed: EmbedStruct
            }
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl std::hash::Hash for Toto {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.embed.otherfield.hash(state);
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
        std::option::Option::Some(self.cmp(other))
    }
}
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.embed.otherfield, &other.embed.otherfield)
            .then_with(|| self.a.cmp(&other.a))
            .then_with(|| self.c.cmp(&other.c))
    }
}
"#
        );
    }
}
