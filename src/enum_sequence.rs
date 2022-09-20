use proc_macro2::{Ident, TokenStream};

use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields};

const ENUM_HELP: &str = "EnumSequence: Only enums are supported";

pub fn impl_enum_sequence(input: DeriveInput) -> TokenStream {
    let ident = input.ident.clone();
    let input_span = input.span();
    let trait_ident = Ident::new(format!("{ident}EnumSequence").as_str(), input_span);

    let enu = match input.data {
        Data::Enum(enu) => enu,
        _ => {
            return syn::Error::new(input_span, ENUM_HELP).into_compile_error();
        }
    };

    let mut match_branches = Vec::new();

    for (i, variant) in enu.variants.iter().enumerate() {
        let span = variant.span();
        let ident = &variant.ident;

        let pattern = match &variant.fields {
            Fields::Named(_) => quote::quote_spanned!(span => Self::#ident{..}),
            Fields::Unnamed(_) => quote::quote_spanned!(span => Self::#ident(..)),
            Fields::Unit => quote::quote_spanned!(span => Self::#ident),
        };

        match_branches.push(quote::quote_spanned! { span =>
            #pattern => #i
        })
    }

    quote::quote_spanned! {input_span =>
        pub trait #trait_ident {
            fn seq(&self) -> usize;
        }

        impl #trait_ident for #ident {
            fn seq(&self) -> usize {
                match self {
                    #(#match_branches),*
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use rust_format::Formatter;

    #[test]
    fn test_this() {
        let input = syn::parse_quote! {
            enum E {
                A,
                B(u8),
                C { field1: bool, field2: u16 }
            }
        };

        let output = crate::enum_sequence::impl_enum_sequence(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();

        assert_eq!(
            r#"pub trait EEnumSequence {
    fn seq(&self) -> usize;
}
impl EEnumSequence for E {
    fn seq(&self) -> usize {
        match self {
            Self::A => 0usize,
            Self::B(..) => 1usize,
            Self::C { .. } => 2usize,
        }
    }
}
"#,
            output
        )
    }
}
