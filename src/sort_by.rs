use proc_macro2::{Literal, TokenStream};
use quote::{quote_spanned, ToTokens};
use std::str::FromStr;
use syn::{
    self, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprCall, ExprLit, Field, Fields,
    FieldsNamed, FieldsUnnamed, GenericParam, Lit, LitBool, Meta, PathSegment,
};

const HELP_SORT_BY: &str =
    r"SortBy: invalid sort_by attribute, expected list form i.e #[sort_by(attr1, attr2, methodcall())]";

pub fn impl_sort_by_derive(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let struct_name = input.ident.clone();

    let mut sortables = vec![];
    let mut derive_eq = true;
    for attr in input
        .attrs
        .into_iter()
        .filter(|i| i.path().get_ident().map(|i| i == "sort_by") == Some(true))
    {
        let (should_derive_eq, mut vec) = match parse_outer(attr) {
            Ok((should_derive_eq, vec)) => (should_derive_eq, vec),
            Err(e) => return e.into_compile_error(),
        };
        derive_eq = should_derive_eq;
        sortables.append(&mut vec);
    }

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => match parse_named_fields(fields) {
            Ok(mut result) => sortables.append(&mut result),
            Err(e) => return e.into_compile_error(),
        },
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(fields),
            ..
        }) => match parse_unnamed_fields(fields) {
            Ok(mut result) => sortables.append(&mut result),
            Err(e) => return e.into_compile_error(),
        },
        Data::Enum(_) => (),
        _ => {
            return Error::new(input_span, r"SortBy: expected an enum or a struct").into_compile_error();
        }
    };

    let mut iter_sort_expressions = sortables.iter();
    let ord_statement = if let Some(field) = iter_sort_expressions.next() {
        quote_spanned! { field.span() =>
            core::cmp::Ord::cmp(&self.#field, &other.#field)
        }
    } else {
        return quote::quote! {
            compile_error!(r#"SortBy: no field to sort on. Mark fields to sort on with #[sort_by]"#);
        };
    };

    let ord_statement = iter_sort_expressions.fold(ord_statement, |ord_statement, field| {
        syn::parse_quote_spanned! {field.span() =>
            #ord_statement.then_with(|| self.#field.cmp(&other.#field))
        }
    });

    let hash_expressions: Vec<_> = sortables
        .iter()
        .map(|expr| quote_spanned!(expr.span() => self.#expr.hash(state)))
        .collect();

    let generics = &input.generics;
    let where_clause = &input.generics.where_clause;
    let generics_params = &input
        .generics
        .params
        .iter()
        .map(|p| match p {
            GenericParam::Type(t) => t.ident.to_token_stream(),
            GenericParam::Const(t) => t.ident.to_token_stream(),
            GenericParam::Lifetime(t) => t.lifetime.to_token_stream(),
        })
        .collect::<Vec<_>>();

    let derived_eq = if derive_eq {
        quote_spanned! {input_span =>
            impl #generics core::cmp::Eq for #struct_name <#(#generics_params),*> #where_clause {}

            impl #generics core::cmp::PartialEq<Self> for #struct_name <#(#generics_params),*> #where_clause {
                fn eq(&self, other: &Self) -> bool {
                    self.cmp(other).is_eq()
                }
            }
        }
    } else {
        TokenStream::new()
    };

    quote_spanned! {input_span =>
        impl #generics std::hash::Hash for #struct_name <#(#generics_params),*> #where_clause {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                #(#hash_expressions);*;
            }
        }

       #derived_eq

        impl #generics core::cmp::PartialOrd<Self> for #struct_name <#(#generics_params),*> #where_clause {
            fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
                std::option::Option::Some(self.cmp(other))
            }
        }

        #[allow(clippy::needless_borrow)]
        impl #generics core::cmp::Ord for #struct_name <#(#generics_params),*> #where_clause {
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                #ord_statement
            }
        }
    }
}

fn field_is_sortable(field: &Field) -> Result<bool, Error> {
    field.attrs.iter().try_fold(false, |found, attribute| match attribute {
        Attribute {
            meta: Meta::Path(path), ..
        } => {
            let mut segments = path.segments.iter();
            if let Some(PathSegment { ident, .. }) = segments.next() {
                if ident == "sort_by" {
                    if found {
                        return Err(Error::new(
                            ident.span(),
                            r"SortBy: expected at most one `sort_by` attribute",
                        ));
                    }
                    if let Some(next_segment) = segments.next() {
                        return Err(Error::new(next_segment.span(), r"sort_by does not take parameters"));
                    }
                    return Ok(true);
                }
            }
            Ok(found)
        }
        _ => Ok(found),
    })
}

fn parse_unnamed_fields(fields: FieldsUnnamed) -> Result<Vec<TokenStream>, Error> {
    fields
        .unnamed
        .into_iter()
        .enumerate()
        .filter_map(|(index, field)| {
            field_is_sortable(&field)
                .map(|f| f.then(|| Literal::usize_unsuffixed(index).to_token_stream()))
                .transpose()
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_named_fields(fields: FieldsNamed) -> Result<Vec<TokenStream>, Error> {
    fields
        .named
        .into_iter()
        .filter_map(|field| {
            field_is_sortable(&field)
                .map(|f| f.then(|| field.ident.to_token_stream()))
                .transpose()
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_outer(attr: Attribute) -> Result<(bool, Vec<TokenStream>), Error> {
    let Meta::List(list) = attr.meta else {
        return Err(Error::new(attr.span(), HELP_SORT_BY));
    };

    let mut derive_eq = true;

    let sortable_fields = syn::parse2::<ExprCall>(list.into_token_stream())?
        .args
        .into_iter()
        .filter_map(|arg| match &arg {
            Expr::Assign(assignment) => {
                let Expr::Path(left)=assignment.left.as_ref() else {
                    return Some(Err(Error::new(assignment.span(), "Unexpected token")))
                };
                if left.path.segments.len() != 1 || left.path.segments[0].ident != "derive_eq" {
                    return Some(Err(Error::new(
                        assignment.span(),
                        "only derive_eq=true|false is supported",
                    )));
                }

                let Expr::Lit(ExprLit{  lit: Lit::Bool(LitBool{ value: bool, .. }),.. }) = assignment.right.as_ref() else {
                    return Some(Err(Error::new(assignment.right.span(), "only derive_eq=true|false is supported")))
                };

                derive_eq = *bool;

                None
            }
            Expr::Lit(ExprLit{ lit: Lit::Str(s), .. }) => {
                match TokenStream::from_str(&s.value()) {
                    Ok(value) => {
                        let tokens = value.into_iter().map(|mut t| {
                            t.set_span(arg.span());
                            t
                        }).collect::<TokenStream>();

                        Some(Ok(tokens))
                    },
                    Err(e) => Some(Err(e.into()))
                }
            }
            arg @ (Expr::Field(_) | Expr::Call(_) | Expr::Lit(_) | Expr::Path(_) | Expr::MethodCall(_)) => {
                Some(Ok(arg.to_token_stream()))
            },
            token => {
                Some(Err(Error::new(token.span(), "Unexpected token")))
            },
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((derive_eq, sortable_fields))
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use rust_format::Formatter;

    #[test]
    fn test_struct() {
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r"impl std::hash::Hash for Toto {
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
#[allow(clippy::needless_borrow)]
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.embed.otherfield, &other.embed.otherfield)
            .then_with(|| self.a.cmp(&other.a))
            .then_with(|| self.c.cmp(&other.c))
    }
}
"
        );
    }

    #[test]
    fn test_enum() {
        let input = syn::parse_quote! {
            #[sort_by(this, this.that, get_something(), something.do_this())]
            #[accessor(global_time: usize)]
            enum Toto {
                A(u32),
                B,
                G { doesnotmatter: String, anyway: usize }
            }
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r"impl std::hash::Hash for Toto {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.this.hash(state);
        self.this.that.hash(state);
        self.get_something().hash(state);
        self.something.do_this().hash(state);
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
#[allow(clippy::needless_borrow)]
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.this, &other.this)
            .then_with(|| self.this.that.cmp(&other.this.that))
            .then_with(|| self.get_something().cmp(&other.get_something()))
            .then_with(|| self.something.do_this().cmp(&other.something.do_this()))
    }
}
"
        );
    }

    #[test]
    fn test_singlecall() {
        let input = syn::parse_quote! {
            #[sort_by(get_something())]
            #[accessor(global_time: usize)]
            enum Toto {
                A(u32),
                B,
                G { doesnotmatter: String, anyway: usize }
            }
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r"impl std::hash::Hash for Toto {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_something().hash(state);
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
#[allow(clippy::needless_borrow)]
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.get_something(), &other.get_something())
    }
}
"
        );
    }

    #[test]
    fn test_lifetime() {
        let input = syn::parse_quote! {
            #[derive(SortBy)]
            pub struct ContextWrapper<'a, T>
            where T: Ctx,
            {
                ctx: Cow<'a, T>,
                #[sort_by]
                elapsed: i32,
            }
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r"impl<'a, T> std::hash::Hash for ContextWrapper<'a, T>
where
    T: Ctx,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.elapsed.hash(state);
    }
}
impl<'a, T> core::cmp::Eq for ContextWrapper<'a, T> where T: Ctx {}
impl<'a, T> core::cmp::PartialEq<Self> for ContextWrapper<'a, T>
where
    T: Ctx,
{
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl<'a, T> core::cmp::PartialOrd<Self> for ContextWrapper<'a, T>
where
    T: Ctx,
{
    fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
        std::option::Option::Some(self.cmp(other))
    }
}
#[allow(clippy::needless_borrow)]
impl<'a, T> core::cmp::Ord for ContextWrapper<'a, T>
where
    T: Ctx,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.elapsed, &other.elapsed)
    }
}
"
        );
    }
    #[test]
    fn test_tuple_struct() {
        let input = syn::parse_quote! {
            #[sort_by(somemethod(), literal, some.path, 2)]
            struct Something (
              #[sort_by]
              u16,
              #[sort_by]
              u32,
              f32,
            );
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r"impl std::hash::Hash for Something {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.somemethod().hash(state);
        self.literal.hash(state);
        self.some.path.hash(state);
        self.2.hash(state);
        self.0.hash(state);
        self.1.hash(state);
    }
}
impl core::cmp::Eq for Something {}
impl core::cmp::PartialEq<Self> for Something {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}
impl core::cmp::PartialOrd<Self> for Something {
    fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
        std::option::Option::Some(self.cmp(other))
    }
}
#[allow(clippy::needless_borrow)]
impl core::cmp::Ord for Something {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.somemethod(), &other.somemethod())
            .then_with(|| self.literal.cmp(&other.literal))
            .then_with(|| self.some.path.cmp(&other.some.path))
            .then_with(|| self.2.cmp(&other.2))
            .then_with(|| self.0.cmp(&other.0))
            .then_with(|| self.1.cmp(&other.1))
    }
}
"
        );
    }
}
