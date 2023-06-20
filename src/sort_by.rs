use proc_macro2::{Literal, Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use syn::{
    self, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Fields,
    FieldsNamed, FieldsUnnamed, GenericParam, Lit, Meta, NestedMeta,
};

const HELP_SORTBY: &str = r#"SortBy: invalid sort_by attribute, expected list form i.e #[sort_by(attr1, attr2, methodcall())]"#;

pub fn impl_sort_by_derive(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let struct_name = input.ident.clone();

    let mut sortable_expressions = vec![];

    for attr in input
        .attrs
        .iter()
        .filter(|i| i.path.get_ident().map(|i| i == "sort_by") == Some(true))
    {
        match parse_outer(attr) {
            Ok(mut vec) => sortable_expressions.append(&mut vec),
            Err(None) => {
                return Error::new(attr.tokens.span(), HELP_SORTBY).into_compile_error();
            }
            Err(Some((span, message))) => {
                return Error::new(span, message).into_compile_error();
            }
        }
    }

    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => match parse_named_fields(fields) {
            Ok(mut result) => sortable_expressions.append(&mut result),
            Err(e) => return e.into_compile_error(),
        },
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(fields),
            ..
        }) => match parse_unnamed_fields(fields) {
            Ok(mut result) => sortable_expressions.append(&mut result),
            Err(e) => return e.into_compile_error(),
        },
        Data::Enum(_) => (),
        _ => {
            return Error::new(input_span, r#"SortBy: expected an enum or a struct"#)
                .into_compile_error();
        }
    };

    let mut iter_sort_expressions = sortable_expressions.iter();
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

    let hash_expressions: Vec<Expr> = sortable_expressions
        .iter()
        .map(|expr| syn::parse_quote_spanned!(expr.span() => self.#expr.hash(state)))
        .collect();

    let generics = &input.generics;
    let where_clause = &input.generics.where_clause;
    let generics_params = &input
        .generics
        .params
        .iter()
        .flat_map(|p| match p {
            GenericParam::Type(t) => Some(t.ident.to_token_stream()),
            GenericParam::Const(t) => Some(t.ident.to_token_stream()),
            GenericParam::Lifetime(t) => Some(t.lifetime.to_token_stream()),
        })
        .collect::<Vec<_>>();

    quote_spanned! {input_span =>
        impl #generics std::hash::Hash for #struct_name <#(#generics_params),*> #where_clause {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                #(#hash_expressions);*;
            }
        }

        impl #generics core::cmp::Eq for #struct_name <#(#generics_params),*> #where_clause {}

        impl #generics core::cmp::PartialEq<Self> for #struct_name <#(#generics_params),*> #where_clause {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other).is_eq()
            }
        }

        impl #generics core::cmp::PartialOrd<Self> for #struct_name <#(#generics_params),*> #where_clause {
            fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
                std::option::Option::Some(self.cmp(other))
            }
        }

        impl #generics core::cmp::Ord for #struct_name <#(#generics_params),*> #where_clause {
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                #ord_statement
            }
        }
    }
}

fn parse_unnamed_fields(fields: FieldsUnnamed) -> Result<Vec<Expr>, Error> {
    let mut sortable_expressions = vec![];

    for (index, field) in fields.unnamed.iter().enumerate() {
        let span = field.span();
        let mut attrs = field
            .attrs
            .iter()
            .filter(|i| i.path.get_ident().map_or(false, |i| i == "sort_by"));

        if attrs.next().is_none() {
            continue;
        }

        let expr: Expr = syn::parse2(Literal::usize_unsuffixed(index).to_token_stream())?;
        sortable_expressions.push(expr);

        if attrs.next().is_some() {
            return Err(Error::new(
                span,
                r#"SortBy: expected at most one `sort_by` attribute"#,
            ));
        }
    }
    Ok(sortable_expressions)
}

fn parse_named_fields(fields: FieldsNamed) -> Result<Vec<Expr>, Error> {
    let mut sortable_expressions = vec![];

    for field in fields.named {
        let span = field.span();
        let mut attrs = field
            .attrs
            .iter()
            .filter(|i| i.path.get_ident().map(|i| i == "sort_by") == Some(true));

        if attrs.next().is_none() {
            continue;
        }

        let expr: Expr = syn::parse2(field.ident.to_token_stream())?;
        sortable_expressions.push(expr);

        if attrs.next().is_some() {
            return Err(Error::new(
                span,
                r#"SortBy: expected at most one `sort_by` attribute"#,
            ));
        }
    }
    Ok(sortable_expressions)
}

fn parse_outer(attr: &Attribute) -> Result<Vec<Expr>, Option<(Span, String)>> {
    if let Ok(Meta::List(list)) = attr.parse_meta() {
        let mut sortable_fields = Vec::new();
        let mut valid = true;
        for name in list.nested {
            match name {
                NestedMeta::Meta(Meta::Path(p)) => {
                    let expr: Expr = syn::parse2(p.get_ident().to_token_stream())
                        .map_err(|err| (p.span(), err.to_string()))?;
                    sortable_fields.push(expr)
                }
                NestedMeta::Lit(Lit::Str(l)) => {
                    sortable_fields.push(l.parse().map_err(|err| (l.span(), err.to_string()))?);
                }
                _ => {
                    valid = false;
                    break;
                }
            }
        }
        if valid {
            return Ok(sortable_fields);
        }
    }

    match syn::parse2::<Expr>(attr.tokens.clone()) {
        Ok(Expr::Tuple(tuple)) => {
            let elems = tuple.elems.into_iter().map(|elem| match elem {
                Expr::Lit(ExprLit {
                              lit: Lit::Str(lit), ..
                          }) => lit.parse().map_err(|e| Some((lit.span(), e.to_string()))),
                Expr::Call(_) | Expr::Field(_) | Expr::Path(_) | Expr::MethodCall(_) => {
                    Ok(elem)
                }
                _ => Err(Some((elem.span(), format!("Invalid form: `{}`.\nAllowed forms: `field`, `method()`, `inner.field`, `inner.method()", elem.to_token_stream()))))
            });
            elems.collect::<Result<_, _>>()
        }
        Ok(Expr::Paren(expr)) => Ok(vec![*expr.expr]),
        _ => Err(None),
    }
}

#[cfg(test)]
mod test {
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
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl std::hash::Hash for Toto {
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
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.this, &other.this)
            .then_with(|| self.this.that.cmp(&other.this.that))
            .then_with(|| self.get_something().cmp(&other.get_something()))
            .then_with(|| self.something.do_this().cmp(&other.something.do_this()))
    }
}
"#
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
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl std::hash::Hash for Toto {
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
impl core::cmp::Ord for Toto {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.get_something(), &other.get_something())
    }
}
"#
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
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl<'a, T> std::hash::Hash for ContextWrapper<'a, T>
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
impl<'a, T> core::cmp::Ord for ContextWrapper<'a, T>
where
    T: Ctx,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.elapsed, &other.elapsed)
    }
}
"#
        );
    }

    #[test]
    fn test_tuple_struct() {
        let input = syn::parse_quote! {
            #[sort_by(somemethod(), literal, some.path)]
            struct Something (
              #[sort_by]
              u16,
              #[sort_by]
              u32,
              f32,
            );
        };

        let output = crate::sort_by::impl_sort_by_derive(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl std::hash::Hash for Something {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.somemethod().hash(state);
        self.literal.hash(state);
        self.some.path.hash(state);
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
impl core::cmp::Ord for Something {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        core::cmp::Ord::cmp(&self.somemethod(), &other.somemethod())
            .then_with(|| self.literal.cmp(&other.literal))
            .then_with(|| self.some.path.cmp(&other.some.path))
            .then_with(|| self.0.cmp(&other.0))
            .then_with(|| self.1.cmp(&other.1))
    }
}
"#
        );
    }
}
