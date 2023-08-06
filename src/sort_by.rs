use proc_macro2::Delimiter::Parenthesis;
use proc_macro2::{Literal, Punct, Spacing, TokenStream, TokenTree};
use quote::{quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    self, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field, Fields, FieldsNamed,
    FieldsUnnamed, GenericParam, Lit, Meta, PathSegment,
};

const HELP_SORT_BY: &str =
    r#"SortBy: invalid sort_by attribute, expected list form i.e #[sort_by(attr1, attr2, methodcall())]"#;

pub fn impl_sort_by_derive(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let struct_name = input.ident.clone();

    let mut sortables = vec![];

    for attr in input
        .attrs
        .into_iter()
        .filter(|i| i.path().get_ident().map(|i| i == "sort_by") == Some(true))
    {
        match parse_outer(attr) {
            Ok(mut vec) => sortables.append(&mut vec),
            Err(e) => return e.into_compile_error(),
        }
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
            return Error::new(input_span, r#"SortBy: expected an enum or a struct"#).into_compile_error();
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

    let hash_expressions: Vec<Expr> = sortables
        .iter()
        .map(|expr| syn::parse_quote_spanned!(expr.span() => self.#expr.hash(state)))
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
                            r#"SortBy: expected at most one `sort_by` attribute"#,
                        ));
                    }
                    if let Some(next_segment) = segments.next() {
                        return Err(Error::new(next_segment.span(), r#"sort_by does not take parameters"#));
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

fn parse_outer(attr: Attribute) -> Result<Vec<TokenStream>, Error> {
    let Meta::List(list) = attr.meta else {
        return Err(Error::new(attr.span(), HELP_SORT_BY));
    };

    let mut sortable_fields: Vec<TokenStream> = Vec::new();

    let mut paren_expected = false;
    let mut dot_expected = false;
    let mut dot_provided = false;

    for token_tree in list.tokens {
        let span = token_tree.span();
        (paren_expected, dot_expected, dot_provided) = match token_tree {
            TokenTree::Group(group) if group.delimiter() != Parenthesis => {
                return Err(Error::new(span, "Unexpected delimiter"));
            }
            TokenTree::Group(_) if !paren_expected => {
                return Err(Error::new(token_tree.span(), "Call can only follow an identifier"));
            }
            TokenTree::Group(group) if !group.stream().is_empty() => {
                return Err(Error::new(span, "Method calls cannot have arguments"));
            }
            TokenTree::Group(_) => {
                sortable_fields.last_mut().unwrap().append(token_tree);
                (false, false, false)
            }
            TokenTree::Literal(lit) => match syn::parse2::<Expr>(lit.into_token_stream())? {
                Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => {
                    sortable_fields.push(s.parse::<Expr>()?.to_token_stream());
                    (false, false, false)
                }
                Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) => {
                    sortable_fields.push(i.to_token_stream());
                    (false, false, false)
                }
                _ => {
                    return Err(Error::new(span, "invalid expression"));
                }
            },
            TokenTree::Ident(_) if dot_provided => {
                sortable_fields
                    .last_mut()
                    .unwrap()
                    .extend([TokenTree::Punct(Punct::new('.', Spacing::Alone)), token_tree]);
                (true, true, false)
            }
            TokenTree::Ident(_) => {
                sortable_fields.push(token_tree.to_token_stream());
                (true, true, false)
            }
            TokenTree::Punct(p) if p.as_char() == ',' => (false, false, false),
            TokenTree::Punct(p) if dot_expected && p.as_char() == '.' => (false, false, true),
            TokenTree::Punct(_) => {
                return Err(Error::new(span, "Unexpected delimiter"));
            }
        };
    }
    Ok(sortable_fields)
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
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
#[allow(clippy::needless_borrow)]
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
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
#[allow(clippy::needless_borrow)]
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
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
#[allow(clippy::needless_borrow)]
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
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
#[allow(clippy::needless_borrow)]
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
            r#"impl std::hash::Hash for Something {
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
"#
        );
    }
}
