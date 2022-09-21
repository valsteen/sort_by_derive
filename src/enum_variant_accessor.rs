use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use syn::{
    self, punctuated::Pair, spanned::Spanned, Attribute, Data, DeriveInput, Expr, ExprParen,
    ExprTuple, ExprType, Fields, Type,
};

const ATTR_HELP: &str = "EnumAccessor: Invalid accessor declaration, expected #[accessor(field1: type, (VariantWithoutAccessor1,VariantWithoutAccessor2))]";
const ENUM_HELP: &str =
    "EnumAccessor: only variants with one unnamed parameter, unit and named variants are supported";

struct Accessor {
    ident: Ident,
    ty: Type,
    except: Vec<Ident>,
    span: Span,
}

fn parse_tuple(expr: &ExprTuple) -> Option<Accessor> {
    let mut pairs = expr.elems.pairs();
    let name_type = pairs.next()?;
    let exceptions = pairs.next()?;

    let mut accessor = match name_type {
        Pair::Punctuated(Expr::Type(exp), _) => parse_ty(exp)?,
        _ => return None,
    };

    match exceptions {
        // Slight variations makes the parser choose one of those compositions
        Pair::End(Expr::Tuple(expr)) => {
            for elem in &expr.elems {
                match elem {
                    Expr::Path(path) => accessor.except.push(path.path.get_ident()?.clone()),
                    _ => return None,
                }
            }
        }
        Pair::End(Expr::Type(expr)) => match expr.ty.as_ref() {
            Type::Tuple(a) => {
                for elem in &a.elems {
                    match elem {
                        Type::Path(path) => accessor.except.push(path.path.get_ident()?.clone()),
                        _ => return None,
                    }
                }
            }
            _ => return None,
        },
        Pair::End(Expr::Paren(expr)) => match expr.expr.as_ref() {
            Expr::Path(path) => accessor.except.push(path.path.get_ident()?.clone()),
            _ => return None,
        },
        _ => return None,
    };

    Some(accessor)
}

fn parse_ty(expr: &ExprType) -> Option<Accessor> {
    match expr.expr.as_ref() {
        Expr::Path(p) => Some(Accessor {
            ident: p.path.get_ident()?.clone(),
            ty: expr.ty.as_ref().clone(),
            except: vec![],
            span: expr.span(),
        }),
        _ => None,
    }
}

fn parse_paren(expr: &ExprParen) -> Option<Accessor> {
    match expr.expr.as_ref() {
        Expr::Type(ty) => parse_ty(ty),
        _ => None,
    }
}

fn parse_attr(attr: &Attribute) -> Result<Accessor, syn::Error> {
    match syn::parse2::<Expr>(attr.tokens.to_token_stream()) {
        Ok(Expr::Paren(expr)) => {
            // #[accessor(field1: type))]
            parse_paren(&expr).ok_or_else(|| syn::Error::new(expr.span(), ATTR_HELP))
        }
        Ok(Expr::Tuple(expr)) => {
            // #[accessor(field1: type, except(variant1,variant2))]
            parse_tuple(&expr).ok_or_else(|| syn::Error::new(expr.span(), ATTR_HELP))
        }
        Ok(expr) => Err(syn::Error::new(expr.span(), ATTR_HELP)),
        Err(e) => {
            let mut err = syn::Error::new(attr.span(), ATTR_HELP);
            err.combine(e);
            Err(err)
        }
    }
}

fn make_mut(ident: &Ident, span: Span) -> Ident {
    Ident::new(format!("{ident}_mut").as_str(), span)
}

pub fn impl_enum_accessor(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let ident = input.ident;

    let enu = match input.data {
        Data::Enum(enu) => enu,
        _ => {
            return syn::Error::new(input_span, ENUM_HELP).into_compile_error();
        }
    };

    let mut variants = vec![];
    let mut named_variants = vec![];
    let mut unit_variants = vec![];

    for variant in enu.variants {
        match variant.fields {
            Fields::Unnamed(_) => {
                variants.push(variant);
            }
            Fields::Named(_) => {
                variants.push(variant.clone());
                named_variants.push(variant.ident);
            }
            Fields::Unit => {
                variants.push(variant.clone());
                unit_variants.push(variant.ident.clone());
            }
        }
    }

    let mut accessors: Vec<Accessor> = vec![];

    for attr in input
        .attrs
        .iter()
        .filter(|i| i.path.get_ident().map(|i| i == "accessor") == Some(true))
    {
        match parse_attr(attr) {
            Ok(accessor) => {
                if accessors.iter().any(|a| a.ident == accessor.ident) {
                    return syn::Error::new(
                        accessor.span,
                        format!("Duplicate accessor {}", accessor.ident),
                    )
                    .into_compile_error();
                }
                accessors.push(accessor)
            }
            Err(e) => return e.into_compile_error(),
        };
    }

    if accessors.is_empty() {
        return syn::Error::new(input_span, r#"Missing accessor declaration, expected #[accessor(field1: type, (ExceptionVariant1,ExceptionVariant2))]"#).into_compile_error();
    }

    let extension_trait = Ident::new(format!("{ident}Accessor").as_str(), input_span);

    let mut accessor_impls = Vec::new();
    let mut accessor_defs = Vec::new();

    for accessor in accessors.iter() {
        let span = accessor.span;
        let accessor_name = &accessor.ident;
        let mut_accessor_name = make_mut(&accessor.ident, accessor.span);

        let mut ro_variants = Vec::new();
        let mut mut_variants = Vec::new();

        let (ro_ret, mut_ret) = {
            let typ = &accessor.ty;
            if accessor.except.is_empty() {
                (
                    quote::quote_spanned!(span => &#typ),
                    quote::quote_spanned!(span => &mut #typ),
                )
            } else {
                (
                    quote::quote_spanned!(span => std::option::Option<&#typ>),
                    quote::quote_spanned!(span => std::option::Option<&mut #typ>),
                )
            }
        };

        for except in accessor.except.iter() {
            if !variants.iter().any(|i| &i.ident == except) {
                return syn::Error::new(except.span(), format!("variant {except} not found"))
                    .into_compile_error();
            }
        }

        for variant in variants.iter() {
            let span = variant.span();
            let variant_ident = &variant.ident;

            match (
                accessor.except.is_empty(),
                accessor.except.contains(variant_ident),
                named_variants.contains(variant_ident),
            ) {
                (true, _, false) => {
                    if unit_variants.contains(variant_ident) {
                        let mut err = syn::Error::new(
                            accessor.span,
                            format!("Unit variant {variant_ident} must be included as exception"),
                        );
                        err.combine(syn::Error::new(variant.span(), format!("Variant {variant_ident} is a unit type, and must be added to exceptions for `{accessor_name}`")));
                        return err.into_compile_error();
                    }
                    let mut span = span;
                    if let Some(f) = variant.fields.iter().next() {
                        span = f.span()
                    }
                    let mut accessor_name = accessor_name.clone();
                    accessor_name.set_span(span);
                    ro_variants.push(
                        quote::quote_spanned!(span => Self::#variant_ident(x, ..) => &x.#accessor_name),
                    );
                    mut_variants.push(
                        quote::quote_spanned!(span => Self::#variant_ident(x, ..) => &mut x.#accessor_name),
                    );
                }
                (true, _, true) => {
                    let span = if let Some(f) = variant
                        .fields
                        .iter()
                        .find(|f| f.ident.as_ref() == Some(accessor_name))
                    {
                        f.span()
                    } else {
                        let mut err = syn::Error::new(
                            accessor.span,
                            format!("No such field '{accessor_name}' on variant {variant_ident}"),
                        );
                        err.combine(syn::Error::new(
                            variant.span(),
                            format!("{accessor_name} is missing from {variant_ident}"),
                        ));
                        return err.into_compile_error();
                    };
                    let mut accessor_name = accessor_name.clone();
                    accessor_name.set_span(span);
                    ro_variants.push(
                        quote::quote_spanned!(span => Self::#variant_ident{#accessor_name, ..} => #accessor_name),
                    );
                    mut_variants.push(
                        quote::quote_spanned!(span => Self::#variant_ident{#accessor_name, ..} => #accessor_name),
                    );
                }
                (_, true, false) => {
                    let mut span = span;
                    if let Some(f) = variant.fields.iter().next() {
                        span = f.span()
                    }
                    if unit_variants.contains(variant_ident) {
                        ro_variants.push(quote::quote_spanned!(span => Self::#variant_ident => std::option::Option::None));
                        mut_variants
                            .push(quote::quote_spanned!(span => Self::#variant_ident => std::option::Option::None));
                    } else {
                        ro_variants.push(quote::quote_spanned!(span => Self::#variant_ident(..) => std::option::Option::None));
                        mut_variants
                            .push(quote::quote_spanned!(span => Self::#variant_ident(..) => std::option::Option::None));
                    }
                }
                (_, true, true) => {
                    ro_variants.push(quote::quote_spanned!(span => Self::#variant_ident{..} => std::option::Option::None));
                    mut_variants
                        .push(quote::quote_spanned!(span => Self::#variant_ident{..} => std::option::Option::None));
                }
                (false, false, false) => {
                    if unit_variants.contains(variant_ident) {
                        let mut err = syn::Error::new(
                            accessor.span,
                            format!("Unit variant {variant_ident} must be included as exception"),
                        );
                        err.combine(syn::Error::new(variant.span(), format!("Variant {variant_ident} is a unit type, and must be added to exceptions for `{accessor_name}`")));
                        return err.into_compile_error();
                    }
                    let span = variant.fields.iter().next().unwrap().span();
                    let mut accessor_name = accessor_name.clone();
                    accessor_name.set_span(span);
                    ro_variants.push(quote::quote_spanned!(span => Self::#variant_ident(x, ..) => std::option::Option::Some(&x.#accessor_name)));
                    mut_variants.push(quote::quote_spanned!(span => Self::#variant_ident(x, ..) => std::option::Option::Some(&mut x.#accessor_name)));
                }
                (false, false, true) => {
                    let span = if let Some(f) = variant
                        .fields
                        .iter()
                        .find(|f| f.ident.as_ref() == Some(accessor_name))
                    {
                        f.span()
                    } else {
                        let mut err = syn::Error::new(
                            accessor.span,
                            format!("No such field '{accessor_name}' on variant {variant_ident}"),
                        );
                        err.combine(syn::Error::new(
                            variant.span(),
                            format!("{accessor_name} is missing from {variant_ident}"),
                        ));
                        return err.into_compile_error();
                    };
                    let mut accessor_name = accessor_name.clone();
                    accessor_name.set_span(span);
                    ro_variants.push(quote::quote_spanned!(span => Self::#variant_ident{#accessor_name, ..} => std::option::Option::Some(#accessor_name)));
                    mut_variants.push(quote::quote_spanned!(span => Self::#variant_ident{#accessor_name, ..}=> std::option::Option::Some(#accessor_name)));
                }
            };
        }

        accessor_impls.push(quote::quote_spanned! {span =>
            fn #accessor_name(&self) -> #ro_ret {
                match self {
                    #(#ro_variants),*
                }
            }

            fn #mut_accessor_name(&mut self) -> #mut_ret {
                match self {
                    #(#mut_variants),*
                }
            }
        });

        accessor_defs.push(quote::quote_spanned! {span =>
            fn #accessor_name(&self) -> #ro_ret;
            fn #mut_accessor_name(&mut self) -> #mut_ret;
        })
    }

    syn::parse_quote_spanned! {input_span =>
        pub trait #extension_trait {
            #(#accessor_defs)*
        }

        impl #extension_trait for #ident {
            #(#accessor_impls)*
        }
    }
}

#[cfg(test)]
mod test {
    use rust_format::Formatter;

    #[test]
    fn test_this() {
        let input = syn::parse_quote! {
            #[accessor(acc1: usize, (A,C))]
            #[accessor(acc2: u8)]
            #[accessor(acc3: String, (D))]
            enum SomeEnum {
                A(b),
                C(d),
                D(g),
                G(x),
                H { acc1: usize, acc2: u8, acc3: String }
            }
        };
        let output = crate::enum_variant_accessor::impl_enum_accessor(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"pub trait SomeEnumAccessor {
    fn acc1(&self) -> std::option::Option<&usize>;
    fn acc1_mut(&mut self) -> std::option::Option<&mut usize>;
    fn acc2(&self) -> &u8;
    fn acc2_mut(&mut self) -> &mut u8;
    fn acc3(&self) -> std::option::Option<&String>;
    fn acc3_mut(&mut self) -> std::option::Option<&mut String>;
}
impl SomeEnumAccessor for SomeEnum {
    fn acc1(&self) -> std::option::Option<&usize> {
        match self {
            Self::A(_) => std::option::Option::None,
            Self::C(_) => std::option::Option::None,
            Self::D(x) => std::option::Option::Some(&x.acc1),
            Self::G(x) => std::option::Option::Some(&x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    fn acc1_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(_) => std::option::Option::None,
            Self::C(_) => std::option::Option::None,
            Self::D(x) => std::option::Option::Some(&mut x.acc1),
            Self::G(x) => std::option::Option::Some(&mut x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    fn acc2(&self) -> &u8 {
        match self {
            Self::A(x) => &x.acc2,
            Self::C(x) => &x.acc2,
            Self::D(x) => &x.acc2,
            Self::G(x) => &x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    fn acc2_mut(&mut self) -> &mut u8 {
        match self {
            Self::A(x) => &mut x.acc2,
            Self::C(x) => &mut x.acc2,
            Self::D(x) => &mut x.acc2,
            Self::G(x) => &mut x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    fn acc3(&self) -> std::option::Option<&String> {
        match self {
            Self::A(x) => std::option::Option::Some(&x.acc3),
            Self::C(x) => std::option::Option::Some(&x.acc3),
            Self::D(_) => std::option::Option::None,
            Self::G(x) => std::option::Option::Some(&x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
    fn acc3_mut(&mut self) -> std::option::Option<&mut String> {
        match self {
            Self::A(x) => std::option::Option::Some(&mut x.acc3),
            Self::C(x) => std::option::Option::Some(&mut x.acc3),
            Self::D(_) => std::option::Option::None,
            Self::G(x) => std::option::Option::Some(&mut x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
}
"#
        );
    }
}
