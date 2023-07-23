use either::Either;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::token::Mut;
use syn::{
    self, punctuated::Pair, spanned::Spanned, Attribute, Data, DeriveInput, Expr, ExprCall,
    ExprParen, ExprTuple, ExprType, Fields, FieldsNamed, GenericParam, Token, Type, TypeReference,
    Variant,
};

const ATTR_HELP: &str = "EnumAccessor: Invalid accessor declaration, expected #[accessor(field1: type, except(VariantWithoutAccessor1,VariantWithoutAccessor2))]";
const ENUM_HELP: &str =
    "EnumAccessor: only variants with one unnamed parameter, unit and named variants are supported";

#[derive(Clone, Copy)]
enum AccessorType {
    Field,
    Call,
    CallMut,
}

#[derive(PartialEq, Eq)]
enum AppliesTo {
    Only(Vec<Ident>),
    Except(Vec<Ident>),
    All,
}

impl AppliesTo {
    fn applies_to(&self, ident: &Ident) -> bool {
        match self {
            Self::Only(vec) => vec.contains(ident),
            Self::Except(vec) => !vec.contains(ident),
            Self::All => true,
        }
    }

    fn validate(&self, actual_variants: &[Variant]) -> Result<(), Ident> {
        let vec = match self {
            AppliesTo::Only(vec) | AppliesTo::Except(vec) => vec,
            AppliesTo::All => return Ok(()),
        };

        for ident in vec {
            if !actual_variants
                .iter()
                .any(|variant| &variant.ident == ident)
            {
                return Err(ident.clone());
            }
        }
        Ok(())
    }
}

struct Accessor {
    ident: Ident,
    alias: Ident,
    ty: Type,
    applies_to: AppliesTo,
    span: Span,
    accessor_type: AccessorType,
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
            accessor.applies_to = AppliesTo::Except(
                expr.elems
                    .iter()
                    .map(|elem| match elem {
                        Expr::Path(path) => Some(path.path.get_ident()?.clone()),
                        _ => None,
                    })
                    .collect::<Option<_>>()?,
            );
        }
        Pair::End(Expr::Type(expr)) => match expr.ty.as_ref() {
            Type::Tuple(expr) => {
                accessor.applies_to = AppliesTo::Except(
                    expr.elems
                        .iter()
                        .map(|elem| match elem {
                            Type::Path(path) => Some(path.path.get_ident()?.clone()),
                            _ => None,
                        })
                        .collect::<Option<_>>()?,
                );
            }
            _ => return None,
        },
        Pair::End(Expr::Paren(expr)) => match expr.expr.as_ref() {
            Expr::Path(path) => {
                accessor.applies_to = AppliesTo::Except(vec![path.path.get_ident()?.clone()])
            }
            _ => return None,
        },
        Pair::End(Expr::Call(call)) => {
            let ident = ident_from_call(call)?;
            let vec = call
                .args
                .iter()
                .map(|arg| match arg {
                    Expr::Path(path) => path.path.get_ident().cloned(),
                    _ => None,
                })
                .collect::<Option<Vec<Ident>>>()?;
            if ident.to_string().eq_ignore_ascii_case("except") {
                accessor.applies_to = AppliesTo::Except(vec);
            } else if ident.to_string().eq_ignore_ascii_case("only") {
                accessor.applies_to = AppliesTo::Only(vec);
            } else {
                return None;
            }
        }
        _ => return None,
    };

    Some(accessor)
}

fn ident_from_call(call: &ExprCall) -> Option<Ident> {
    match call.func.as_ref() {
        Expr::Path(p) => Some(p.path.get_ident()?.clone()),
        _ => None,
    }
}

fn parse_ty(expr: &ExprType) -> Option<Accessor> {
    let (ident, alias, accessor_type, span) = match expr.expr.as_ref() {
        Expr::Path(p) => {
            let ident = p.path.get_ident()?.clone();
            (ident.clone(), ident, AccessorType::Field, expr.span())
        }
        Expr::Call(call) => {
            let ident = ident_from_call(call)?;
            if !call.args.is_empty() {
                return None;
            }
            let accessor_type = match expr.ty.as_ref() {
                Type::Reference(TypeReference {
                    mutability: Some(_),
                    ..
                }) => AccessorType::CallMut,
                _ => AccessorType::Call,
            };

            (ident.clone(), ident, accessor_type, call.span())
        }
        Expr::Cast(expr) => {
            let alias = if let Type::Path(path) = expr.ty.as_ref() {
                path.path.get_ident()?.clone()
            } else {
                return None;
            };
            match expr.expr.as_ref() {
                Expr::Path(path) => {
                    let ident = path.path.get_ident()?.clone();
                    (ident, alias, AccessorType::Field, expr.span())
                }
                Expr::Call(call) => {
                    let ident = ident_from_call(call)?;
                    if !call.args.is_empty() {
                        return None;
                    }
                    (ident, alias, AccessorType::Call, call.span())
                }
                _ => return None,
            }
        }
        _ => return None,
    };

    Some(Accessor {
        ident,
        alias,
        ty: expr.ty.as_ref().clone(),
        applies_to: AppliesTo::All,
        span,
        accessor_type,
    })
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

fn get_ret(span: Span, is_optional: bool, access_type: TypeModifier, typ: &Type) -> TokenStream {
    let modifier = match access_type {
        TypeModifier::Ref => Some(quote_spanned!(span => &)),
        TypeModifier::RefMut => Some(quote_spanned!(span => &mut )),
        _ => None,
    };

    if is_optional {
        quote_spanned!(span => std::option::Option<#modifier #typ>)
    } else {
        quote_spanned!(span => #modifier #typ)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum TypeModifier {
    Ref,
    RefMut,
    Unmodified,
}

fn make_match_arms(
    variant: &Variant,
    accessor: &Accessor,
    access_type: TypeModifier,
) -> Result<TokenStream, syn::Error> {
    let span = variant.span();
    let variant_ident = &variant.ident;
    let accessor_name = &accessor.ident;

    let mut modifier = None;
    let mut call = None;

    match access_type {
        TypeModifier::Ref => {
            modifier = Some(quote_spanned!(span => &));
        }
        TypeModifier::RefMut => {
            modifier = Some(quote_spanned!(span => &mut ));
        }
        TypeModifier::Unmodified => call = Some(quote_spanned!(span => ())),
    }

    match (
        accessor.applies_to == AppliesTo::All,
        accessor.applies_to.applies_to(variant_ident),
        &variant.fields,
    ) {
        (_, true, Fields::Unit) => {
            let mut err = syn::Error::new(
                accessor.span,
                format!("Unit variant {variant_ident} must be included as exception"),
            );
            err.combine(syn::Error::new(variant.span(), format!("Variant {variant_ident} is a unit type, and must be added to exceptions for `{accessor_name}`")));
            Err(err)
        }
        (true, _, Fields::Unnamed(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span()
            }
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);

            Ok(
                quote_spanned!(span => Self::#variant_ident(x, ..) => #modifier x.#accessor_name #call),
            )
        }
        (false, true, Fields::Unnamed(..)) => {
            let span = variant.fields.iter().next().unwrap().span();
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);

            Ok(
                quote_spanned!(span => Self::#variant_ident(x, ..) => std::option::Option::Some(#modifier x.#accessor_name #call)),
            )
        }
        (true, _, Fields::Named(fields)) => {
            let span = get_named_variant_field_span(variant, accessor, fields)?;
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);
            Ok(
                quote_spanned!(span => Self::#variant_ident{#accessor_name, ..} => #accessor_name #call),
            )
        }
        (false, true, Fields::Named(fields)) => {
            let span = get_named_variant_field_span(variant, accessor, fields)?;
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);

            Ok(
                quote_spanned!(span => Self::#variant_ident{#accessor_name, ..}=> std::option::Option::Some(#accessor_name #call)),
            )
        }
        (_, false, Fields::Unit) => {
            Ok(quote_spanned!(span => Self::#variant_ident => std::option::Option::None))
        }
        (_, false, Fields::Named(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span()
            }
            Ok(quote_spanned!(span => Self::#variant_ident{..} => std::option::Option::None))
        }
        (_, false, Fields::Unnamed(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span()
            }
            Ok(quote_spanned!(span => Self::#variant_ident(..) => std::option::Option::None))
        }
    }
}

fn get_named_variant_field_span(
    variant: &Variant,
    accessor: &Accessor,
    fields: &FieldsNamed,
) -> Result<Span, syn::Error> {
    let accessor_name = &accessor.ident;

    let span = if let Some(f) = fields
        .named
        .iter()
        .find(|f| f.ident.as_ref() == Some(accessor_name))
    {
        f.span()
    } else {
        let variant_ident = &variant.ident;
        let mut err = syn::Error::new(
            accessor.span,
            format!("No such field '{accessor_name}' on variant {variant_ident}"),
        );
        err.combine(syn::Error::new(
            variant.span(),
            format!("{accessor_name} is missing from {variant_ident}"),
        ));
        return Err(err);
    };
    Ok(span)
}

#[derive(PartialEq, Clone, Copy)]
enum SignatureType {
    ReadOnly,
    FieldMut,
    CallMut,
}

fn get_method_modifiers(
    signature_type: SignatureType,
    method_name: &Ident,
    span: Span,
) -> (Option<Mut>, Ident) {
    let self_modifier = match signature_type {
        SignatureType::ReadOnly => None,
        SignatureType::FieldMut | SignatureType::CallMut => Some(Token![mut](span)),
    };

    let method_name = if let SignatureType::FieldMut = signature_type {
        make_mut(method_name, span)
    } else {
        method_name.clone()
    };
    (self_modifier, method_name)
}

fn make_def(
    span: Span,
    signature_type: SignatureType,
    method_name: &Ident,
    ret: &TokenStream,
) -> TokenStream {
    let (self_modifier, method_name) = get_method_modifiers(signature_type, method_name, span);

    quote_spanned! {span =>
        fn #method_name(& #self_modifier self) -> #ret;
    }
}

fn make_impl(
    span: Span,
    signature_type: SignatureType,
    method_name: &Ident,
    ret: &TokenStream,
    arms: Vec<TokenStream>,
) -> TokenStream {
    let (self_modifier, method_name) = get_method_modifiers(signature_type, method_name, span);

    quote_spanned! {span =>
        fn #method_name(& #self_modifier self) -> #ret {
            match self {
                #(#arms),*
            }
        }
    }
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
        return quote::quote! {
            compile_error!(r#"Missing accessor declaration, expected #[accessor(field1: type, except(ExceptionVariant1,ExceptionVariant2))]"#);
        };
    }

    let mut accessor_impls = Vec::new();
    let mut accessor_defs = Vec::new();

    for accessor in accessors.iter() {
        if let Err(ident) = accessor.applies_to.validate(&variants) {
            return syn::Error::new(ident.span(), format!("variant {ident} not found"))
                .into_compile_error();
        }

        let span = accessor.alias.span();
        let method_name = &accessor.alias;

        let variations = match accessor.accessor_type {
            AccessorType::Call => {
                Either::Left([(SignatureType::ReadOnly, TypeModifier::Unmodified)])
            }
            AccessorType::CallMut => {
                Either::Left([(SignatureType::CallMut, TypeModifier::Unmodified)])
            }
            AccessorType::Field => Either::Right([
                (SignatureType::ReadOnly, TypeModifier::Ref),
                (SignatureType::FieldMut, TypeModifier::RefMut),
            ]),
        };

        for (signature_type, self_modifier) in variations.into_iter() {
            let ret = get_ret(
                span,
                accessor.applies_to != AppliesTo::All,
                self_modifier,
                &accessor.ty,
            );
            let match_arms = match variants
                .iter()
                .map(|variant| make_match_arms(variant, accessor, self_modifier))
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(r) => r,
                Err(err) => return err.into_compile_error(),
            };

            accessor_impls.push(make_impl(
                span,
                signature_type,
                method_name,
                &ret,
                match_arms,
            ));
            accessor_defs.push(make_def(span, signature_type, method_name, &ret));
        }
    }

    let generics = &input.generics;
    let where_clause = &input.generics.where_clause;
    let generics_params = &input
        .generics
        .params
        .iter()
        .flat_map(|p| match p {
            GenericParam::Type(t) => Some(&t.ident),
            GenericParam::Const(t) => Some(&t.ident),
            _ => None,
        })
        .collect::<Vec<_>>();

    syn::parse_quote_spanned! {input_span =>
        impl #generics #ident <#(#generics_params),*> #where_clause {
            #(#accessor_impls)*
        }
    }
}

#[cfg(test)]
mod test {
    use rust_format::Formatter;

    #[test]
    fn test_enum() {
        let input = syn::parse_quote! {
            #[accessor(acc1: usize, Only(D,G,H))]
            #[accessor(acc2 as get_u8: u8)]
            #[accessor(acc3: String, Except(D))]
            #[accessor(acc4(): String, (D))]
            #[accessor(acc5() as other: String, Except(D))]
            enum SomeEnum {
                A(b),
                C(d),
                D(g),
                G(x,y,z),
                H { acc1: usize, acc2: u8, acc3: String, acc4: fn() -> String, acc5: fn() -> String }
            }
        };
        let output = crate::enum_variant_accessor::impl_enum_accessor(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl SomeEnum {
    fn acc1(&self) -> std::option::Option<&usize> {
        match self {
            Self::A(..) => std::option::Option::None,
            Self::C(..) => std::option::Option::None,
            Self::D(x, ..) => std::option::Option::Some(&x.acc1),
            Self::G(x, ..) => std::option::Option::Some(&x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    fn acc1_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(..) => std::option::Option::None,
            Self::C(..) => std::option::Option::None,
            Self::D(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::G(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    fn get_u8(&self) -> &u8 {
        match self {
            Self::A(x, ..) => &x.acc2,
            Self::C(x, ..) => &x.acc2,
            Self::D(x, ..) => &x.acc2,
            Self::G(x, ..) => &x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    fn get_u8_mut(&mut self) -> &mut u8 {
        match self {
            Self::A(x, ..) => &mut x.acc2,
            Self::C(x, ..) => &mut x.acc2,
            Self::D(x, ..) => &mut x.acc2,
            Self::G(x, ..) => &mut x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    fn acc3(&self) -> std::option::Option<&String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&x.acc3),
            Self::C(x, ..) => std::option::Option::Some(&x.acc3),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(&x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
    fn acc3_mut(&mut self) -> std::option::Option<&mut String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::C(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
    fn acc4(&self) -> std::option::Option<String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(x.acc4()),
            Self::C(x, ..) => std::option::Option::Some(x.acc4()),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(x.acc4()),
            Self::H { acc4, .. } => std::option::Option::Some(acc4()),
        }
    }
    fn other(&self) -> std::option::Option<String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(x.acc5()),
            Self::C(x, ..) => std::option::Option::Some(x.acc5()),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(x.acc5()),
            Self::H { acc5, .. } => std::option::Option::Some(acc5()),
        }
    }
}
"#
        );
    }

    #[test]
    fn test_unit() {
        let input = syn::parse_quote! {
            #[accessor(acc1: usize, (B))]
            enum SomeEnum {
                A(a),
                B,
                C(b)
            }
        };
        let output = crate::enum_variant_accessor::impl_enum_accessor(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r#"impl SomeEnum {
    fn acc1(&self) -> std::option::Option<&usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&x.acc1),
            Self::B => std::option::Option::None,
            Self::C(x, ..) => std::option::Option::Some(&x.acc1),
        }
    }
    fn acc1_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::B => std::option::Option::None,
            Self::C(x, ..) => std::option::Option::Some(&mut x.acc1),
        }
    }
}
"#
        )
    }

    #[test]
    fn test_mut_method() {
        let input = syn::parse_quote! {
            #[accessor(inner_mut(): &mut usize, (D))]
            enum SomeEnum {
                A(b),
                C(d),
                D(g),
            }
        };
        let output = crate::enum_variant_accessor::impl_enum_accessor(syn::parse2(input).unwrap());
        let output = rust_format::RustFmt::default()
            .format_str(output.to_string())
            .unwrap();
        assert_eq!(
            output,
            r"impl SomeEnum {
    fn inner_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(x.inner_mut()),
            Self::C(x, ..) => std::option::Option::Some(x.inner_mut()),
            Self::D(..) => std::option::Option::None,
        }
    }
}
"
        )
    }
}
