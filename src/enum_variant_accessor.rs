use either::Either;
use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::{quote_spanned, ToTokens, TokenStreamExt};
use syn::token::Mut;
use syn::{
    self, spanned::Spanned, Attribute, Data, DeriveInput, Error, Expr, ExprParen, ExprPath, Fields, FieldsNamed,
    GenericParam, Meta, MetaList, Token, Type, TypeReference, Variant,
};

const ATTR_HELP: &str = "EnumAccessor: Invalid accessor declaration, expected #[accessor(field1: type, except(VariantWithoutAccessor1,VariantWithoutAccessor2))]";
const ENUM_HELP: &str = "EnumAccessor: only variants with one unnamed parameter, unit and named variants are supported";

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
            Self::Only(vec) | Self::Except(vec) => vec,
            Self::All => return Ok(()),
        };

        for ident in vec {
            if !actual_variants.iter().any(|variant| &variant.ident == ident) {
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

fn parse_attr(attr: &Attribute) -> Result<Accessor, Error> {
    let Meta::List(MetaList{  tokens,.. }) = attr.meta.clone() else {
        return Err(Error::new(attr.span(), ATTR_HELP));
    };

    let mut tokens = tokens.into_iter();

    let Some(TokenTree::Ident(target_attribute)) = tokens.next() else {
        return Err(Error::new(attr.span(), ATTR_HELP));
    };

    let (target_attribute, is_call, next) = match tokens.next() {
        Some(TokenTree::Group(paren)) if paren.delimiter() == Delimiter::Parenthesis => {
            if !paren.stream().is_empty() {
                return Err(Error::new(attr.span(), "Target method cannot take arguments"));
            }
            (target_attribute, true, tokens.next())
        }
        next => (target_attribute, false, next),
    };

    let (alias, next) = match next {
        Some(TokenTree::Ident(ident)) if ident == "as" => {
            let Some(TokenTree::Ident(alias)) = tokens.next() else {
                return Err(Error::new(ident.span(), "Expected alias identifier"));
            };
            (alias, tokens.next())
        }
        next => (target_attribute.clone(), next),
    };

    let mut typ = match (next, tokens.next()) {
        (Some(TokenTree::Punct(p)), Some(typ)) if p.as_char() == ':' => typ.into_token_stream(),
        (next, _) => {
            return Err(Error::new(
                next.map_or_else(|| attr.span(), |token| token.span()),
                ATTR_HELP,
            ))
        }
    };

    let (typ, next) = match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
            (syn::parse2::<Type>(typ)?, Some(TokenTree::Punct(punct)))
        }
        Some(mut token) => loop {
            typ.append(token);

            if let Ok(typ) = syn::parse2::<Type>(typ.clone()) {
                break (typ, tokens.next());
            }

            token = tokens.next().ok_or_else(|| Error::new(typ.span(), "invalid type"))?;
        },
        None => (syn::parse2::<Type>(typ)?, None),
    };

    let accessor_type = match (&typ, is_call) {
        (
            Type::Reference(TypeReference {
                mutability: Some(_), ..
            }),
            true,
        ) => AccessorType::CallMut,
        (_, true) => AccessorType::Call,
        (_, false) => AccessorType::Field,
    };

    match next {
        Some(TokenTree::Punct(p)) if p.as_char() == ',' => (),
        Some(token) => return Err(Error::new(token.span(), "unexpected token")),
        None => {
            return Ok(Accessor {
                ident: target_attribute,
                alias,
                ty: typ,
                applies_to: AppliesTo::All,
                span: attr.span(),
                accessor_type,
            });
        }
    };

    let tokens = tokens.collect::<TokenStream>();
    let applies_to = match syn::parse2::<Expr>(tokens.clone())? {
        Expr::Tuple(expr) => AppliesTo::Except(
            expr.elems
                .into_iter()
                .map(|elem| match elem {
                    Expr::Path(ExprPath { path, .. }) if path.segments.len() == 1 => {
                        Ok(path.get_ident().ok_or_else(|| path.span())?.clone())
                    }
                    _ => Err(elem.span()),
                })
                .collect::<Result<_, _>>()
                .map_err(|span| Error::new(span, "invalid attribute name"))?,
        ),
        Expr::Paren(ExprParen { expr, .. }) => if let Expr::Path(path) = *expr {
            path.path
                .get_ident()
                .map(|ident| AppliesTo::Except(vec![ident.clone()]))
                .ok_or_else(|| path.span())
        } else {
            Err(expr.span())
        }
        .map_err(|span| Error::new(span, "comma-separated list of fields expected"))?,
        Expr::Call(call) => {
            let ident = if let Expr::Path(p) = call.func.as_ref() {
                p.path.get_ident().cloned()
            } else {
                None
            }
            .ok_or_else(|| Error::new(call.span(), "'except' or 'only' expected"))?;

            let vec = call
                .args
                .iter()
                .map(|arg| match arg {
                    Expr::Path(path) => Ok(path.path.get_ident().ok_or_else(|| arg.span())?.clone()),
                    _ => Err(arg.span()),
                })
                .collect::<Result<_, _>>()
                .map_err(|span| Error::new(span, "invalid attribute name"))?;

            if ident.to_string().eq_ignore_ascii_case("except") {
                AppliesTo::Except(vec)
            } else if ident.to_string().eq_ignore_ascii_case("only") {
                AppliesTo::Only(vec)
            } else {
                return Err(Error::new(tokens.span(), "'except' or 'only' expected"));
            }
        }
        _ => return Err(Error::new(tokens.span(), ATTR_HELP)),
    };

    Ok(Accessor {
        ident: target_attribute,
        alias,
        ty: typ,
        applies_to,
        span: attr.span(),
        accessor_type,
    })
}

fn make_mut(ident: &Ident, span: Span) -> Ident {
    Ident::new(format!("{ident}_mut").as_str(), span)
}

fn get_ret(span: Span, is_optional: bool, access_type: TypeModifier, typ: &Type) -> TokenStream {
    let modifier = match access_type {
        TypeModifier::Ref => Some(quote_spanned!(span => &)),
        TypeModifier::RefMut => Some(quote_spanned!(span => &mut )),
        TypeModifier::Unmodified => None,
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

fn make_match_arms(variant: &Variant, accessor: &Accessor, access_type: TypeModifier) -> Result<TokenStream, Error> {
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
            let mut err = Error::new(
                accessor.span,
                format!("Unit variant {variant_ident} must be included as exception"),
            );
            err.combine(Error::new(
                variant.span(),
                format!(
                    "Variant {variant_ident} is a unit type, and must be added to exceptions for `{accessor_name}`"
                ),
            ));
            Err(err)
        }
        (true, _, Fields::Unnamed(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span();
            }
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);

            Ok(quote_spanned!(span => Self::#variant_ident(x, ..) => #modifier x.#accessor_name #call))
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
            Ok(quote_spanned!(span => Self::#variant_ident{#accessor_name, ..} => #accessor_name #call))
        }
        (false, true, Fields::Named(fields)) => {
            let span = get_named_variant_field_span(variant, accessor, fields)?;
            let mut accessor_name = accessor_name.clone();
            accessor_name.set_span(span);

            Ok(
                quote_spanned!(span => Self::#variant_ident{#accessor_name, ..}=> std::option::Option::Some(#accessor_name #call)),
            )
        }
        (_, false, Fields::Unit) => Ok(quote_spanned!(span => Self::#variant_ident => std::option::Option::None)),
        (_, false, Fields::Named(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span();
            }
            Ok(quote_spanned!(span => Self::#variant_ident{..} => std::option::Option::None))
        }
        (_, false, Fields::Unnamed(..)) => {
            let mut span = span;
            if let Some(f) = variant.fields.iter().next() {
                span = f.span();
            }
            Ok(quote_spanned!(span => Self::#variant_ident(..) => std::option::Option::None))
        }
    }
}

fn get_named_variant_field_span(variant: &Variant, accessor: &Accessor, fields: &FieldsNamed) -> Result<Span, Error> {
    let accessor_name = &accessor.ident;

    let span = if let Some(f) = fields.named.iter().find(|f| f.ident.as_ref() == Some(accessor_name)) {
        f.span()
    } else {
        let variant_ident = &variant.ident;
        let mut err = Error::new(
            accessor.span,
            format!("No such field '{accessor_name}' on variant {variant_ident}"),
        );
        err.combine(Error::new(
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

fn get_method_modifiers(signature_type: SignatureType, method_name: &Ident, span: Span) -> (Option<Mut>, Ident) {
    let self_modifier = match signature_type {
        SignatureType::ReadOnly => None,
        SignatureType::FieldMut | SignatureType::CallMut => Some(Token![mut](span)),
    };

    let method_name = if signature_type == SignatureType::FieldMut {
        make_mut(method_name, span)
    } else {
        method_name.clone()
    };
    (self_modifier, method_name)
}

fn make_impl(
    span: Span,
    signature_type: SignatureType,
    method_name: &Ident,
    ret: &TokenStream,
    arms: &[TokenStream],
) -> TokenStream {
    let (self_modifier, method_name) = get_method_modifiers(signature_type, method_name, span);

    quote_spanned! {span =>
        pub fn #method_name(& #self_modifier self) -> #ret {
            match self {
                #(#arms),*
            }
        }
    }
}

pub fn impl_enum_accessor(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let ident = input.ident;

    let Data::Enum(enu) = input.data else {
            return Error::new(input_span, ENUM_HELP).into_compile_error();
        };

    let variants = enu.variants.into_iter().collect::<Vec<_>>();

    let mut accessors: Vec<Accessor> = vec![];

    for attr in input
        .attrs
        .into_iter()
        .filter(|i| i.path().get_ident().map(|i| i == "accessor") == Some(true))
    {
        match parse_attr(&attr) {
            Ok(accessor) => {
                if accessors.iter().any(|a| a.alias == accessor.alias) {
                    return Error::new(accessor.alias.span(), format!("Duplicate accessor {}", accessor.alias))
                        .into_compile_error();
                }
                accessors.push(accessor);
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

    for accessor in &accessors {
        if let Err(ident) = accessor.applies_to.validate(&variants) {
            return Error::new(ident.span(), format!("variant {ident} not found")).into_compile_error();
        }

        let span = accessor.alias.span();
        let method_name = &accessor.alias;

        let variations = match accessor.accessor_type {
            AccessorType::Call => Either::Left([(SignatureType::ReadOnly, TypeModifier::Unmodified)]),
            AccessorType::CallMut => Either::Left([(SignatureType::CallMut, TypeModifier::Unmodified)]),
            AccessorType::Field => Either::Right([
                (SignatureType::ReadOnly, TypeModifier::Ref),
                (SignatureType::FieldMut, TypeModifier::RefMut),
            ]),
        };

        for (signature_type, self_modifier) in variations.into_iter() {
            let ret = get_ret(span, accessor.applies_to != AppliesTo::All, self_modifier, &accessor.ty);
            let match_arms = match variants
                .iter()
                .map(|variant| make_match_arms(variant, accessor, self_modifier))
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(r) => r,
                Err(err) => return err.into_compile_error(),
            };

            accessor_impls.push(make_impl(span, signature_type, method_name, &ret, &match_arms));
        }
    }

    let generics = &input.generics;
    let where_clause = &input.generics.where_clause;
    let generics_params = &input
        .generics
        .params
        .iter()
        .filter_map(|p| match p {
            GenericParam::Type(t) => Some(&t.ident),
            GenericParam::Const(t) => Some(&t.ident),
            GenericParam::Lifetime(_) => None,
        })
        .collect::<Vec<_>>();

    syn::parse_quote_spanned! {input_span =>
        #[allow(dead_code)]
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r#"#[allow(dead_code)]
impl SomeEnum {
    pub fn acc1(&self) -> std::option::Option<&usize> {
        match self {
            Self::A(..) => std::option::Option::None,
            Self::C(..) => std::option::Option::None,
            Self::D(x, ..) => std::option::Option::Some(&x.acc1),
            Self::G(x, ..) => std::option::Option::Some(&x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    pub fn acc1_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(..) => std::option::Option::None,
            Self::C(..) => std::option::Option::None,
            Self::D(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::G(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::H { acc1, .. } => std::option::Option::Some(acc1),
        }
    }
    pub fn get_u8(&self) -> &u8 {
        match self {
            Self::A(x, ..) => &x.acc2,
            Self::C(x, ..) => &x.acc2,
            Self::D(x, ..) => &x.acc2,
            Self::G(x, ..) => &x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    pub fn get_u8_mut(&mut self) -> &mut u8 {
        match self {
            Self::A(x, ..) => &mut x.acc2,
            Self::C(x, ..) => &mut x.acc2,
            Self::D(x, ..) => &mut x.acc2,
            Self::G(x, ..) => &mut x.acc2,
            Self::H { acc2, .. } => acc2,
        }
    }
    pub fn acc3(&self) -> std::option::Option<&String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&x.acc3),
            Self::C(x, ..) => std::option::Option::Some(&x.acc3),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(&x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
    pub fn acc3_mut(&mut self) -> std::option::Option<&mut String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::C(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(&mut x.acc3),
            Self::H { acc3, .. } => std::option::Option::Some(acc3),
        }
    }
    pub fn acc4(&self) -> std::option::Option<String> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(x.acc4()),
            Self::C(x, ..) => std::option::Option::Some(x.acc4()),
            Self::D(..) => std::option::Option::None,
            Self::G(x, ..) => std::option::Option::Some(x.acc4()),
            Self::H { acc4, .. } => std::option::Option::Some(acc4()),
        }
    }
    pub fn other(&self) -> std::option::Option<String> {
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r#"#[allow(dead_code)]
impl SomeEnum {
    pub fn acc1(&self) -> std::option::Option<&usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&x.acc1),
            Self::B => std::option::Option::None,
            Self::C(x, ..) => std::option::Option::Some(&x.acc1),
        }
    }
    pub fn acc1_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(&mut x.acc1),
            Self::B => std::option::Option::None,
            Self::C(x, ..) => std::option::Option::Some(&mut x.acc1),
        }
    }
}
"#
        );
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
        let output = rust_format::RustFmt::default().format_str(output.to_string()).unwrap();
        assert_eq!(
            output,
            r#"#[allow(dead_code)]
impl SomeEnum {
    pub fn inner_mut(&mut self) -> std::option::Option<&mut usize> {
        match self {
            Self::A(x, ..) => std::option::Option::Some(x.inner_mut()),
            Self::C(x, ..) => std::option::Option::Some(x.inner_mut()),
            Self::D(..) => std::option::Option::None,
        }
    }
}
"#
        );
    }
}
