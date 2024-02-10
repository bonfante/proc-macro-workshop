use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Field, Fields, FieldsNamed, Ident, Meta,
    PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let derive_ident = input.ident.clone();
    let builder_ident = Ident::new(
        format!("{}{}", derive_ident, "Builder").as_str(),
        input.span(),
    );
    let named_fields = match extract_named_fields(input) {
        Ok(fields) => fields.named,
        Err(err) => return err.to_compile_error().into(),
    };

    let mut builder_fields: Vec<proc_macro2::TokenStream> = Vec::new();
    let mut builder_fields_defaults: Vec<proc_macro2::TokenStream> = Vec::new();
    let mut builder_methods: Vec<proc_macro2::TokenStream> = Vec::new();
    let mut build_clone_fields: Vec<proc_macro2::TokenStream> = Vec::new();

    for field in &named_fields {
        let field_ident = field.ident.clone().expect("Should be named");
        let field_option = extract_option_type(field);
        let field_builder_attr = match parse_builder_attribute(&field.attrs) {
            Ok(r) => r,
            Err(e) => {
                return e.to_compile_error().into();
            }
        };

        let optional_field = field_option.is_some() || field_builder_attr.is_some();
        let field_ty = field_option.unwrap_or(field.ty.to_token_stream());

        if let Some(each_value) = field_builder_attr.clone() {
            let field_inner_ty = extract_vec_type(field).expect("");
            builder_fields.push(quote!(#field_ident: #field_ty));
            builder_fields_defaults.push(quote!(#field_ident: std::vec::Vec::new()));
            builder_methods.push(quote!(
                fn #each_value(&mut self, #each_value: #field_inner_ty) -> &mut Self {
                    self.#field_ident.push(#each_value);
                    self
                }
            ));
        } else {
            builder_fields.push(quote!(#field_ident: std::option::Option<#field_ty>));
            builder_fields_defaults.push(quote!(#field_ident: std::option::Option::None));
            builder_methods.push(quote!(
                fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                    self.#field_ident = std::option::Option::Some(#field_ident);
                    self
                }
            ));
        }

        if optional_field {
            build_clone_fields.push(quote!(#field_ident: self.#field_ident.clone()));
        } else {
            build_clone_fields.push(quote!(#field_ident: self.#field_ident.clone().expect("")));
        }
    }

    quote!(
        impl #derive_ident {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields_defaults,)*
                }
            }
        }
        struct #builder_ident {
            #(#builder_fields,)*
        }
        impl #builder_ident{
             pub fn build(&mut self) -> std::result::Result<#derive_ident, std::boxed::Box<dyn std::error::Error>> {
                 std::result::Result::Ok(#derive_ident{
                    #(#build_clone_fields,)*
                })
             }
            #(#builder_methods)*
        }
    ).into()
}

fn extract_named_fields(input: DeriveInput) -> syn::Result<FieldsNamed> {
    let ds = match input.data {
        Data::Struct(ds) => ds,
        Data::Enum(e) => {
            return Err(syn::Error::new(
                e.enum_token.span,
                "Expected struct found enum",
            ))
        }
        Data::Union(u) => {
            return Err(syn::Error::new(
                u.union_token.span,
                "Expected struct found union",
            ))
        }
    };
    let Fields::Named(fields) = ds.fields else {
        return Err(syn::Error::new(ds.fields.span(), "Expected named fields"));
    };
    Ok(fields)
}

fn extract_option_type(field: &Field) -> Option<proc_macro2::TokenStream> {
    let path = match field.ty {
        Type::Path(ref path) => &path.path,
        _ => return None,
    };

    if path.segments.is_empty() || path.segments[0].ident != "Option" {
        return None;
    }
    let PathArguments::AngleBracketed(args) = path.segments[0].clone().arguments else {
        return None;
    };

    Some(args.args.to_token_stream())
}

fn extract_vec_type(field: &Field) -> Option<proc_macro2::TokenStream> {
    let path = match field.ty {
        Type::Path(ref path) => &path.path,
        _ => return None,
    };

    if path.segments.is_empty() || path.segments[0].ident != "Vec" {
        return None;
    }
    let PathArguments::AngleBracketed(args) = path.segments[0].clone().arguments else {
        return None;
    };

    Some(args.args.to_token_stream())
}

fn parse_builder_attribute(attrs: &Vec<Attribute>) -> syn::Result<Option<Ident>> {
    if attrs.is_empty() {
        return Ok(None);
    }
    let Some(meta) = attrs.iter().find_map(|attr| {
        let Meta::List(list) = &attr.meta else {
            return None;
        };
        if list.path.segments.is_empty() || list.path.segments[0].ident != "builder" {
            return None;
        }
        Some(list)
    }) else {
        return Ok(None);
    };

    let err = syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`");

    if meta.tokens.is_empty() {
        return Err(err);
    }

    let mut meta_tokens = meta.tokens.clone().into_iter();
    match meta_tokens.next() {
        Some(proc_macro2::TokenTree::Ident(i)) if i == "each" => {}
        _ => return Err(err),
    }
    match meta_tokens.next() {
        Some(proc_macro2::TokenTree::Punct(p)) if p.as_char().eq(&'=') => {}
        _ => return Err(err),
    }
    match meta_tokens.next() {
        Some(proc_macro2::TokenTree::Literal(ref l)) => {
            Ok(Some(Ident::new(l.to_string().trim_matches('"'), l.span())))
        }
        _ => Err(err),
    }
}
