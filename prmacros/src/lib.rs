use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, ItemEnum, Path,
    LitStr, PatTupleStruct, Pat, Token,
    parse::{ParseStream, Result as ParseResult, Parse}
};

struct EnumStrInput {
    ty: Path,
    items: Vec<(TokenStream2, TokenStream2)>,
}

impl Parse for EnumStrInput {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let ty = input.parse()?;
        input.parse::<Token![:]>()?;
        let mut items = Vec::new();
        while {
            let s: LitStr = input.parse()?;
            let s = s.into_token_stream();
            input.parse::<Token![=>]>()?;
            let p: Pat = input.parse()?;
            let p = p.into_token_stream();
            items.push((s, p));
            input.parse::<Token![,]>().is_ok()
        } {}
        Ok(EnumStrInput {
            ty, items
        })
    }
}

#[proc_macro]
pub fn enum_str(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as EnumStrInput);
    let ty = &item.ty;
    let pats = item.items.iter().map(|(lit, pat)| quote! {
        #lit => #pat
    });
    let strs = item.items.iter().map(|(lit, pat)| quote! {
        #pat => #lit
    });
    TokenStream::from(quote! {
        impl #ty {
            pub fn maybe_str(ty: &str) -> Option<Self> {
                Some(match ty {
                    #(#pats ,)*
                    _ => return None
                })
            }

            fn as_str(&self) -> &'static str {
                match self {
                    #(#strs ,)*
                    _ => "unknown"
                }
            }
        }
    })
}
