use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn compare_programs(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);
    let fn_name = input_fn.sig.ident.clone();
    let vis = input_fn.vis.clone();
    let block = (*input_fn.block).clone();

    // Rewrite calls to Pubkey::new_unique() into compare_programs::new_unique_pubkey()
    // Lightweight, ad-hoc token replacement inside the block string. This is a fallback if syn visit_mut isn't available.
    let mut ts = quote! { #block }.to_string();
    ts = ts.replace(
        "solana_pubkey :: Pubkey :: new_unique",
        "compare_programs :: new_unique_pubkey",
    );
    ts = ts.replace(
        "Pubkey :: new_unique",
        "compare_programs :: new_unique_pubkey",
    );
    let block: syn::Block = syn::parse_str(&format!("{{ {ts} }}")).unwrap_or(*input_fn.block);

    let prog_a = "spl_associated_token_account";
    let prog_b = "spl_associated_token_account";
    let label_a = "SPL ATA";
    let label_b = "p-ATA";

    // Wrap the body to run twice. We reconstruct the function to include the runs.
    let wrapped = quote! {
        #[test]
        #vis fn #fn_name() {
            let seed = {
                let mut h: u64 = 0xcbf29ce484222325;
                for b in stringify!(#fn_name).as_bytes() { h = h ^ (*b as u64); h = h.wrapping_mul(0x100000001b3); }
                h
            };
            compare_programs::set_test_name(stringify!(#fn_name));
            // First run
            compare_programs::set_run_config(0, #prog_a, #prog_b, #label_a, #label_b, seed);
            { #block }
            // Second run
            compare_programs::set_run_config(1, #prog_a, #prog_b, #label_a, #label_b, seed);
            { #block }
        }
    };

    TokenStream::from(wrapped)
}
