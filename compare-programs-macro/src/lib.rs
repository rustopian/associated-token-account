use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
    Expr, ExprCall, ExprMethodCall, ExprPath, Ident, ItemFn, LitStr,
};

#[proc_macro_attribute]
pub fn instrument(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input_fn = parse_macro_input!(input as ItemFn);

    // Rewrite the function body using the same transformations as the test macro
    let mut body = (*input_fn.block).clone();
    {
        let mut rewriter = BodyRewriter {};
        rewriter.visit_block_mut(&mut body);
    }

    // Provide a local step counter, mirroring the test wrapper behavior
    let wrapped = quote!({
        let mut __cp_step: usize = 0;
        #body
    });

    input_fn.block = Box::new(parse_quote!(#wrapped));
    TokenStream::from(quote!(#input_fn))
}

#[proc_macro_attribute]
pub fn compare_programs(args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);
    let fn_name = input_fn.sig.ident.clone();
    let vis = input_fn.vis.clone();

    // Visit the function body to rewrite calls and paths
    let mut body = (*input_fn.block).clone();
    {
        let mut rewriter = BodyRewriter {};
        rewriter.visit_block_mut(&mut body);
    }

    // Defaults
    let mut programs: Vec<String> = Vec::new();
    let mut filter_program_ids: Vec<String> = Vec::new();

    // Parse attribute: programs=[..], filter_program_ids=[..]
    if !args.is_empty() {
        let args_str = args.to_string();
        // programs
        if let Some(i) = args_str.find("programs") {
            if let Some(start) = args_str[i..].find('[') {
                let start_idx = i + start + 1;
                if let Some(end_rel) = args_str[start_idx..].find(']') {
                    let end_idx = start_idx + end_rel;
                    let inside = &args_str[start_idx..end_idx];
                    let vals: Vec<String> = inside
                        .split('"')
                        .filter_map(|t| {
                            let t = t.trim();
                            if t.is_empty() || t == "," {
                                None
                            } else {
                                Some(t.to_string())
                            }
                        })
                        .collect();
                    if !vals.is_empty() {
                        programs = vals;
                    }
                }
            }
        }
        // filter_program_ids
        if let Some(i) = args_str.find("filter_program_ids") {
            if let Some(start) = args_str[i..].find('[') {
                let start_idx = i + start + 1;
                if let Some(end_rel) = args_str[start_idx..].find(']') {
                    let end_idx = start_idx + end_rel;
                    let inside = &args_str[start_idx..end_idx];
                    let vals: Vec<String> = inside
                        .split('"')
                        .filter_map(|t| {
                            let t = t.trim();
                            if t.is_empty() || t == "," {
                                None
                            } else {
                                Some(t.to_string())
                            }
                        })
                        .collect();
                    if !vals.is_empty() {
                        filter_program_ids = vals;
                    }
                }
            }
        }
    }

    assert!(
        !programs.is_empty(),
        "compare_programs: 'programs' must be provided and non-empty"
    );
    let program_literals: Vec<proc_macro2::TokenStream> = programs
        .iter()
        .map(|p| {
            let lit = LitStr::new(p, proc_macro2::Span::call_site());
            quote! { #lit }
        })
        .collect();

    // Convert filter_program_ids into token stream
    let filter_ids_tokens: Vec<proc_macro2::TokenStream> = filter_program_ids
        .iter()
        .map(|id| {
            let tokens: proc_macro2::TokenStream = id.parse().expect("Invalid program ID path");
            quote! { #tokens }
        })
        .collect();

    // Wrap the body to run for each variant against baseline with two runs per variant
    let wrapped = quote! {
        #[test]
        #vis fn #fn_name() {
            let seed = {
                let mut h: u64 = 0xcbf29ce484222325;
                for b in stringify!(#fn_name).as_bytes() { h = h ^ (*b as u64); h = h.wrapping_mul(0x100000001b3); }
                h
            };
            compare_programs::set_test_name(stringify!(#fn_name));
            compare_programs::set_test_suite(file!());
            compare_programs::set_filter_program_ids(&[#(#filter_ids_tokens),*]);

            let __cp_programs: &[&str] = &[#(#program_literals),*];
            assert!(__cp_programs.len() >= 2, "programs must have at least 2 entries");
            let __cp_baseline = __cp_programs[0];
            for __cp_variant in &__cp_programs[1..] {
                // Run baseline
                compare_programs::set_run_config(0, __cp_baseline, __cp_variant, "Base", *__cp_variant, seed);
                {
                    let mut __cp_step: usize = 0;
                    #body
                }
                // Run variant
                compare_programs::set_run_config(1, __cp_baseline, __cp_variant, "Base", *__cp_variant, seed);
                {
                    let mut __cp_step: usize = 0;
                    #body
                }
            }
        }
    };

    TokenStream::from(wrapped)
}

struct BodyRewriter {}

impl VisitMut for BodyRewriter {
    fn visit_expr_method_call_mut(&mut self, node: &mut ExprMethodCall) {
        // Recurse first to transform nested expressions
        visit_mut::visit_expr_method_call_mut(self, node);

        let method_ident = node.method.to_string();
        if method_ident == "process_and_validate_instruction" {
            // Rename method
            node.method = Ident::new(
                "process_and_validate_instruction_with_observer",
                node.method.span(),
            );
            // Capture original args
            let original_ix: Expr = node
                .args
                .first()
                .cloned()
                .expect("expected instruction arg");
            let original_checks: Expr = node
                .args
                .iter()
                .nth(1)
                .cloned()
                .expect("expected checks arg");

            let receiver_expr: Expr = (*node.receiver).clone();

            // Build wrapped first arg that sets TLS (name + ctx) and yields original ix
            let first_arg_wrapped: Expr = parse_quote!({
                let __cp_name = { __cp_step += 1; format!("step-{:03}", __cp_step) };
                compare_programs::set_current_instruction_name(&__cp_name);
                compare_programs::set_current_ctx_ptr(&#receiver_expr);
                #original_ix
            });

            // Rebuild arg list: wrapped ix, original checks, function pointer observer
            let mut new_args: syn::punctuated::Punctuated<Expr, syn::token::Comma> =
                Default::default();
            new_args.push(first_arg_wrapped);
            new_args.push(original_checks);
            new_args.push(parse_quote!(compare_programs::default_observer));
            node.args = new_args;
        } else if method_ident == "process_and_validate_instruction_chain" {
            node.method = Ident::new(
                "process_and_validate_instruction_chain_with_observer",
                node.method.span(),
            );

            // Wrap the first arg (the instruction slice) to set ctx before call
            let original_slice: Expr = node
                .args
                .first()
                .cloned()
                .expect("expected instruction slice arg");
            let receiver_expr: Expr = (*node.receiver).clone();
            let first_arg_wrapped: Expr = parse_quote!({
                compare_programs::set_current_ctx_ptr(&#receiver_expr);
                #original_slice
            });

            // Replace first arg
            if let Some(first) = node.args.first_mut() {
                *first = first_arg_wrapped;
            }

            // Append minimal per-step observer
            node.args.push(parse_quote!(
                |__cp_step_ix: usize, __cp_res, __cp_invoke_ctx| {
                    let __cp_name = format!("chain-step-{:03}", __cp_step_ix);
                    compare_programs::set_current_instruction_name(&__cp_name);
                    let __cp_dummy_ix = solana_instruction::Instruction {
                        program_id: __cp_res.program_id,
                        accounts: vec![],
                        data: vec![],
                    };
                    compare_programs::default_observer(&__cp_dummy_ix, &__cp_res, __cp_invoke_ctx);
                }
            ));
        }
    }

    fn visit_expr_call_mut(&mut self, node: &mut ExprCall) {
        // Recurse
        visit_mut::visit_expr_call_mut(self, node);

        // Rewrite new_unique() calls for Pubkey, Keypair, and Address
        if let Expr::Path(ExprPath { path, .. }) = &*node.func {
            let segs: Vec<_> = path.segments.iter().collect();
            if segs.len() >= 2 && segs[segs.len() - 1].ident == "new_unique" {
                let type_name = segs[segs.len() - 2].ident.to_string();
                let replacement = match type_name.as_str() {
                    "Pubkey" => Some(parse_quote!(compare_programs::new_unique_pubkey)),
                    "Keypair" => Some(parse_quote!(compare_programs::new_unique_keypair)),
                    "Address" => Some(parse_quote!(compare_programs::new_unique_address)),
                    _ => None,
                };
                if let Some(new_func) = replacement {
                    node.func = Box::new(new_func);
                }
            }
        }
    }
}
