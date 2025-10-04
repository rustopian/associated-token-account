use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
    Expr, ExprCall, ExprMethodCall, ExprPath, Ident, ItemFn,
};

#[proc_macro_attribute]
pub fn compare_programs(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);
    let fn_name = input_fn.sig.ident.clone();
    let vis = input_fn.vis.clone();

    // Visit the function body to rewrite calls and paths
    let mut body = (*input_fn.block).clone();
    {
        let mut rewriter = BodyRewriter {};
        rewriter.visit_block_mut(&mut body);
    }

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
            {
                let mut __cp_step: usize = 0;
                #body
            }
            // Second run
            compare_programs::set_run_config(1, #prog_a, #prog_b, #label_a, #label_b, seed);
            {
                let mut __cp_step: usize = 0;
                #body
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

        // Rewrite Pubkey::new_unique() and solana_pubkey::Pubkey::new_unique()
        if let Expr::Path(ExprPath { path, .. }) = &*node.func {
            let segs: Vec<_> = path.segments.iter().collect();
            let matches_pubkey = segs.len() >= 2
                && segs[segs.len() - 2].ident == "Pubkey"
                && segs[segs.len() - 1].ident == "new_unique";
            if matches_pubkey {
                // Replace callee with compare_programs::new_unique_pubkey
                node.func = Box::new(parse_quote!(compare_programs::new_unique_pubkey));
            }
        }
    }
}
