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

            // receiver expression reference to pass into logger
            let receiver_expr: Expr = (*node.receiver).clone();

            // Append observer closure as last argument
            let observer: Expr = parse_quote!({
                let __cp_name = { __cp_step += 1; format!("step-{:03}", __cp_step) };
                let __cp_ctx_ref = &#receiver_expr;
                move |__cp_res, __cp_invoke_ctx| {
                    let __cp_result_str: &str = if __cp_res.program_result.is_ok() { "OK" } else { "ERR" };
                    compare_programs::log_cu_and_byte_comparison_ctx(
                        __cp_ctx_ref,
                        &__cp_name,
                        Some(__cp_res.compute_units_consumed as u64),
                        Some(__cp_result_str),
                    );
                }
            });
            node.args.push(observer);
        } else if method_ident == "process_and_validate_instruction_chain" {
            node.method = Ident::new(
                "process_and_validate_instruction_chain_with_observer",
                node.method.span(),
            );

            let receiver_expr: Expr = (*node.receiver).clone();
            let observer: Expr = parse_quote!({
                let __cp_ctx_ref = &#receiver_expr;
                move |__cp_step_ix: usize, __cp_res, __cp_invoke_ctx| {
                    let __cp_name = format!("chain-step-{:03}", __cp_step_ix);
                    let __cp_result_str: &str = if __cp_res.program_result.is_ok() { "OK" } else { "ERR" };
                    compare_programs::log_cu_and_byte_comparison_ctx(
                        __cp_ctx_ref,
                        &__cp_name,
                        Some(__cp_res.compute_units_consumed as u64),
                        Some(__cp_result_str),
                    );
                }
            });
            node.args.push(observer);
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
