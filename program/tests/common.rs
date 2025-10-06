use compare_programs::{decoder_for_borsh, register_decoder};
use spl_associated_token_account_interface::instruction::AssociatedTokenAccountInstruction;

#[ctor::ctor]
fn setup() {
    register_decoder(decoder_for_borsh::<AssociatedTokenAccountInstruction>(
        spl_associated_token_account_interface::program::id(),
    ));
}
