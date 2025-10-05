use ata_mollusk_harness::{AtaTestHarness, CreateAtaInstructionType};
use compare_programs::compare_programs;

#[compare_programs(programs = ["spl_associated_token_account","spl_associated_token_account"], filter_program_ids = ["spl_associated_token_account_interface::program::id()"])]
fn success_create() {
    let mut harness =
        AtaTestHarness::new_with_seed(&spl_token_interface::id(), compare_programs::seed())
            .with_wallet_and_mint(1_000_000, 6);
    harness.create_ata(CreateAtaInstructionType::default());
}

#[compare_programs(programs = ["spl_associated_token_account","spl_associated_token_account"], filter_program_ids = ["spl_associated_token_account_interface::program::id()"])]
fn success_using_deprecated_instruction_creator() {
    let mut harness =
        AtaTestHarness::new_with_seed(&spl_token_interface::id(), compare_programs::seed())
            .with_wallet_and_mint(1_000_000, 6);

    harness.create_and_check_ata_with_custom_instruction(
        CreateAtaInstructionType::default(),
        |instruction| {
            instruction.data = vec![];
            instruction
                .accounts
                .push(solana_instruction::AccountMeta::new_readonly(
                    solana_sysvar::rent::id(),
                    false,
                ));
        },
    );
}
