use ata_mollusk_harness::{AtaTestHarness, CreateAtaInstructionType};
use compare_programs::compare_programs;

#[compare_programs]
fn success_create() {
    let mut harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
    .with_wallet_and_mint(1_000_000, 6);
    let (_addr, result) = harness.create_ata(CreateAtaInstructionType::default());
    compare_programs::log_cu_and_byte_comparison_ctx(&harness.ctx, "Create", Some(result.compute_units_consumed));
}

#[compare_programs]
fn success_using_deprecated_instruction_creator() {
    let mut harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
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
    compare_programs::log_cu_and_byte_comparison_ctx(&harness.ctx, "Create_Legacy", None);
}
