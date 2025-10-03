use {
    ata_mollusk_harness::{
        build_create_ata_instruction, token_2022_immutable_owner_rent_exempt_balance,
        AtaTestHarness, CreateAtaInstructionType,
    },
    compare_programs::compare_programs,
    mollusk_svm::result::Check,
    solana_instruction::AccountMeta,
    solana_program_error::ProgramError,
    solana_pubkey::Pubkey,
    solana_sysvar as sysvar,
    spl_associated_token_account_interface::address::get_associated_token_address_with_program_id,
};

#[compare_programs]
fn test_associated_token_address() {
    let mut harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_2022_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
    .with_wallet_and_mint(1_000_000, 6);
    let (_addr, result) = harness.create_ata(CreateAtaInstructionType::default());
    compare_programs::log_cu_and_byte_comparison_ctx(
        &harness.ctx,
        "Create",
        Some(result.compute_units_consumed),
        None,
    );
}

#[compare_programs]
fn test_create_with_fewer_lamports() {
    let harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_2022_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
    .with_wallet_and_mint(1_000_000, 6);

    let wallet = harness.wallet.unwrap();
    let mint = harness.mint.unwrap();
    let ata_address = get_associated_token_address_with_program_id(
        &wallet,
        &mint,
        &spl_token_2022_interface::id(),
    );

    let insufficient_lamports = 890880;
    harness.ensure_account_exists_with_lamports(ata_address, insufficient_lamports);

    let instruction = build_create_ata_instruction(
        spl_associated_token_account_interface::program::id(),
        harness.payer,
        ata_address,
        wallet,
        mint,
        spl_token_2022_interface::id(),
        CreateAtaInstructionType::default(),
    );

    let result = harness.ctx.process_and_validate_instruction(
        &instruction,
        &[
            Check::success(),
            Check::account(&ata_address)
                .lamports(token_2022_immutable_owner_rent_exempt_balance())
                .owner(&spl_token_2022_interface::id())
                .build(),
        ],
    );
    compare_programs::log_cu_and_byte_comparison_ctx(
        &harness.ctx,
        "Create",
        Some(result.compute_units_consumed),
        None,
    );
}

#[compare_programs]
fn test_create_with_excess_lamports() {
    let harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_2022_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
    .with_wallet_and_mint(1_000_000, 6);

    let wallet = harness.wallet.unwrap();
    let mint = harness.mint.unwrap();
    let ata_address = get_associated_token_address_with_program_id(
        &wallet,
        &mint,
        &spl_token_2022_interface::id(),
    );

    let excess_lamports = token_2022_immutable_owner_rent_exempt_balance() + 1;
    harness.ensure_account_exists_with_lamports(ata_address, excess_lamports);

    let instruction = build_create_ata_instruction(
        spl_associated_token_account_interface::program::id(),
        harness.payer,
        ata_address,
        wallet,
        mint,
        spl_token_2022_interface::id(),
        CreateAtaInstructionType::default(),
    );

    let result = harness.ctx.process_and_validate_instruction(
        &instruction,
        &[
            Check::success(),
            Check::account(&ata_address)
                .lamports(excess_lamports)
                .owner(&spl_token_2022_interface::id())
                .build(),
        ],
    );
    compare_programs::log_cu_and_byte_comparison_ctx(
        &harness.ctx,
        "Create",
        Some(result.compute_units_consumed),
        None,
    );
}

#[compare_programs]
fn test_create_account_mismatch() {
    let harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_2022_interface::id(),
        compare_programs::current_program_filename(),
        compare_programs::seed(),
    )
    .with_wallet_and_mint(1_000_000, 6);

    let wallet = harness.wallet.unwrap();
    let mint = harness.mint.unwrap();
    let ata_address = get_associated_token_address_with_program_id(
        &wallet,
        &mint,
        &spl_token_2022_interface::id(),
    );

    for account_idx in [1, 2, 3] {
        let mut instruction = build_create_ata_instruction(
            spl_associated_token_account_interface::program::id(),
            harness.payer,
            ata_address,
            wallet,
            mint,
            spl_token_2022_interface::id(),
            CreateAtaInstructionType::default(),
        );

        instruction.accounts[account_idx] = if account_idx == 1 {
            AccountMeta::new(Pubkey::default(), false)
        } else {
            AccountMeta::new_readonly(Pubkey::default(), false)
        };

        let result = harness.ctx.process_and_validate_instruction(
            &instruction,
            &[Check::err(ProgramError::InvalidSeeds)],
        );
        let name = match account_idx {
            1 => "Create_InvalidSeeds_1",
            2 => "Create_InvalidSeeds_2",
            _ => "Create_InvalidSeeds_3",
        };
        compare_programs::log_cu_and_byte_comparison_ctx(
            &harness.ctx,
            name,
            Some(result.compute_units_consumed),
            None,
        );
    }
}

#[compare_programs]
fn test_create_associated_token_account_using_legacy_implicit_instruction() {
    let mut harness = AtaTestHarness::new_with_program_and_seed(
        &spl_token_2022_interface::id(),
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
                .push(AccountMeta::new_readonly(sysvar::rent::id(), false));
        },
    );
    compare_programs::log_cu_and_byte_comparison_ctx(&harness.ctx, "Create_Legacy", None, None);
}
