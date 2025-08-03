//! Test utilities and helpers, available when std feature is enabled.
//!
//! This module provides shared utilities that are used by both integration tests
//! and benchmarks. It's only compiled when the `std` feature is enabled.
use {solana_pubkey::Pubkey, std::vec::Vec};

// Re-export address generation utilities
pub mod address_gen {
    use super::*;

    // Define simplified types for address generation (used by integration tests)
    #[derive(Clone, Copy)]
    pub enum AtaVariant {
        SplAta,
        PAtaLegacy,
        PAtaPrefunded,
    }

    #[derive(Clone, Copy)]
    pub enum TestBankId {
        Benchmarks,
        Failures,
    }

    #[derive(Clone, Copy, PartialEq)]
    pub enum AccountTypeId {
        Payer,
        Wallet,
        Mint,
        OwnerMint,
        NestedMint,
        Ata,
        NestedAta,
        OwnerAta,
    }

    /// Generate a structured pubkey from 4-byte coordinate system
    /// [variant, test_bank, test_number, account_type].
    /// Avoids some issues with test cross-contamination by using predictable
    /// addresses that avoid collisions.
    pub fn structured_pk(
        variant: AtaVariant,
        test_bank: TestBankId,
        test_number: u8,
        account_type: AccountTypeId,
    ) -> Pubkey {
        let variant_byte = match variant {
            AtaVariant::SplAta => 0x01,
            AtaVariant::PAtaLegacy => 0x02,
            AtaVariant::PAtaPrefunded => 0x03,
        };

        let test_bank_byte = match test_bank {
            TestBankId::Benchmarks => 0x10,
            TestBankId::Failures => 0x20,
        };

        let account_type_byte = match account_type {
            AccountTypeId::Payer => 0x01,
            AccountTypeId::Wallet => 0x02,
            AccountTypeId::Mint => 0x03,
            AccountTypeId::OwnerMint => 0x04,
            AccountTypeId::NestedMint => 0x05,
            AccountTypeId::Ata => 0x06,
            AccountTypeId::NestedAta => 0x07,
            AccountTypeId::OwnerAta => 0x08,
        };

        let mut bytes = [0u8; 32];
        bytes[0] = variant_byte;
        bytes[1] = test_bank_byte;
        bytes[2] = test_number;
        bytes[3] = account_type_byte;

        #[allow(clippy::needless_range_loop)]
        for i in 4..32 {
            bytes[i] = (i as u8)
                .wrapping_mul(variant_byte)
                .wrapping_add(test_number);
        }

        Pubkey::new_from_array(bytes)
    }

    /// Generate multiple structured pubkeys
    pub fn structured_pk_multi(
        variant: AtaVariant,
        test_bank: TestBankId,
        test_number: u8,
        account_types: &[AccountTypeId],
    ) -> Vec<Pubkey> {
        account_types
            .iter()
            .map(|&account_type| structured_pk(variant, test_bank, test_number, account_type))
            .collect()
    }

    /// Generate a random seeded pubkey for testing with multiple entropy sources
    pub fn random_seeded_pk(
        variant: AtaVariant,
        test_bank: TestBankId,
        test_number: u8,
        account_type: AccountTypeId,
        fixed_seed: u64,
        entropy: u64,
    ) -> Pubkey {
        // Start with structured pubkey as base
        let base = structured_pk(variant, test_bank, test_number, account_type);
        let mut bytes = base.to_bytes();

        // Mix in the entropy sources
        let fixed_seed_bytes = fixed_seed.to_le_bytes();
        let entropy_bytes = entropy.to_le_bytes();

        // XOR with entropy to randomize while keeping deterministic
        for i in 0..32 {
            bytes[i] ^= fixed_seed_bytes[i % 8];
            bytes[i] ^= entropy_bytes[i % 8];
            bytes[i] = bytes[i].wrapping_add((i as u8).wrapping_mul(test_number));
        }

        Pubkey::new_from_array(bytes)
    }

    /// Macro for structured pubkey array generation
    #[macro_export]
    macro_rules! pk_array {
        ($variant:expr, $test_bank:expr, $test_number:expr, [$($account_type:expr),+]) => {
            [$(
                $crate::test_helpers::address_gen::structured_pk(
                    $variant, $test_bank, $test_number, $account_type
                )
            ),+]
        };
    }
}
