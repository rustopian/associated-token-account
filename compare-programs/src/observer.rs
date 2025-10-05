//! Instruction observation and program comparison logic.
//!
//! The observer captures instruction execution results and compares state across program versions.

use solana_account::Account;
use solana_pubkey::Pubkey;
use solana_system_interface::instruction::SystemInstruction;
use std::collections::BTreeMap;

use crate::report::{ComparisonRecord, ReportEvent};
use crate::{
    TLS_FILTER_PROGRAM_IDS, TLS_INDEX, TLS_INSTR_NAME, TLS_PENDING, TLS_TEST_NAME,
};

/// Extracts the suffix after a space, or returns the whole string.
fn context_suffix_or_whole(context_label: &str) -> String {
    if let Some((_prefix, rest)) = context_label.split_once(' ') {
        rest.to_string()
    } else {
        context_label.to_string()
    }
}

/// Extracts variant name from Debug output like "Variant { .. }" or "Variant(..)"
fn variant_name_only(debug_str: &str) -> String {
    if let Some((name, _)) = debug_str.split_once(' ') {
        name.to_string()
    } else if let Some((name, _)) = debug_str.split_once('(') {
        name.to_string()
    } else if let Some((name, _)) = debug_str.split_once('{') {
        name.to_string()
    } else {
        debug_str.to_string()
    }
}

/// Decodes instruction into a human-readable label and optional detail string.
///
/// Supports System, ATA, Token v1, and Token-2022 programs.
fn decode_instruction_label_and_detail(
    ix: &solana_instruction::Instruction,
) -> Option<(String, Option<String>)> {
    // System via Debug variant name
    if ix.program_id == solana_system_interface::program::id() {
        let dbg = bincode::deserialize::<SystemInstruction>(&ix.data)
            .ok()
            .map(|si| format!("{:?}", si))?;
        return Some((variant_name_only(&dbg), None));
    }
    // ATA via tag
    if ix.program_id == spl_associated_token_account_interface::program::id() {
        if ix.data.is_empty() {
            return Some(("CreateLegacyImplicit".to_string(), None));
        }
        let name = match ix.data.first().copied() {
            Some(0) => "Create",
            Some(1) => "CreateIdempotent",
            Some(2) => "RecoverNested",
            _ => "UnknownATA",
        };
        return Some((name.to_string(), None));
    }
    // SPL Token v1
    if ix.program_id == spl_token_interface::id() {
        if let Ok(instr) = spl_token_interface::instruction::TokenInstruction::unpack(&ix.data) {
            let dbg = format!("{:?}", instr);
            return Some((variant_name_only(&dbg), Some(dbg)));
        }
        return None;
    }
    // SPL Token-2022
    if ix.program_id == spl_token_2022_interface::id() {
        if let Ok(instr) = spl_token_2022_interface::instruction::TokenInstruction::unpack(&ix.data)
        {
            let dbg = format!("{:?}", instr);
            return Some((variant_name_only(&dbg), Some(dbg)));
        }
        return None;
    }
    None
}

/// Core comparison logic: stores baseline run (index 0) or compares against it (index 1).
pub(crate) fn finalize_comparison(
    key: &str,
    instruction_name: &str,
    context: Option<String>,
    instruction_detail: Option<String>,
    cu_used: Option<u64>,
    result_str: Option<&str>,
    account_snapshot: BTreeMap<Pubkey, Account>,
) {
    let test_name = TLS_TEST_NAME.with(|n| n.borrow().clone());
    let idx = TLS_INDEX.with(|c| c.get());

    match idx {
        0 => {
            TLS_PENDING.with(|p| {
                p.borrow_mut().insert(
                    (test_name.clone(), key.to_string()),
                    crate::PendingRun {
                        compute_units: cu_used,
                        result: result_str.map(|s| s.to_string()),
                        snapshot: account_snapshot,
                    },
                );
            });
        }
        _ => {
            let (mut compute_units_a, mut result_a, mut snapshot_a) = (None, None, BTreeMap::new());
            TLS_PENDING.with(|p| {
                if let Some(pending) = p.borrow_mut().remove(&(test_name.clone(), key.to_string()))
                {
                    compute_units_a = pending.compute_units;
                    result_a = pending.result;
                    snapshot_a = pending.snapshot;
                }
            });
            let byte_equal = if !snapshot_a.is_empty() {
                Some(snapshot_a == account_snapshot)
            } else {
                None
            };
            let record = ComparisonRecord {
                test: test_name,
                instruction: instruction_name.to_string(),
                context,
                instruction_detail,
                compute_units_a,
                compute_units_b: cu_used,
                byte_equal,
                result_a,
                result_b: result_str.map(|s| s.to_string()),
                accounts_a: snapshot_a,
                accounts_b: account_snapshot,
            };
            crate::report::emit_report_event(ReportEvent::Comparison { record });
        }
    }
}

pub(crate) fn log_instruction_logs(instruction: &str, logs: &[String]) {
    let test_name = TLS_TEST_NAME.with(|n| n.borrow().clone());
    let label = crate::current_program_label().to_string();
    crate::report::emit_report_event(ReportEvent::InstructionLogs {
        test: test_name,
        instruction: instruction.to_string(),
        program_label: label,
        logs: logs.to_vec(),
    });
}

/// Default observer for capturing and comparing instruction execution.
///
/// Filters by program ID, captures logs, decodes instructions, snapshots account state,
/// and triggers comparison logic.
pub fn default_observer(
    ix: &solana_instruction::Instruction,
    res: &mollusk_svm::result::InstructionResult,
    invoke_ctx: &solana_program_runtime::invoke_context::InvokeContext,
) {
    if !TLS_FILTER_PROGRAM_IDS
        .with(|ids| ids.borrow().is_empty() || ids.borrow().contains(&ix.program_id))
    {
        return;
    }

    let logs = invoke_ctx
        .get_log_collector()
        .map_or_else(Vec::new, |lc| lc.borrow().get_recorded_content().to_vec());
    let context_label = TLS_INSTR_NAME.with(|n| n.borrow().clone());
    log_instruction_logs(&context_label, &logs);
    let (parsed_instr, instruction_detail) = decode_instruction_label_and_detail(ix)
        .unwrap_or_else(|| (context_suffix_or_whole(&context_label), None));
    let result_str = if res.program_result.is_ok() {
        "OK"
    } else {
        "ERR"
    };

    let ctx_ptr = crate::TLS_CTX_PTR.with(|c| *c.borrow());
    if !ctx_ptr.is_null() {
        let ctx_ref = unsafe { &*ctx_ptr };
        let store = ctx_ref.account_store.borrow();
        let account_snapshot: BTreeMap<Pubkey, Account> =
            store.iter().map(|(k, v)| (*k, v.clone())).collect();
        finalize_comparison(
            &context_label,
            &parsed_instr,
            Some(context_label.clone()),
            instruction_detail,
            Some(res.compute_units_consumed),
            Some(result_str),
            account_snapshot,
        );
    }
}
