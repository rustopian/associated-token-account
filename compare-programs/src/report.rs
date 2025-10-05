//! Report generation and output formatting.
//!
//! Handles all output formats: CSV summaries, detailed text reports, and instruction logs.

use once_cell::sync::OnceCell;
use solana_account::Account;
use solana_pubkey::Pubkey;
use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::Mutex;

use crate::{PROGRAM_FILENAMES, TLS_SUITE_NAME};

#[derive(Clone)]
pub(crate) struct ComparisonRecord {
    pub test: String,
    pub instruction: String,
    pub context: Option<String>,
    pub instruction_detail: Option<String>,
    pub compute_units_a: Option<u64>,
    pub compute_units_b: Option<u64>,
    pub byte_equal: Option<bool>,
    pub result_a: Option<String>,
    pub result_b: Option<String>,
    pub accounts_a: BTreeMap<Pubkey, Account>,
    pub accounts_b: BTreeMap<Pubkey, Account>,
}

pub(crate) enum ReportEvent {
    InstructionLogs {
        test: String,
        instruction: String,
        program_label: String,
        logs: Vec<String>,
    },
    Comparison {
        record: ComparisonRecord,
    },
}

static WRITE_LOCK: OnceCell<Mutex<()>> = OnceCell::new();
static RUN_PREFIX: OnceCell<String> = OnceCell::new();

fn with_compare_programs_file<F>(filename: &str, write_fn: F)
where
    F: FnOnce(&mut std::fs::File, bool),
{
    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    let prefix = RUN_PREFIX.get_or_init(crate::get_or_make_session_prefix);
    path.push(format!("{}-{}", prefix, filename));
    if let Ok(_g) = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock() {
        if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
            let header_needed = f.metadata().map(|m| m.len() == 0).unwrap_or(true);
            write_fn(&mut f, header_needed);
        }
    }
}

fn write_missing_and_extra(
    buffer: &mut String,
    accounts_a: &BTreeMap<Pubkey, Account>,
    accounts_b: &BTreeMap<Pubkey, Account>,
) {
    for pubkey in accounts_a
        .keys()
        .filter(|pubkey| !accounts_b.contains_key(*pubkey))
    {
        let _ = writeln!(buffer, "- Missing in B: {}", pubkey);
    }
    for pubkey in accounts_b
        .keys()
        .filter(|pubkey| !accounts_a.contains_key(*pubkey))
    {
        let _ = writeln!(buffer, "+ Extra in B: {}", pubkey);
    }
}

fn hex_window(data: &[u8], start: usize, end: usize) -> String {
    let mut s = String::new();
    for (i, b) in data[start..end].iter().enumerate() {
        if i > 0 {
            let _ = write!(s, " ");
        }
        let _ = write!(s, "{:02x}", b);
    }
    s
}

/// Returns a diff summary string and writes detailed hex comparison to buffer.
fn write_data_diff(buffer: &mut String, data_a: &[u8], data_b: &[u8]) -> String {
    if data_a.len() != data_b.len() {
        return format!("data_len: {} -> {}", data_a.len(), data_b.len());
    }

    let (first_idx, total) = data_a
        .iter()
        .zip(data_b.iter())
        .enumerate()
        .filter(|(_, (a, b))| a != b)
        .fold((0, 0), |(first, count), (i, _)| {
            (if count == 0 { i } else { first }, count + 1)
        });

    if total > 0 {
        let start = first_idx.saturating_sub(16);
        let end = (start + 64).min(data_a.len());
        let _ = writeln!(
            buffer,
            "  data_diff: first_at={}, differing_bytes={}",
            first_idx, total
        );
        let _ = writeln!(buffer, "  A: {}", hex_window(data_a, start, end));
        let _ = writeln!(buffer, "  B: {}", hex_window(data_b, start, end));
    }
    "data: bytes differ".to_string()
}

macro_rules! compare_field {
    ($diffs:expr, $a:expr, $b:expr, $name:literal) => {
        if $a != $b {
            $diffs.push(format!("{}: {} -> {}", $name, $a, $b));
        }
    };
}

fn write_account_changes(
    buffer: &mut String,
    accounts_a: &BTreeMap<Pubkey, Account>,
    accounts_b: &BTreeMap<Pubkey, Account>,
) {
    for (pubkey, account_a) in accounts_a.iter() {
        if let Some(account_b) = accounts_b.get(pubkey) {
            let mut diffs: Vec<String> = Vec::new();
            compare_field!(diffs, account_a.lamports, account_b.lamports, "lamports");
            compare_field!(diffs, account_a.owner, account_b.owner, "owner");
            if account_a.data != account_b.data {
                diffs.push(write_data_diff(buffer, &account_a.data, &account_b.data));
            }
            compare_field!(
                diffs,
                account_a.executable,
                account_b.executable,
                "executable"
            );
            compare_field!(
                diffs,
                account_a.rent_epoch,
                account_b.rent_epoch,
                "rent_epoch"
            );
            if !diffs.is_empty() {
                let _ = writeln!(buffer, "* Changed {}: {}", pubkey, diffs.join(", "));
            }
        }
    }
}

fn write_accounts_section(buffer: &mut String, title: &str, accounts: &BTreeMap<Pubkey, Account>) {
    let _ = writeln!(buffer, "{}", title);
    for (pubkey, account) in accounts.iter() {
        let _ = writeln!(
            buffer,
            "{} owner={} lamports={} data_len={}",
            pubkey,
            account.owner,
            account.lamports,
            account.data.len()
        );
    }
}

fn write_csv_summary(f: &mut std::fs::File, header_needed: bool, record: &ComparisonRecord) {
    if header_needed {
        let (a, b) = PROGRAM_FILENAMES
            .get()
            .cloned()
            .unwrap_or_else(|| ("A".to_string(), "B".to_string()));
        let _ = f.write_all(
            format!(
                "suite,test,instruction,context,{a} CUs,{b} CUs,ByteEqual,{a} Result,{b} Result\n"
            )
            .as_bytes(),
        );
    }
    let byte_equality_label = record
        .byte_equal
        .map(|b| if b { "100%" } else { "DIFF" })
        .unwrap_or("");
    let _ = f.write_all(
        format!(
            "{},{},{},{},{},{},{},{},{}\n",
            TLS_SUITE_NAME.with(|n| n.borrow().clone()),
            record.test,
            record.instruction,
            record.context.as_deref().unwrap_or_default(),
            record
                .compute_units_a
                .map(|v| v.to_string())
                .unwrap_or_default(),
            record
                .compute_units_b
                .map(|v| v.to_string())
                .unwrap_or_default(),
            byte_equality_label,
            record.result_a.as_deref().unwrap_or_default(),
            record.result_b.as_deref().unwrap_or_default(),
        )
        .as_bytes(),
    );
}

fn write_details_report(f: &mut std::fs::File, record: &ComparisonRecord) {
    let mut buffer = String::new();
    let _ = writeln!(buffer, "=== {} :: {} ===", record.test, record.instruction);
    if let Some(detail) = record.instruction_detail.as_deref() {
        let _ = writeln!(buffer, "Instruction: {}", detail);
    }
    let _ = writeln!(
        buffer,
        "CUs: A={}, B={}",
        record
            .compute_units_a
            .map(|v| v.to_string())
            .unwrap_or_default(),
        record
            .compute_units_b
            .map(|v| v.to_string())
            .unwrap_or_default()
    );
    let _ = writeln!(
        buffer,
        "ByteEqual: {}",
        record
            .byte_equal
            .map(|b| if b { "100%" } else { "DIFF" })
            .unwrap_or("")
    );
    if let Some(ctx) = record.context.as_deref() {
        let _ = writeln!(buffer, "Context: {}", ctx);
    }
    if let Some(result_a_str) = record.result_a.as_deref() {
        let _ = writeln!(buffer, "Result A: {}", result_a_str);
    }
    if let Some(result_b_str) = record.result_b.as_deref() {
        let _ = writeln!(buffer, "Result B: {}", result_b_str);
    }
    write_missing_and_extra(&mut buffer, &record.accounts_a, &record.accounts_b);
    write_account_changes(&mut buffer, &record.accounts_a, &record.accounts_b);
    if record.accounts_a == record.accounts_b {
        let _ = writeln!(buffer, "No state differences");
    }
    write_accounts_section(&mut buffer, "-- Accounts A --", &record.accounts_a);
    write_accounts_section(&mut buffer, "-- Accounts B --", &record.accounts_b);
    buffer.push('\n');
    let _ = f.write_all(buffer.as_bytes());
}

pub(crate) fn emit_report_event(event: ReportEvent) {
    match event {
        ReportEvent::InstructionLogs {
            test,
            instruction,
            program_label,
            logs,
        } => {
            let mut buffer = String::new();
            let _ = writeln!(
                buffer,
                "=== {} :: {} :: {} ===",
                test, instruction, program_label
            );
            for line in logs.iter() {
                let _ = writeln!(buffer, "{}", line);
            }
            buffer.push('\n');
            with_compare_programs_file("logs.txt", |f, _| {
                let _ = f.write_all(buffer.as_bytes());
            });
        }
        ReportEvent::Comparison { record } => {
            with_compare_programs_file("summary.csv", |f, header_needed| {
                write_csv_summary(f, header_needed, &record);
            });
            with_compare_programs_file("details.txt", |f, _| {
                write_details_report(f, &record);
            });
        }
    }
}
