pub use compare_programs_macro::compare_programs;
use mollusk_svm::MolluskContext;
use once_cell::sync::OnceCell;
use solana_account::Account;
use solana_pubkey::Pubkey;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::fmt::Write as FmtWrite;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

#[derive(Clone)]
struct PendingRun {
    compute_units: Option<u64>,
    result: Option<String>,
    snapshot: BTreeMap<Pubkey, Account>,
}

#[derive(Clone)]
struct ComparisonRecord {
    test: String,
    instruction: String,
    note: Option<String>,
    compute_units_a: Option<u64>,
    compute_units_b: Option<u64>,
    byte_equal: Option<bool>,
    result_a: Option<String>,
    result_b: Option<String>,
    accounts_a: BTreeMap<Pubkey, Account>,
    accounts_b: BTreeMap<Pubkey, Account>,
}

static PROGRAM_FILENAMES: OnceCell<(String, String)> = OnceCell::new();
static PROGRAM_LABELS: OnceCell<(String, String)> = OnceCell::new();
static CURRENT_INDEX: OnceCell<AtomicUsize> = OnceCell::new();
static TEST_SEED: OnceCell<u64> = OnceCell::new();
thread_local! {
    static TLS_TEST_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    static TLS_INDEX: Cell<usize> = const { Cell::new(0) };
    static TLS_PENDING: RefCell<HashMap<(String, String), PendingRun>> = RefCell::new(HashMap::new());
}
thread_local! {
    static TLS_CTX_PTR: RefCell<*const MolluskContext<HashMap<Pubkey, Account>>> = const { RefCell::new(std::ptr::null()) };
    static TLS_INSTR_NAME: RefCell<String> = const { RefCell::new(String::new()) };
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

fn write_account_changes(
    buffer: &mut String,
    accounts_a: &BTreeMap<Pubkey, Account>,
    accounts_b: &BTreeMap<Pubkey, Account>,
) {
    for (pubkey, account_a) in accounts_a.iter() {
        if let Some(account_b) = accounts_b.get(pubkey) {
            let mut diffs: Vec<String> = Vec::new();
            if account_a.lamports != account_b.lamports {
                diffs.push(format!(
                    "lamports: {} -> {}",
                    account_a.lamports, account_b.lamports
                ));
            }
            if account_a.owner != account_b.owner {
                diffs.push(format!("owner: {} -> {}", account_a.owner, account_b.owner));
            }
            if account_a.data != account_b.data {
                if account_a.data.len() != account_b.data.len() {
                    diffs.push(format!(
                        "data_len: {} -> {}",
                        account_a.data.len(),
                        account_b.data.len()
                    ));
                } else {
                    diffs.push("data: bytes differ".to_string());
                    let mut first_difference_index: Option<usize> = None;
                    let mut total_diffs: usize = 0;
                    for (i, (byte_a, byte_b)) in
                        account_a.data.iter().zip(account_b.data.iter()).enumerate()
                    {
                        if byte_a != byte_b {
                            total_diffs += 1;
                            if first_difference_index.is_none() {
                                first_difference_index = Some(i);
                            }
                        }
                    }
                    let first_index = first_difference_index.unwrap_or(0);
                    let start = first_index.saturating_sub(16);
                    let end = (start + 64).min(account_a.data.len());
                    let a_hex_window = hex_window(&account_a.data, start, end);
                    let b_hex_window = hex_window(&account_b.data, start, end);
                    let _ = writeln!(
                        buffer,
                        "  data_diff: first_at={}, differing_bytes={}",
                        first_index, total_diffs
                    );
                    let _ = writeln!(buffer, "  A: {}", a_hex_window);
                    let _ = writeln!(buffer, "  B: {}", b_hex_window);
                }
            }
            if account_a.executable != account_b.executable {
                diffs.push(format!(
                    "executable: {} -> {}",
                    account_a.executable, account_b.executable
                ));
            }
            if account_a.rent_epoch != account_b.rent_epoch {
                diffs.push(format!(
                    "rent_epoch: {} -> {}",
                    account_a.rent_epoch, account_b.rent_epoch
                ));
            }
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

static WRITE_LOCK: OnceCell<Mutex<()>> = OnceCell::new();

thread_local! { static TLS_RNG: RefCell<Option<u64>> = const { RefCell::new(None) }; }

enum ReportEvent {
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

fn with_compare_programs_file<F>(filename: &str, write_fn: F)
where
    F: FnOnce(&mut std::fs::File, bool),
{
    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    path.push(filename);
    let header_needed = fs::metadata(&path).map(|m| m.len() == 0).unwrap_or(true);
    if let Ok(_g) = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock() {
        if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
            write_fn(&mut f, header_needed);
        }
    }
}

fn emit_report_event(event: ReportEvent) {
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
            with_compare_programs_file("report.csv", |f, header_needed| {
                if header_needed {
                    let (a, b) = PROGRAM_LABELS
                        .get()
                        .cloned()
                        .unwrap_or_else(|| ("A".to_string(), "B".to_string()));
                    let _ = f.write_all(
                        format!(
                            "test,instruction,note,{a} CUs,{b} CUs,ByteEqual,{a} Result,{b} Result\n"
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
                        "{},{},{},{},{},{},{},{}\n",
                        record.test,
                        record.instruction,
                        record.note.as_deref().unwrap_or_default(),
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
            });

            // Full details
            let mut buffer = String::new();
            let _ = writeln!(buffer, "=== {} :: {} ===", record.test, record.instruction);
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
            with_compare_programs_file("report_details.txt", |f, _| {
                let _ = f.write_all(buffer.as_bytes());
            });
        }
    }
}

fn rng_next_bytes32() -> [u8; 32] {
    TLS_RNG.with(|cell| {
        if cell.borrow().is_none() {
            // Derive a fresh account using OS RNG via Pubkey::new_unique()
            let account = Pubkey::new_unique();
            let seed_u64 = u64::from_le_bytes(account.to_bytes()[..8].try_into().unwrap());
            *cell.borrow_mut() = Some(seed_u64);
        }
        let mut bytes = [0u8; 32];
        let mut binding = cell.borrow_mut();
        let state = binding.as_mut().unwrap();
        // LCG advance; produces different bytes per test session, but repeats across A/B runs
        const A: u64 = 6364136223846793005;
        const C: u64 = 1;
        for chunk in bytes.chunks_mut(8) {
            *state = state.wrapping_mul(A).wrapping_add(C);
            chunk.copy_from_slice(&state.to_le_bytes());
        }
        bytes
    })
}

pub fn new_unique_pubkey() -> solana_pubkey::Pubkey {
    solana_pubkey::Pubkey::new_from_array(rng_next_bytes32())
}

pub fn set_run_config(
    index: usize,
    a_filename: &str,
    b_filename: &str,
    a_label: &str,
    b_label: &str,
    seed: u64,
) {
    ensure_sbf_out_dir();
    let _ = PROGRAM_FILENAMES.set((a_filename.to_string(), b_filename.to_string()));
    let _ = PROGRAM_LABELS.set((a_label.to_string(), b_label.to_string()));
    // Maintain a process-wide index for legacy callers, but use TLS for correctness under parallel tests
    let idx = CURRENT_INDEX.get_or_init(|| AtomicUsize::new(index));
    idx.store(index, Ordering::SeqCst);
    TLS_INDEX.with(|c| c.set(index));
    let _ = TEST_SEED.set(seed);
}

pub fn set_test_name(name: &str) {
    TLS_TEST_NAME.with(|n| *n.borrow_mut() = name.to_string());
}

pub fn current_program_filename() -> &'static str {
    let (a, b) = PROGRAM_FILENAMES
        .get()
        .expect("compare-programs not initialized");
    match TLS_INDEX.with(|c| c.get()) {
        0 => a.as_str(),
        _ => b.as_str(),
    }
}

pub fn current_program_label() -> &'static str {
    let (a, b) = PROGRAM_LABELS
        .get()
        .expect("compare-programs not initialized");
    match TLS_INDEX.with(|c| c.get()) {
        0 => a.as_str(),
        _ => b.as_str(),
    }
}

pub fn counterpart_program_label() -> &'static str {
    let (a, b) = PROGRAM_LABELS
        .get()
        .expect("compare-programs not initialized");
    match TLS_INDEX.with(|c| c.get()) {
        0 => b.as_str(),
        _ => a.as_str(),
    }
}

pub fn seed() -> u64 {
    *TEST_SEED.get().expect("compare-programs not initialized")
}

fn finalize_comparison(
    instruction_name: &str,
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
                    (test_name.clone(), instruction_name.to_string()),
                    PendingRun {
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
                if let Some(pending) = p
                    .borrow_mut()
                    .remove(&(test_name.clone(), instruction_name.to_string()))
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
                note: None,
                compute_units_a,
                compute_units_b: cu_used,
                byte_equal,
                result_a,
                result_b: result_str.map(|s| s.to_string()),
                accounts_a: snapshot_a,
                accounts_b: account_snapshot,
            };
            emit_report_event(ReportEvent::Comparison { record });
        }
    }
}

pub fn log_cu_and_byte_comparison_ctx(
    ctx: &MolluskContext<std::collections::HashMap<Pubkey, Account>>,
    instruction_name: &str,
    cu_used: Option<u64>,
    result_str: Option<&str>,
) {
    let store = ctx.account_store.borrow();
    let mut account_snapshot: BTreeMap<Pubkey, Account> = BTreeMap::new();
    for (pubkey, account) in store.iter() {
        account_snapshot.insert(*pubkey, account.clone());
    }
    finalize_comparison(instruction_name, cu_used, result_str, account_snapshot);
}

pub fn log_cu_and_byte_comparison_snapshot(
    instruction_name: &str,
    cu_used: Option<u64>,
    result_str: Option<&str>,
    account_snapshot: &BTreeMap<Pubkey, Account>,
) {
    finalize_comparison(
        instruction_name,
        cu_used,
        result_str,
        account_snapshot.clone(),
    );
}

pub fn log_instruction_logs(instruction: &str, logs: &[String]) {
    let test_name = TLS_TEST_NAME.with(|n| n.borrow().clone());
    let label = current_program_label().to_string();
    emit_report_event(ReportEvent::InstructionLogs {
        test: test_name,
        instruction: instruction.to_string(),
        program_label: label,
        logs: logs.to_vec(),
    });
}

pub fn set_current_ctx_ptr(ctx: &MolluskContext<HashMap<Pubkey, Account>>) {
    TLS_CTX_PTR.with(|c| *c.borrow_mut() = ctx as *const _);
}

pub fn set_current_instruction_name(name: &str) {
    TLS_INSTR_NAME.with(|n| *n.borrow_mut() = name.to_string());
}

pub fn default_observer(
    _ix: &solana_instruction::Instruction,
    res: &mollusk_svm::result::InstructionResult,
    invoke_ctx: &solana_program_runtime::invoke_context::InvokeContext,
) {
    if let Some(lc) = invoke_ctx.get_log_collector() {
        let logs: Vec<String> = {
            let br = lc.borrow();
            br.get_recorded_content().to_vec()
        };
        let name = TLS_INSTR_NAME.with(|n| n.borrow().clone());
        log_instruction_logs(&name, &logs);
    }
    let result_str: &str = if res.program_result.is_ok() {
        "OK"
    } else {
        "ERR"
    };
    let name = TLS_INSTR_NAME.with(|n| n.borrow().clone());
    let ctx_ptr = TLS_CTX_PTR.with(|c| *c.borrow());
    if !ctx_ptr.is_null() {
        let ctx_ref = unsafe { &*ctx_ptr };
        log_cu_and_byte_comparison_ctx(
            ctx_ref,
            &name,
            Some(res.compute_units_consumed),
            Some(result_str),
        );
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

fn ensure_sbf_out_dir() {
    if std::env::var("SBF_OUT_DIR").is_ok() {
        return;
    }
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut path = PathBuf::from(manifest_dir);
        path.push("..");
        path.push("target");
        path.push("deploy");
        std::env::set_var("SBF_OUT_DIR", path);
    }
}
