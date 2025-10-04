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

static PROGRAM_FILENAMES: OnceCell<(String, String)> = OnceCell::new();
static PROGRAM_LABELS: OnceCell<(String, String)> = OnceCell::new();
static CURRENT_INDEX: OnceCell<AtomicUsize> = OnceCell::new();
static TEST_SEED: OnceCell<u64> = OnceCell::new();
thread_local! {
    static TLS_TEST_NAME: RefCell<String> = RefCell::new(String::new());
    static TLS_INDEX: Cell<usize> = Cell::new(0);
    static TLS_PENDING: RefCell<HashMap<(String, String), (Option<u64>, Option<String>, BTreeMap<Pubkey, Account>)>> = RefCell::new(HashMap::new());
}

fn write_missing_and_extra(
    buffer: &mut String,
    accounts_a: &BTreeMap<Pubkey, Account>,
    accounts_b: &BTreeMap<Pubkey, Account>,
) {
    for pubkey in accounts_a.keys().filter(|pubkey| !accounts_b.contains_key(*pubkey)) {
        let _ = writeln!(buffer, "- Missing in B: {}", pubkey);
    }
    for pubkey in accounts_b.keys().filter(|pubkey| !accounts_a.contains_key(*pubkey)) {
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
                diffs.push(format!("lamports: {} -> {}", account_a.lamports, account_b.lamports));
            }
            if account_a.owner != account_b.owner {
                diffs.push(format!("owner: {} -> {}", account_a.owner, account_b.owner));
            }
            if account_a.data != account_b.data {
                if account_a.data.len() != account_b.data.len() {
                    diffs.push(format!("data_len: {} -> {}", account_a.data.len(), account_b.data.len()));
                } else {
                    diffs.push("data: bytes differ".to_string());
                    let mut first_difference_index: Option<usize> = None;
                    let mut total_diffs: usize = 0;
                    for (i, (byte_a, byte_b)) in account_a.data.iter().zip(account_b.data.iter()).enumerate() {
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
                    let _ = writeln!(buffer, "  data_diff: first_at={}, differing_bytes={}", first_index, total_diffs);
                    let _ = writeln!(buffer, "  A: {}", a_hex_window);
                    let _ = writeln!(buffer, "  B: {}", b_hex_window);
                }
            }
            if account_a.executable != account_b.executable {
                diffs.push(format!("executable: {} -> {}", account_a.executable, account_b.executable));
            }
            if account_a.rent_epoch != account_b.rent_epoch {
                diffs.push(format!("rent_epoch: {} -> {}", account_a.rent_epoch, account_b.rent_epoch));
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

static SNAPSHOT_A: OnceCell<BTreeMap<Pubkey, Account>> = OnceCell::new();
static SNAPSHOT_B: OnceCell<BTreeMap<Pubkey, Account>> = OnceCell::new();
static WRITE_LOCK: OnceCell<Mutex<()>> = OnceCell::new();

thread_local! { static TLS_RNG: RefCell<Option<u64>> = RefCell::new(None); }

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

    let test_name = TLS_TEST_NAME.with(|n| n.borrow().clone());
    let idx = TLS_INDEX.with(|c| c.get());

    match idx {
        0 => {
            let _ = SNAPSHOT_A.set(account_snapshot.clone());
            TLS_PENDING.with(|p| {
                p.borrow_mut().insert(
                    (test_name.clone(), instruction_name.to_string()),
                    (cu_used, result_str.map(|s| s.to_string()), account_snapshot),
                );
            });
        }
        _ => {
            let _ = SNAPSHOT_B.set(account_snapshot.clone());
            let (mut compute_units_a, mut result_a, mut snapshot_a) = (None, None, BTreeMap::new());
            TLS_PENDING.with(|p| {
                if let Some((a_cu, a_res, a_snap)) = p
                    .borrow_mut()
                    .remove(&(test_name.clone(), instruction_name.to_string()))
                {
                    compute_units_a = a_cu;
                    result_a = a_res;
                    snapshot_a = a_snap;
                }
            });
            let byte_equal = if !snapshot_a.is_empty() {
                Some(snapshot_a == account_snapshot)
            } else {
                None
            };
            append_row(
                &test_name,
                instruction_name,
                None,
                compute_units_a,
                cu_used,
                byte_equal,
                result_a.as_deref(),
                result_str,
            );
            write_full_details(
                &test_name,
                instruction_name,
                compute_units_a,
                cu_used,
                byte_equal,
                result_a.as_deref(),
                result_str,
                &snapshot_a,
                &account_snapshot,
            );
        }
    }
}

fn append_row(
    test: &str,
    instruction: &str,
    note: Option<&str>,
    compute_units_a: Option<u64>,
    compute_units_b: Option<u64>,
    byte_equal: Option<bool>,
    result_a: Option<&str>,
    result_b: Option<&str>,
) {
    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    path.push("report.csv");

    let _guard = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
    if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
        let header_needed = fs::metadata(&path).map(|m| m.len() == 0).unwrap_or(true);
        if header_needed {
            let (a, b) = PROGRAM_LABELS
                .get()
                .cloned()
                .unwrap_or_else(|| ("A".to_string(), "B".to_string()));
            let _ = f.write_all(
                format!("test,instruction,note,{a} CUs,{b} CUs,ByteEqual,{a} Result,{b} Result\n")
                    .as_bytes(),
            );
        }

        let byte_equality_label = byte_equal
            .map(|b| if b { "100%" } else { "DIFF" })
            .unwrap_or("");
        let _ = f.write_all(
            format!(
                "{},{},{},{},{},{},{},{}\n",
                test,
                instruction,
                note.unwrap_or_default(),
                compute_units_a.map(|v| v.to_string()).unwrap_or_default(),
                compute_units_b.map(|v| v.to_string()).unwrap_or_default(),
                byte_equality_label,
                result_a.unwrap_or_default(),
                result_b.unwrap_or_default(),
            )
            .as_bytes(),
        );
    }
}

fn write_full_details(
    test: &str,
    instruction: &str,
    compute_units_a: Option<u64>,
    compute_units_b: Option<u64>,
    byte_equal: Option<bool>,
    result_a: Option<&str>,
    result_b: Option<&str>,
    accounts_a: &BTreeMap<Pubkey, Account>,
    accounts_b: &BTreeMap<Pubkey, Account>,
) {
    let mut buffer = String::new();
    let _ = writeln!(buffer, "=== {} :: {} ===", test, instruction);
    let _ = writeln!(
        buffer,
        "CUs: A={}, B={}",
        compute_units_a.map(|v| v.to_string()).unwrap_or_default(),
        compute_units_b.map(|v| v.to_string()).unwrap_or_default()
    );
    let _ = writeln!(
        buffer,
        "ByteEqual: {}",
        byte_equal
            .map(|b| if b { "100%" } else { "DIFF" })
            .unwrap_or("")
    );
    if let Some(result_a_str) = result_a {
        let _ = writeln!(buffer, "Result A: {}", result_a_str);
    }
    if let Some(result_b_str) = result_b {
        let _ = writeln!(buffer, "Result B: {}", result_b_str);
    }

    write_missing_and_extra(&mut buffer, accounts_a, accounts_b);
    write_account_changes(&mut buffer, accounts_a, accounts_b);
    if accounts_a == accounts_b {
        let _ = writeln!(buffer, "No state differences");
    }

    write_accounts_section(&mut buffer, "-- Accounts A --", accounts_a);
    write_accounts_section(&mut buffer, "-- Accounts B --", accounts_b);
    buffer.push_str("\n");

    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    path.push("report_details.txt");
    if let Ok(_g) = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock() {
        if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
            let _ = f.write_all(buffer.as_bytes());
        }
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
    // Default to workspace target/deploy assuming we are in a crate under the workspace root
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut path = PathBuf::from(manifest_dir);
        path.push("..");
        path.push("target");
        path.push("deploy");
        std::env::set_var("SBF_OUT_DIR", path);
    }
}
