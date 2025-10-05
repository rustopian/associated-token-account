//! Program comparison framework for Solana programs.
//!
//! Compare multiple versions of a program side-by-side to detect behavioral differences,
//! performance regressions, and state divergences.

use chrono::{Datelike, Timelike, Utc};
pub use compare_programs_macro::compare_programs;
pub use compare_programs_macro::instrument;
use mollusk_svm::MolluskContext;
use once_cell::sync::OnceCell;
use solana_account::Account;
use solana_pubkey::Pubkey;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

mod observer;
mod report;

// Re-export the default observer for macro-generated code
pub use observer::default_observer;

#[derive(Clone)]
pub(crate) struct PendingRun {
    compute_units: Option<u64>,
    result: Option<String>,
    snapshot: BTreeMap<Pubkey, Account>,
}

pub(crate) static PROGRAM_FILENAMES: OnceCell<(String, String)> = OnceCell::new();
static PROGRAM_LABELS: OnceCell<(String, String)> = OnceCell::new();
static CURRENT_INDEX: OnceCell<AtomicUsize> = OnceCell::new();
static TEST_SEED: OnceCell<u64> = OnceCell::new();

thread_local! {
    pub(crate) static TLS_TEST_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    pub(crate) static TLS_INDEX: Cell<usize> = const { Cell::new(0) };
    pub(crate) static TLS_PENDING: RefCell<HashMap<(String, String), PendingRun>> = RefCell::new(HashMap::new());
    pub(crate) static TLS_CTX_PTR: RefCell<*const MolluskContext<HashMap<Pubkey, Account>>> = const { RefCell::new(std::ptr::null()) };
    static TLS_INSTR_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    pub(crate) static TLS_SUITE_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    pub(crate) static TLS_FILTER_PROGRAM_IDS: RefCell<Vec<Pubkey>> = const { RefCell::new(Vec::new()) };
}

thread_local! {
    static TLS_RNG: RefCell<Option<u64>> = const { RefCell::new(None) };
}

/// Generates a session prefix from git commit and timestamp for output files.
pub(crate) fn get_or_make_session_prefix() -> String {
    let dir = PathBuf::from("target/compare-programs");
    let _ = fs::create_dir_all(&dir);
    let mut path = dir.clone();
    path.push("run_prefix.txt");
    if let Ok(bytes) = fs::read(&path) {
        if let Ok(existing) = String::from_utf8(bytes) {
            let trimmed = existing.trim().to_string();
            if !trimmed.is_empty() {
                return trimmed;
            }
        }
    }
    let prefix = current_run_prefix();
    // Try atomic create
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&path)
    {
        use std::io::Write;
        let _ = f.write_all(prefix.as_bytes());
        let _ = f.sync_all();
        return prefix;
    }
    // If someone else created it, read back
    fs::read_to_string(&path)
        .map(|s| s.trim().to_string())
        .unwrap_or(prefix)
}

fn current_run_prefix() -> String {
    let git_hash = std::env::var("GIT_COMMIT")
        .ok()
        .or_else(git_rev_parse_head)
        .unwrap_or_else(|| "no-git".to_string());
    let ts = Utc::now();
    let short = format!(
        "{:04}{:02}{:02}-{:02}{:02}{:02}",
        ts.year(),
        ts.month(),
        ts.day(),
        ts.hour(),
        ts.minute(),
        ts.second()
    );
    format!("{}-{}", git_hash, short)
}

fn git_rev_parse_head() -> Option<String> {
    use std::process::Command;
    if let Ok(output) = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
    {
        if output.status.success() {
            return Some(String::from_utf8_lossy(&output.stdout).trim().to_string());
        }
    }
    None
}

/// Deterministic RNG for generating reproducible pubkeys/keypairs across test runs.
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

pub fn new_unique_address() -> solana_address::Address {
    solana_address::Address::new_from_array(rng_next_bytes32())
}

pub fn new_unique_keypair() -> solana_keypair::Keypair {
    solana_keypair::Keypair::new_from_array(rng_next_bytes32())
}

/// Configures the test run with program filenames, labels, and seed.
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

pub fn set_test_suite(name: &str) {
    let s = std::path::Path::new(name)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or(name)
        .to_string();
    TLS_SUITE_NAME.with(|n| *n.borrow_mut() = s);
}

/// Sets program IDs to filter observations. Empty list = observe all programs.
pub fn set_filter_program_ids(ids: &[Pubkey]) {
    TLS_FILTER_PROGRAM_IDS.with(|c| *c.borrow_mut() = ids.to_vec());
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

pub fn seed() -> u64 {
    *TEST_SEED.get().expect("compare-programs not initialized")
}

pub fn set_current_ctx_ptr(ctx: &MolluskContext<HashMap<Pubkey, Account>>) {
    TLS_CTX_PTR.with(|c| *c.borrow_mut() = ctx as *const _);
}

pub fn set_current_instruction_name(name: &str) {
    TLS_INSTR_NAME.with(|n| *n.borrow_mut() = name.to_string());
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
