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
    let mut bm: BTreeMap<Pubkey, Account> = BTreeMap::new();
    for (k, v) in store.iter() {
        bm.insert(*k, v.clone());
    }

    let test_name = TLS_TEST_NAME.with(|n| n.borrow().clone());
    let idx = TLS_INDEX.with(|c| c.get());

    match idx {
        0 => {
            let _ = SNAPSHOT_A.set(bm.clone());
            TLS_PENDING.with(|p| {
                p.borrow_mut().insert(
                    (test_name.clone(), instruction_name.to_string()),
                    (cu_used, result_str.map(|s| s.to_string()), bm),
                );
            });
        }
        _ => {
            let _ = SNAPSHOT_B.set(bm.clone());
            let (mut cu_a, mut res_a, mut snap_a) = (None, None, BTreeMap::new());
            TLS_PENDING.with(|p| {
                if let Some((a_cu, a_res, a_snap)) = p
                    .borrow_mut()
                    .remove(&(test_name.clone(), instruction_name.to_string()))
                {
                    cu_a = a_cu;
                    res_a = a_res;
                    snap_a = a_snap;
                }
            });
            let byte_equal = if !snap_a.is_empty() {
                Some(snap_a == bm)
            } else {
                None
            };
            append_row(
                &test_name,
                instruction_name,
                None,
                cu_a,
                cu_used,
                byte_equal,
                res_a.as_deref(),
                result_str,
            );
            write_full_details(
                &test_name,
                instruction_name,
                cu_a,
                cu_used,
                byte_equal,
                res_a.as_deref(),
                result_str,
                &snap_a,
                &bm,
            );
        }
    }
}

fn append_row(
    test: &str,
    instruction: &str,
    note: Option<&str>,
    cu_a: Option<u64>,
    cu_b: Option<u64>,
    byte_equal: Option<bool>,
    res_a: Option<&str>,
    res_b: Option<&str>,
) {
    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    path.push("report.csv");

    let row = {
        let mut buf = String::new();
        // ensure header once under lock
        let _guard = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
        let header_needed = match fs::metadata(&path) {
            Ok(meta) => meta.len() == 0,
            Err(_) => true,
        };
        if header_needed {
            if let Some((a, b)) = PROGRAM_LABELS.get().cloned() {
                buf.push_str(&format!(
                    "test,instruction,note,{a} CUs,{b} CUs,ByteEqual,{a} Result,{b} Result\n"
                ));
            } else {
                buf.push_str("test,instruction,note,A CUs,B CUs,ByteEqual,A Result,B Result\n");
            }
        }
        buf.push_str(&format!(
            "{test},{instruction},{note},{cu_a},{cu_b},{byte},{ra},{rb}\n",
            test = test,
            instruction = instruction,
            note = note.unwrap_or_default(),
            cu_a = cu_a.map(|v| v.to_string()).unwrap_or_default(),
            cu_b = cu_b.map(|v| v.to_string()).unwrap_or_default(),
            byte = byte_equal
                .map(|b| if b {
                    "100%".to_string()
                } else {
                    "DIFF".to_string()
                })
                .unwrap_or_default(),
            ra = res_a.unwrap_or_default(),
            rb = res_b.unwrap_or_default(),
        ));
        buf
    };
    if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
        let _ = f.write_all(row.as_bytes());
    }
}

fn write_full_details(
    test: &str,
    instruction: &str,
    cu_a: Option<u64>,
    cu_b: Option<u64>,
    byte_equal: Option<bool>,
    res_a: Option<&str>,
    res_b: Option<&str>,
    a: &BTreeMap<Pubkey, Account>,
    b: &BTreeMap<Pubkey, Account>,
) {
    let mut buf = String::new();
    buf.push_str(&format!("=== {test} :: {instruction} ===\n"));
    buf.push_str(&format!(
        "CUs: A={}, B={}\n",
        cu_a.map(|v| v.to_string()).unwrap_or_default(),
        cu_b.map(|v| v.to_string()).unwrap_or_default(),
    ));
    buf.push_str(&format!(
        "ByteEqual: {}\n",
        byte_equal
            .map(|b| if b { "100%" } else { "DIFF" })
            .unwrap_or("")
    ));
    if let Some(ra) = res_a {
        buf.push_str(&format!("Result A: {}\n", ra));
    }
    if let Some(rb) = res_b {
        buf.push_str(&format!("Result B: {}\n", rb));
    }

    for (k, _) in a.iter() {
        if !b.contains_key(k) {
            buf.push_str(&format!("- Missing in B: {k}\n"));
        }
    }
    for (k, _) in b.iter() {
        if !a.contains_key(k) {
            buf.push_str(&format!("+ Extra in B: {k}\n"));
        }
    }
    for (k, a_acc) in a.iter() {
        if let Some(b_acc) = b.get(k) {
            let mut diffs = Vec::new();
            if a_acc.lamports != b_acc.lamports {
                diffs.push(format!(
                    "lamports: {} -> {}",
                    a_acc.lamports, b_acc.lamports
                ));
            }
            if a_acc.owner != b_acc.owner {
                diffs.push(format!("owner: {} -> {}", a_acc.owner, b_acc.owner));
            }
            if a_acc.data != b_acc.data {
                if a_acc.data.len() != b_acc.data.len() {
                    diffs.push(format!(
                        "data_len: {} -> {}",
                        a_acc.data.len(),
                        b_acc.data.len()
                    ));
                } else {
                    diffs.push("data: bytes differ".to_string());
                    // Provide a concise diff window
                    let first_diff = a_acc
                        .data
                        .iter()
                        .zip(b_acc.data.iter())
                        .position(|(x, y)| x != y)
                        .unwrap_or(0);
                    let total_diffs = a_acc
                        .data
                        .iter()
                        .zip(b_acc.data.iter())
                        .filter(|(x, y)| x != y)
                        .count();
                    let start = first_diff.saturating_sub(16);
                    let end = (start + 64).min(a_acc.data.len());
                    let aw = hex_window(&a_acc.data, start, end);
                    let bw = hex_window(&b_acc.data, start, end);
                    let _ = writeln!(
                        buf,
                        "  data_diff: first_at={}, differing_bytes={}",
                        first_diff, total_diffs
                    );
                    let _ = writeln!(buf, "  A: {}", aw);
                    let _ = writeln!(buf, "  B: {}", bw);
                }
            }
            if a_acc.executable != b_acc.executable {
                diffs.push(format!(
                    "executable: {} -> {}",
                    a_acc.executable, b_acc.executable
                ));
            }
            if a_acc.rent_epoch != b_acc.rent_epoch {
                diffs.push(format!(
                    "rent_epoch: {} -> {}",
                    a_acc.rent_epoch, b_acc.rent_epoch
                ));
            }
            if !diffs.is_empty() {
                buf.push_str(&format!("* Changed {k}: {}\n", diffs.join(", ")));
            }
        }
    }
    if a == b {
        buf.push_str("No state differences\n");
    }

    buf.push_str("-- Accounts A --\n");
    for (k, acc) in a.iter() {
        buf.push_str(&format!(
            "{} owner={} lamports={} data_len={}\n",
            k,
            acc.owner,
            acc.lamports,
            acc.data.len()
        ));
    }
    buf.push_str("-- Accounts B --\n");
    for (k, acc) in b.iter() {
        buf.push_str(&format!(
            "{} owner={} lamports={} data_len={}\n",
            k,
            acc.owner,
            acc.lamports,
            acc.data.len()
        ));
    }
    buf.push_str("\n");

    let mut path = PathBuf::from("target/compare-programs");
    if fs::create_dir_all(&path).is_err() {
        return;
    }
    path.push("report_details.txt");
    if let Ok(_g) = WRITE_LOCK.get_or_init(|| Mutex::new(())).lock() {
        if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
            let _ = f.write_all(buf.as_bytes());
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
