use std::thread;
use std::time::SystemTime;

pub fn snp_print(log_level: &str, file: &str, line: u32, message: &str) {
    let thread_id = thread::current().id();
    let now = SystemTime::now();
    let timestamp = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    println!(
        "[{}#{:?} @ {}:{}] [{}] {}",
        log_level, thread_id, file, line, timestamp, message
    );
}