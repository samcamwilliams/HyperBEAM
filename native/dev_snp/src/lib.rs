use rustler::{NifMap, NifTuple, NifUnitEnum, NifTaggedEnum, NifUntaggedEnum};
use sev::firmware::guest::*;
use sev::certs::snp::{ca, Chain};
use openssl::x509::X509;
use std::convert::TryInto;
use std::thread;
use std::time::SystemTime;
// See Derive Macros docs at https://docs.rs/rustler/0.26.0/rustler/index.html#derives

/// Custom logger function that mimics the C code's logging behavior
fn beamr_print(log_level: &str, file: &str, line: u32, message: &str) {
    // Get the current thread ID
    let thread_id = thread::current().id();

    // Get the current time for timestamping
    let now = SystemTime::now();
    let timestamp = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    // Print the log message
    println!(
        "[{}#{:?} @ {}:{}] [{}] {}",
        log_level, thread_id, file, line, timestamp, message
    );
}


#[derive(NifMap)]
struct MyMap {
    lhs: i32,
    rhs: i32,
}

#[derive(NifTuple)]
struct MyTuple {
    lhs: i32,
    rhs: i32,
}

#[derive(NifUnitEnum)]
enum UnitEnum {
    FooBar,
    Baz,
}

#[derive(NifTaggedEnum)]
enum TaggedEnum {
    Foo,
    Bar(String),
    Baz{ a: i32, b: i32 },
}

#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
}

#[rustler::nif(name = "add")]
fn add_nif(a: i64, b: i64) -> i64 {
    add(a, b)
}


/// Convert a byte array to a hexadecimal string.
fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{:02x}", byte)).collect()
}

/// Generate an attestation report.
fn generate_attestation_report(unique_data: [u8; 64], vmpl: u32) -> Result<AttestationReport, String> {
    println!("Opening firmware for attestation report generation...");
    let mut fw = Firmware::open().map_err(|e| format!("Failed to open firmware: {:?}", e))?;
    println!("Generating attestation report...");
    let report = fw
        .get_report(None, Some(unique_data), Some(vmpl))
        .map_err(|e| format!("Failed to generate attestation report: {:?}", e))?;

    println!("Attestation report generated successfully.");

    Ok(report)
}

fn calculate_launch_measurment(snp_measure_args: SnpMeasurementArgs) -> Result<[u8; 384 / 8], Whatever> {
    let ld = snp_calc_launch_digest(snp_measure_args)
        .whatever_context("failed to compute launch digest")?;
    let ld_vec = bincode::serialize(&ld)
        .whatever_context("failed to bincode serialize SnpLaunchDigest to Vec<u8>")?;
    let ld_arr: [u8; 384 / 8] = match ld_vec.try_into() {
        Ok(v) => v,
        Err(_) => whatever!("SnpLaunchDigest has unexpected length"),
    };
    Ok(ld_arr)
}

pub fn get_ovmf_hash_from_file(ovmf_file: PathBuf) -> Result<SnpLaunchDigest, MeasurementError> {
    calc_snp_ovmf_hash(ovmf_file)
}

pub fn get_hashes_from_files(kernel_file: PathBuf, initrd_file: Option<PathBuf>, append: Option<&str>) -> Result<SevHashes, MeasurementError> {
    SevHashes::new(kernel_file, initrd_file, append)
}

#[rustler::nif]
fn request_attestation_report(unique_data: rustler::Binary) -> Result<String, String> {
    // Access the raw pointer to the binary data
    let data_ptr = unique_data.as_ptr();
    let data_len = unique_data.len();

    beamr_print(
        "DBG",
        file!(),
        line!(),
        &format!(
            "Received data pointer: {:?}, length: {}",
            data_ptr, data_len
        ),
    );

    // Validate the binary size
    if data_len != 64 {
        return Err(format!(
            "Invalid data length: {}. Expected 64 bytes.",
            data_len
        ));
    }

    // Convert binary to Vec<u8> if needed
    let unique_data_vec = unique_data.as_slice().to_vec();

    beamr_print(
        "DBG",
        file!(),
        line!(),
        &format!("Processed data: {:?}", unique_data_vec),
    );


    if unique_data_vec.len() != 64 {
		beamr_print("ERR", file!(), line!(), "Unique data length is invalid.");
        return Err("Unique data must be exactly 64 bytes".to_string());
    }

    let mut fw = Firmware::open().map_err(|e| {
        let err_msg = format!("Failed to open firmware: {:?}", e);
        err_msg
    })?;

    let report = fw
        .get_report(None, Some(unique_data_vec.try_into().unwrap()), None)
        .map_err(|e| {
            let err_msg = format!("Failed to get attestation report: {:?}", e);
            err_msg
        })?;
    Ok("Attestation report generated.".to_string())
}

rustler::init!(
    "sev_nif",
    [
        request_attestation_report,
    ]
);

fn main() {
    let unique_data = [0u8; 64];

    let ovmf_file = "/home/peter/Workspace/snp-guard/build/snp-release/usr/local/share/qemu/DIRECT_BOOT_OVMF.fd".to_owned();
    
    let sev_hashes = get_hashes_from_files(kernel_file.clone().into(), Some(initrd_file.clone().into()), Some(kernal_cmdline.as_str())).unwrap();

    println!("Kernel Hash: {}", hex::encode(sev_hashes.kernel_hash));
    println!("Initrd Hash: {}", hex::encode(sev_hashes.initrd_hash));
    println!("Cmdline Hash: {}", hex::encode(sev_hashes.cmdline_hash));


    let ovmf_hash = Some("b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510");
    let kernel_hash = Some(hex::decode("69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576").unwrap().try_into().unwrap());
    let initrd_hash = Some(hex::decode("ca3bf9f3c453471fb98beadb9ca735dd77e8f9879bb33177696b349114ff0737").unwrap().try_into().unwrap());
    let cmdline_hash = Some(hex::decode("f8532adfd035eefb418ff317c0095b0cc4bfd9d622aeadee98e04a134fa0edd9").unwrap().try_into().unwrap());

    let myovmfhash = get_ovmf_hash_from_file(ovmf_file.clone().into()).unwrap();
    println!("OVMF Hash: {:x?}", myovmfhash.get_hex_ld());

    let arguments = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,
        
        vcpus: 1,
        vcpu_type: CpuType::EpycV4,
        vmm_type: Some(VMMType::QEMU),
        guest_features: GuestFeatures(0x1),

        ovmf_hash_str: ovmf_hash,
        kernel_hash,
        initrd_hash,
        append_hash: cmdline_hash,
    };


    let expected_hash = calculate_launch_measurment(arguments).unwrap();


    println!("Expected Launch Hash: {}", bytes_to_hex(&expected_hash));

    println!("Starting attestation process...");
    match generate_attestation_report(unique_data, 0) {
        Ok(report) => {
            println!("Attestation Report: {:?}", report);
        }
        Err(err) => eprintln!("Failed to generate attestation report: {}", err),
    }
}