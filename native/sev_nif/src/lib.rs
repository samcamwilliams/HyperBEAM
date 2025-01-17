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

fn add(a: i64, b: i64) -> i64 {
	beamr_print("INFO", file!(), line!(), "add_nif called.");
    a + b
}

#[rustler::nif(name = "my_map")]
fn my_map_nif() -> MyMap {
    my_map()
}

#[rustler::nif]
fn my_maps() -> Vec<MyMap> {
    vec![ my_map(), my_map()]
}

fn my_map() -> MyMap {
    MyMap { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn my_tuple() -> MyTuple {
    MyTuple { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
    unit_enum
}

#[rustler::nif]
fn tagged_enum_echo(tagged_enum: TaggedEnum) -> TaggedEnum {
    tagged_enum
}

#[rustler::nif]
fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
    untagged_enum
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


// New function: Verify root of trust
#[rustler::nif]
fn verify_root_of_trust(ark_pem: String, ask_pem: String, vcek_pem: String) -> Result<bool, String> {
	let ark = X509::from_pem(ark_pem.as_bytes())
    .map_err(|_| "Failed to parse ARK PEM".to_string())?;
	let ask = X509::from_pem(ask_pem.as_bytes())
		.map_err(|_| "Failed to parse ASK PEM".to_string())?;
	let vcek = X509::from_pem(vcek_pem.as_bytes())
		.map_err(|_| "Failed to parse VCEK PEM".to_string())?;
    if !ark.verify(&ark.public_key().unwrap()).unwrap_or(false) {
        return Err("ARK verification failed".to_string());
    }
    if !ask.verify(&ark.public_key().unwrap()).unwrap_or(false) {
        return Err("ASK verification failed".to_string());
    }
    if !vcek.verify(&ask.public_key().unwrap()).unwrap_or(false) {
        return Err("VCEK verification failed".to_string());
    }

    Ok(true)
}


rustler::init!(
    "sev_nif",
    [
        add_nif,
        my_map_nif,
        my_maps,
        my_tuple,
        unit_enum_echo,
        tagged_enum_echo,
        untagged_enum_echo,
        request_attestation_report,
        verify_root_of_trust
    ]
);


#[cfg(test)]
mod tests {
    use crate::add;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

	#[test]
    fn test_request_attestation_report() {
        let unique_data = vec![0; 64];
        let result = request_attestation_report(unique_data);
        assert!(result.is_ok());
    }

    #[test]
    fn test_verify_root_of_trust() {
        let ark_pem = "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----";
        let ask_pem = "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----";
        let vcek_pem = "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----";

        let result = verify_root_of_trust(ark_pem.to_string(), ask_pem.to_string(), vcek_pem.to_string());
        assert!(result.is_ok());
    }
}
