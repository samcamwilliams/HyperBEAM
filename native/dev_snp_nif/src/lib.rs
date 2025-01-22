use std::path::PathBuf;

use logger::snp_print;
use rustler::types::atom::{self, ok};
use rustler::{Binary, Encoder, Env, MapIterator, NifResult, OwnedBinary, Term};
use sev::measurement::snp::SnpMeasurementArgs;
use sev::measurement::vcpu_types::CpuType;
use sev::measurement::vmsa::{GuestFeatures, VMMType};

pub mod attestation;
pub mod calculation;
pub mod logger;

/// Struct to hold launch digest arguments passed from Erlang
#[derive(Debug)]
struct LaunchDigestArgs {
    vcpus: u32,
    vcpu_type: u8,
    vmm_type: u8,
    guest_features: u64,
    ovmf_hash_str: String,
    kernel_hash: String,
    initrd_hash: String,
    append_hash: String,
}

/// Generates an attestation report from unique data and VMPL value
#[rustler::nif]
fn request_attestation_report<'a>(
    env: Env<'a>,
    unique_data: Binary,
    vmpl: u32,
) -> NifResult<Term<'a>> {
    snp_print("INFO", file!(), line!(), "Requesting attestation report...");

    // Convert the binary input to a fixed-size array
    let unique_data_vec = unique_data.as_slice().to_vec();
    let unique_data_array: [u8; 64] = unique_data_vec
        .try_into()
        .expect("slice with incorrect length");

    // Generate the attestation report
    let report = attestation::generate_attestation_report(unique_data_array, vmpl).unwrap();
    snp_print("INFO", file!(), line!(), "Attestation report generated successfully.");

    // Serialize the report to binary
    let report_bytes = bincode::serialize(&report).unwrap();
    let mut report_binary = OwnedBinary::new(report_bytes.len()).unwrap();
    report_binary.as_mut_slice().copy_from_slice(&report_bytes);

    snp_print("INFO", file!(), line!(), "Attestation report serialized.");
    Ok((ok(), report_binary.release(env)).encode(env))
}

/// Calculates the launch digest based on input arguments provided as an Erlang map
#[rustler::nif]
fn calculate_launch_digest<'a>(env: Env<'a>, input_map: Term<'a>) -> NifResult<Term<'a>> {
    snp_print("INFO", file!(), line!(), "Calculating launch digest...");

    // Ensure the input is a map
    if !input_map.is_map() {
        snp_print("ERROR", file!(), line!(), "Input is not a map.");
        return Err(rustler::Error::BadArg);
    }

    // Helper function to decode string values
    fn decode_string(value: Term) -> NifResult<String> {
        match value.get_type() {
            rustler::TermType::List => {
                let list: Vec<u8> = value.decode()?;
                String::from_utf8(list).map_err(|_| rustler::Error::BadArg)
            }
            _ => value.decode(),
        }
    }

    // Initialize the arguments struct
    let map_iter = MapIterator::new(input_map).unwrap();
    let mut args = LaunchDigestArgs {
        vcpus: 0,
        vcpu_type: 0,
        vmm_type: 0,
        guest_features: 0,
        ovmf_hash_str: String::new(),
        kernel_hash: String::new(),
        initrd_hash: String::new(),
        append_hash: String::new(),
    };

    // Populate arguments from the map
    for (key, value) in map_iter {
        let key_str = key.atom_to_string()?.to_string();
        match key_str.as_str() {
            "vcpus" => args.vcpus = value.decode()?,
            "vcpu_type" => args.vcpu_type = value.decode()?,
            "vmm_type" => args.vmm_type = value.decode()?,
            "guest_features" => args.guest_features = value.decode()?,
            "ovmf_hash_str" => args.ovmf_hash_str = decode_string(value)?,
            "kernel_hash" => args.kernel_hash = decode_string(value)?,
            "initrd_hash" => args.initrd_hash = decode_string(value)?,
            "append_hash" => args.append_hash = decode_string(value)?,
            _ => snp_print("WARN", file!(), line!(), &format!("Unexpected key: {}", key_str)),
        }
    }

    snp_print("INFO", file!(), line!(), &format!("Parsed args: {:?}", args));

    // Prepare arguments for calculation
    let ovmf_file = "./native/dev_snp_nif/OVMF.fd".to_owned();
    let arguments = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,

        vcpus: args.vcpus,
        vcpu_type: CpuType::try_from(args.vcpu_type).unwrap(),
        vmm_type: Some(VMMType::try_from(args.vmm_type).unwrap()),
        guest_features: GuestFeatures(args.guest_features),

        ovmf_hash_str: Some(args.ovmf_hash_str.as_str()),
        kernel_hash: Some(hex::decode(args.kernel_hash).unwrap().try_into().unwrap()),
        initrd_hash: Some(hex::decode(args.initrd_hash).unwrap().try_into().unwrap()),
        append_hash: Some(hex::decode(args.append_hash).unwrap().try_into().unwrap()),
    };

    // Perform the calculation
    let digest = calculation::calculate_launch_measurment(arguments).unwrap();
    snp_print("INFO", file!(), line!(), "Launch digest calculated successfully.");

    // Return the digest as the result
    Ok((atom::ok(), digest).encode(env))
}

// Initialize the NIF module
rustler::init!("dev_snp_nif");
