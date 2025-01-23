use std::path::PathBuf;
use logger::snp_print;
use rustler::types::atom::{self, ok};
use rustler::{Binary, Encoder, Env, MapIterator, NifResult, OwnedBinary, Term};
use serde_json::Value;
use sev::certs::snp::ecdsa::Signature;
use sev::certs::snp::{ca, Certificate, Chain, Verifiable};
use sev::firmware::guest::{AttestationReport, GuestPolicy, PlatformInfo};
use sev::firmware::host::TcbVersion;
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
    snp_print(
        "INFO",
        file!(),
        line!(),
        "Attestation report generated successfully.",
    );

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
            _ => snp_print(
                "WARN",
                file!(),
                line!(),
                &format!("Unexpected key: {}", key_str),
            ),
        }
    }

    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Parsed args: {:?}", args),
    );

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
    snp_print(
        "INFO",
        file!(),
        line!(),
        "Launch digest calculated successfully.",
    );

    // Return the digest as the result
    Ok((atom::ok(), digest).encode(env))
}

/// Verifies the measurement in the attestation report against an expected measurement
#[rustler::nif]
fn verify_measurement<'a>(
    env: Env<'a>,
    _report: Binary,
    _expected_measurement: Binary,
) -> NifResult<Term<'a>> {
    use serde::Deserialize;

    #[derive(Debug, Deserialize)]
    struct AttestationReport {
        measurement: Vec<u8>,
        // Add other fields here if needed
    }

    snp_print("INFO", file!(), line!(), "Verifying measurement...");

    // Deserialize the JSON report
    let report: AttestationReport =
        serde_json::from_slice(_report.as_slice()).expect("Failed to deserialize report.");
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Parsed report: {:?}", report),
    );

    // Extract the measurement from the report
    let actual_measurement = &report.measurement;
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Actual measurement: {:?}", actual_measurement),
    );

    // Decode the expected measurement
    let expected_measurement_vec = _expected_measurement.as_slice().to_vec();
    let expected_measurement: Vec<u8> = expected_measurement_vec;

    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Expected measurement: {:?}", expected_measurement),
    );

    // Compare the actual and expected measurements
    if actual_measurement == &expected_measurement {
        snp_print("INFO", file!(), line!(), "Measurements match.");
        Ok((atom::ok(), "Measurements match").encode(env))
    } else {
        snp_print("ERROR", file!(), line!(), "Measurements do not match.");
        Ok((atom::error(), "Measurements do not match").encode(env))
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn verify_signature<'a>(env: Env<'a>, report: Binary<'a>) -> NifResult<Term<'a>> {
    use tokio::runtime::Runtime;

    let result = tokio::task::block_in_place(|| {
        let runtime = Runtime::new().expect("Failed to create Tokio runtime");
        runtime.block_on(async { verify_signature_async(env, report).await })
    });

    match result {
        Ok(term) => Ok(term),
        Err(err) => Ok((atom::error(), format!("Error: {:?}", err)).encode(env)),
    }
}

async fn verify_signature_async<'a>(
    env: Env<'a>,
    report: Binary<'a>,
) -> Result<Term<'a>, Box<dyn std::error::Error + 'a>> {
    
    snp_print("INFO", file!(), line!(), "Verifying signature...");

    // Parse the JSON manually
    let json_data = match serde_json::from_slice::<Value>(report.as_slice()) {
        Ok(data) => data,
        Err(err) => {
            return Ok((
                rustler::types::atom::error(),
                format!("Failed to parse JSON: {}", err),
            )
                .encode(env));
        }
    };

    // Map JSON fields to the AttestationReport struct (TODO: Move to a helper function)
    let attestation_report = AttestationReport {
        version: json_data["version"].as_u64().unwrap_or(0) as u32,
        guest_svn: json_data["guest_svn"].as_u64().unwrap_or(0) as u32,
        policy: GuestPolicy(json_data["policy"].as_u64().unwrap_or(0)),
        family_id: json_data["family_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 16]),
        image_id: json_data["image_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 16]),
        vmpl: json_data["vmpl"].as_u64().unwrap_or(0) as u32,
        sig_algo: json_data["sig_algo"].as_u64().unwrap_or(0) as u32,
        current_tcb: TcbVersion {
            bootloader: json_data["current_tcb"]["bootloader"].as_u64().unwrap_or(0) as u8,
            tee: json_data["current_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["current_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["current_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        plat_info: PlatformInfo(json_data["plat_info"].as_u64().unwrap_or(0)),
        _author_key_en: json_data["_author_key_en"].as_u64().unwrap_or(0) as u32,
        _reserved_0: json_data["_reserved_0"].as_u64().unwrap_or(0) as u32,
        report_data: json_data["report_data"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 64]),
        measurement: json_data["measurement"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        host_data: json_data["host_data"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        id_key_digest: json_data["id_key_digest"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        author_key_digest: json_data["author_key_digest"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 48]),
        report_id: json_data["report_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        report_id_ma: json_data["report_id_ma"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 32]),
        reported_tcb: TcbVersion {
            bootloader: json_data["reported_tcb"]["bootloader"]
                .as_u64()
                .unwrap_or(0) as u8,
            tee: json_data["reported_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["reported_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["reported_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        _reserved_1: [0; 24],
        chip_id: json_data["chip_id"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .map(|v| v.as_u64().unwrap_or(0) as u8)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap_or([0; 64]),
        committed_tcb: TcbVersion {
            bootloader: json_data["committed_tcb"]["bootloader"]
                .as_u64()
                .unwrap_or(0) as u8,
            tee: json_data["committed_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["committed_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["committed_tcb"]["microcode"]
                .as_u64()
                .unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        current_build: json_data["current_build"].as_u64().unwrap_or(0) as u8,
        current_minor: json_data["current_minor"].as_u64().unwrap_or(0) as u8,
        current_major: json_data["current_major"].as_u64().unwrap_or(0) as u8,
        _reserved_2: json_data["_reserved_2"].as_u64().unwrap_or(0) as u8,
        committed_build: json_data["committed_build"].as_u64().unwrap_or(0) as u8,
        committed_minor: json_data["committed_minor"].as_u64().unwrap_or(0) as u8,
        committed_major: json_data["committed_major"].as_u64().unwrap_or(0) as u8,
        _reserved_3: json_data["_reserved_3"].as_u64().unwrap_or(0) as u8,
        launch_tcb: TcbVersion {
            bootloader: json_data["launch_tcb"]["bootloader"].as_u64().unwrap_or(0) as u8,
            tee: json_data["launch_tcb"]["tee"].as_u64().unwrap_or(0) as u8,
            snp: json_data["launch_tcb"]["snp"].as_u64().unwrap_or(0) as u8,
            microcode: json_data["launch_tcb"]["microcode"].as_u64().unwrap_or(0) as u8,
            _reserved: [0; 4],
        },
        _reserved_4: [0; 168],
        signature: Signature {
            r: json_data["signature"]["r"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .map(|v| v.as_u64().unwrap_or(0) as u8)
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap_or([0; 72]),
            s: json_data["signature"]["s"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .map(|v| v.as_u64().unwrap_or(0) as u8)
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap_or([0; 72]),
            _reserved: [0; 368],
        },
    };

    // Get the chip id and TCB version from the report
    let chip_id_array: [u8; 64] = attestation_report
        .chip_id
        .clone()
        .try_into()
        .expect("chip_id should be 64 bytes");

    let tcb_version = attestation_report.current_tcb;

    let ca = request_cert_chain("Milan").await.unwrap();
    let vcek = request_vcek(chip_id_array, tcb_version).await.unwrap();

    // Verify the ca chain
    let ca_verify = ca.verify();
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("CA Verification successful: {:?}", ca_verify),
    );

    // Create the certificate chain
    let cert_chain = Chain { ca, vek: vcek };

    // Verify the certificate chain
    if let Err(e) = cert_chain.verify() {
        snp_print(
            "ERROR",
            file!(),
            line!(),
            &format!("Verification failed: {:?}", e),
        );
        return Ok((atom::error(), format!("Verification failed: {:?}", e)).encode(env));
    }

    // Verify the attestation report
    let verified = (&cert_chain, &attestation_report).verify();
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Verification successful: {:?}", verified),
    );

    Ok((atom::ok(), "Verification successful").encode(env))
}


// HELPERS (TODO: Move to seperate file)
const KDS_CERT_SITE: &str = "https://kdsintf.amd.com";
const KDS_VCEK: &str = "/vcek/v1";
const KDS_CERT_CHAIN: &str = "cert_chain";

/// Requests the AMD certificate chain (ASK + ARK) for the given SEV product name.
pub async fn request_cert_chain(
    sev_prod_name: &str,
) -> Result<ca::Chain, Box<dyn std::error::Error>> {
    let url = format!("{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{KDS_CERT_CHAIN}");
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Requesting AMD certificate chain from: {url}"),
    );

    let rsp = reqwest::get(&url).await?;
    let body = rsp.bytes().await?;

    let chain = openssl::x509::X509::stack_from_pem(&body)?;
    if chain.len() < 2 {
        return Err("Expected at least two certificates (ARK and ASK) in the chain".into());
    }

    // Convert ARK and ASK into the required `ca::Chain` structure.
    let ark = chain[1].to_pem()?;
    let ask = chain[0].to_pem()?;
    let ca_chain = ca::Chain::from_pem(&ark, &ask)?;

    snp_print(
        "INFO",
        file!(),
        line!(),
        "Successfully fetched AMD certificate chain.",
    );
    Ok(ca_chain)
}

/// Requests the VCEK for the given chip ID and reported TCB.
pub async fn request_vcek(
    chip_id: [u8; 64],
    reported_tcb: TcbVersion,
) -> Result<Certificate, Box<dyn std::error::Error>> {
    let hw_id = chip_id
        .iter()
        .map(|byte| format!("{:02x}", byte))
        .collect::<String>();

    let url = format!(
        "{KDS_CERT_SITE}{KDS_VCEK}/Milan/{hw_id}?blSPL={:02}&teeSPL={:02}&snpSPL={:02}&ucodeSPL={:02}",
        reported_tcb.bootloader, reported_tcb.tee, reported_tcb.snp, reported_tcb.microcode
    );
    snp_print(
        "INFO",
        file!(),
        line!(),
        &format!("Requesting VCEK from: {url}"),
    );

    let rsp = reqwest::get(&url).await?;
    let rsp_bytes = rsp.bytes().await?;

    // Parse the VCEK into a `Certificate`.
    let vcek_cert = Certificate::from_der(&rsp_bytes)?;
    snp_print("INFO", file!(), line!(), "Successfully fetched VCEK.");
    Ok(vcek_cert)
}

// Initialize the NIF module
rustler::init!("dev_snp_nif");



// OLD CODE
// let sig = EcdsaSig::try_from(&attestation_report.signature)?;
// let measurable_bytes: &[u8] = &bincode::serialize(&attestation_report).map_err(|e| {
//     Error::new(
//         ErrorKind::Other,
//         format!("Unable to serialize bytes: {}", e),
//     )
// })?[..0x2a0];
// //print measurable bytes
// snp_print(
//     "INFO",
//     file!(),
//     line!(),
//     &format!("Measurable bytes: {:?}", measurable_bytes),
// );

// let mut hasher = Sha384::new();
// hasher.update(measurable_bytes);
// let base_digest = hasher.finish();

// //print base digest
// snp_print(
//     "INFO",
//     file!(),
//     line!(),
//     &format!("Base digest: {:?}", base_digest),
// );

// let ec = cert_chain.vek.public_key()?.ec_key()?;

// // print ec key
// snp_print("INFO", file!(), line!(), &format!("EC key: {:?}", ec));

// let signed = sig.verify(&base_digest, &ec)?;

// snp_print(
//     "INFO",
//     file!(),
//     line!(),
//     &format!("Signature verified: {:?}", signed),
// );