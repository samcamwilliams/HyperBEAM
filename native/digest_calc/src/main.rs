// This program calculates the SEV-SNP launch digest, which is used for 
// verifying the integrity of a virtual machine at launch. It computes 
// cryptographic hashes of the kernel, initrd, cmdline, and OVMF files, 
// and generates the corresponding launch digest required for secure attestation 
// in SEV-SNP environments.

use bincode;
use clap::{App, Arg};
use serde::{Deserialize, Serialize};
use sev::error::MeasurementError;
use sev::measurement::sev_hashes::SevHashes;
use sev::measurement::snp::{
    calc_snp_ovmf_hash, snp_calc_launch_digest, SnpLaunchDigest, SnpMeasurementArgs,
};
use sev::measurement::vcpu_types::CpuType;
use sev::measurement::vmsa::{GuestFeatures, VMMType};
use std::fs;
use std::path::PathBuf;

/// Struct to hold the arguments received from the command line.
#[derive(Serialize)]
struct Arguments {
    config: Option<String>,       // Path to the configuration file
    kernel_file: Option<String>,  // Path to the kernel file
    initrd_file: Option<String>,  // Path to the initrd file
    ovmf_file: Option<String>,    // Path to the OVMF file
    cmdline: Option<String>,      // Kernel command line
    vcpus: Option<u32>,           // Number of virtual CPUs
    vcpu_type: Option<String>,    // Type of virtual CPU
    vmm_type: Option<String>,     // Virtual Machine Monitor type
    guest_features: Option<String>, // Guest features as a hex value
}

/// Struct to hold the configuration loaded from a YAML file.
#[derive(Debug, Deserialize, Serialize)]
struct Config {
    kernel_file: String,
    initrd_file: String,
    ovmf_file: String,
    cmdline: String,
    vcpus: Option<u32>,
    vcpu_type: Option<String>,
    vmm_type: Option<String>,
    guest_features: Option<String>,
}

/// Converts a byte slice to a hexadecimal string representation.
fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{:02x}", byte)).collect()
}

/// Calculates the launch measurement digest using the SEV-SNP arguments.
fn calculate_launch_measurment(
    snp_measure_args: SnpMeasurementArgs,
) -> Result<[u8; 384 / 8], String> {
    // Calculate the launch digest
    let ld = snp_calc_launch_digest(snp_measure_args)
        .map_err(|e| format!("Failed to compute launch digest: {:?}", e))?;

    // Serialize the launch digest
    let ld_vec = bincode::serialize(&ld).map_err(|e| {
        format!(
            "Failed to bincode serialize SnpLaunchDigest to Vec<u8>: {:?}",
            e
        )
    })?;

    // Convert the serialized data into a fixed-length byte array
    let ld_arr: [u8; 384 / 8] = ld_vec
        .try_into()
        .map_err(|_| "SnpLaunchDigest has unexpected length".to_string())?;

    Ok(ld_arr)
}

/// Calculates the OVMF file hash.
pub fn get_ovmf_hash_from_file(ovmf_file: PathBuf) -> Result<SnpLaunchDigest, MeasurementError> {
    calc_snp_ovmf_hash(ovmf_file)
}

/// Retrieves the hashes for kernel, initrd, and cmdline files.
pub fn get_hashes_from_files(
    kernel_file: PathBuf,
    initrd_file: Option<PathBuf>,
    append: Option<&str>,
) -> Result<SevHashes, MeasurementError> {
    SevHashes::new(kernel_file, initrd_file, append)
}

fn main() {
    // Starting message
    println!("=== Digest Calculator Starting ===");

    println!("\n=== Getting Command Line Arguments ===");
    // Parse command line arguments using the clap library
    let matches = App::new("SEV SNP Measurement")
        .version("1.0")
        .author("Peter Farber <farberpete@gmail.com>")
        .about(
            "SEV-SNP Launch Digest Calculation\n\n\
            Example commands:\n\
            1. Basic example with default values:\n\
                ./sev_snp_measurement --kernel_file /path/to/kernel --ovmf_file /path/to/ovmf --cmdline \"root=/dev/sda console=ttyS0\"\n\
            2. Specify all arguments, including optional ones:\n\
                ./sev_snp_measurement --kernel_file /path/to/kernel --initrd_file /path/to/initrd --ovmf_file /path/to/ovmf --cmdline \"root=/dev/sda console=ttyS0\" --vcpus 2 --vcpu_type EpycV4 --vmm_type QEMU --guest_features 0x1\n\
            3. Use a different VMM type and guest features:\n\
                ./sev_snp_measurement --kernel_file /path/to/kernel --ovmf_file /path/to/ovmf --cmdline \"root=/dev/sda console=ttyS0\" --vcpus 4 --vcpu_type EpycMilan --vmm_type EC2 --guest_features 0x2\n"
        )
        .arg(Arg::new("config")
        .help("Path to the YAML configuration file")
        .takes_value(true))
        .arg(Arg::new("kernel_file")
            .help("The path to the kernel file (required)")
            .required_unless_present("config")
            .takes_value(true))
        .arg(Arg::new("initrd_file")
            .help("The path to the initrd file (required)")
            .required_unless_present("config")
            .takes_value(true))
        .arg(Arg::new("ovmf_file")
            .help("The path to the OVMF file (required)")
            .required_unless_present("config")
            .takes_value(true))
        .arg(Arg::new("cmdline")
            .help("The kernel command line (required)")
            .required_unless_present("config")
            .takes_value(true))
        .arg(Arg::new("vcpus")
            .help("Number of virtual CPUs (default: 1)")
            .takes_value(true)
            .default_value("1"))
        .arg(Arg::new("vcpu_type")
            .help("The type of virtual CPU (default: EpycV4)\n\
                Available options:\n\
                \"Epyc\" => CpuType::Epyc,\n\
                \"EpycV1\" => CpuType::EpycV1,\n\
                \"EpycV2\" => CpuType::EpycV2,\n\
                \"EpycIBPB\" => CpuType::EpycIBPB,\n\
                \"EpycV3\" => CpuType::EpycV3,\n\
                \"EpycV4\" => CpuType::EpycV4,\n\
                \"EpycRome\" => CpuType::EpycRome,\n\
                \"EpycRomeV1\" => CpuType::EpycRomeV1,\n\
                \"EpycRomeV2\" => CpuType::EpycRomeV2,\n\
                \"EpycRomeV3\" => CpuType::EpycRomeV3,\n\
                \"EpycMilan\" => CpuType::EpycMilan,\n\
                \"EpycMilanV1\" => CpuType::EpycMilanV1,\n\
                \"EpycMilanV2\" => CpuType::EpycMilanV2,\n\
                \"EpycGenoa\" => CpuType::EpycGenoa,\n\
                \"EpycGenoaV1\" => CpuType::EpycGenoaV1")
            .takes_value(true)
            .default_value("EpycV4"))
        .arg(Arg::new("vmm_type")
            .help("The VMM type (default: QEMU)\n\
                Available options:\n\
                \"QEMU\" => Some(VMMType::QEMU),\n\
                \"EC2\" => Some(VMMType::EC2),\n\
                \"KRUN\" => Some(VMMType::KRUN),")
            .takes_value(true)
            .default_value("QEMU"))
        .arg(Arg::new("guest_features")
            .help("Guest features as a hex value (default: 0x1)\n\
                Available features:\n\
                | 0  | SNPActive             |\n\
                | 1  | vTOM                  |\n\
                | 2  | ReflectVC             |\n\
                | 3  | RestrictedInjection   |\n\
                | 4  | AlternateInjection    |\n\
                | 5  | DebugSwap             |\n\
                | 6  | PreventHostIBS        |\n\
                | 7  | BTBIsolation          |\n\
                | 8  | VmplSSS               |\n\
                | 9  | SecureTSC             |\n\
                | 10 | VmgexitParameter      |\n\
                | 11 | Reserved, SBZ         |\n\
                | 12 | IbsVirtualization     |\n\
                | 13 | Reserved, SBZ         |\n\
                | 14 | VmsaRegProt           |\n\
                | 15 | SmtProtection         |\n\
                | 63:16 | Reserved, SBZ       |\n\n\
                Example Usage:\n\
                1. Enable SNPActive (bit 0):\n\
                guest_features 0000000000000001\n")
            .takes_value(true)
            .default_value("0x1"))
        .get_matches();

    // Store the parsed command line arguments
    let args = Arguments {
        config: matches.value_of("config").map(String::from),
        kernel_file: matches.value_of("kernel_file").map(String::from),
        initrd_file: matches.value_of("initrd_file").map(String::from),
        ovmf_file: matches.value_of("ovmf_file").map(String::from),
        cmdline: matches.value_of("cmdline").map(String::from),
        vcpus: matches.value_of("vcpus").map(|v| v.parse().unwrap()),
        vcpu_type: matches.value_of("vcpu_type").map(String::from),
        vmm_type: matches.value_of("vmm_type").map(String::from),
        guest_features: matches.value_of("guest_features").map(String::from),
    };

    // Output arguments in a nicely formatted JSON style
    let formatted_json = serde_json::to_string_pretty(&args).unwrap();
    println!("{}", formatted_json);

    println!("\n=== Parsing Command Line Arguments ===");

    // Check if a config file path is provided, and load the configuration
    let config: Option<Config> = if let Some(config_path) = matches.value_of("config") {
        let config_content = fs::read_to_string(config_path)
            .map_err(|e| format!("Failed to read config file: {:?}", e))
            .unwrap();
        let config: Config = serde_yaml::from_str(&config_content)
            .map_err(|e| format!("Failed to parse config file: {:?}", e))
            .unwrap();
        Some(config)
    } else {
        None
    };

    // If a config file is loaded, print it as formatted JSON
    if let Some(config) = config.as_ref() {
        let formatted_json = serde_json::to_string_pretty(&config).unwrap();
        println!("{}", formatted_json);
    } else {
        println!("No config loaded.");
    }

    // Retrieve the kernel file from either the config or the command line arguments
    let kernel_file = config
        .as_ref()
        .and_then(|c| Some(c.kernel_file.clone()))
        .unwrap_or_else(|| matches.value_of("kernel_file").unwrap().to_owned());

    // Process other command line arguments or config values similarly...
    let initrd_file = config
        .as_ref()
        .and_then(|c| Some(c.initrd_file.clone()))
        .or_else(|| matches.value_of("initrd_file").map(|s| s.to_owned()));

    let ovmf_file = config
        .as_ref()
        .and_then(|c| Some(c.ovmf_file.clone()))
        .unwrap_or_else(|| matches.value_of("ovmf_file").unwrap().to_owned());

    let cmdline = config
        .as_ref()
        .and_then(|c| Some(c.cmdline.clone()))
        .unwrap_or_else(|| matches.value_of("cmdline").unwrap().to_owned());

    let vcpus: u32 = config
        .as_ref()
        .and_then(|c| c.vcpus)
        .unwrap_or_else(|| matches.value_of("vcpus").unwrap().parse().unwrap());

    let vcpu_type = config
        .as_ref()
        .and_then(|c| c.vcpu_type.clone())
        .unwrap_or_else(|| matches.value_of("vcpu_type").unwrap().to_owned());

    // Process virtual CPU type
    let vcpu_type = match vcpu_type.as_str() {
        "Epyc" => CpuType::Epyc,
        "EpycV1" => CpuType::EpycV1,
        "EpycV2" => CpuType::EpycV2,
        "EpycIBPB" => CpuType::EpycIBPB,
        "EpycV3" => CpuType::EpycV3,
        "EpycV4" => CpuType::EpycV4,
        "EpycRome" => CpuType::EpycRome,
        "EpycRomeV1" => CpuType::EpycRomeV1,
        "EpycRomeV2" => CpuType::EpycRomeV2,
        "EpycRomeV3" => CpuType::EpycRomeV3,
        "EpycMilan" => CpuType::EpycMilan,
        "EpycMilanV1" => CpuType::EpycMilanV1,
        "EpycMilanV2" => CpuType::EpycMilanV2,
        "EpycGenoa" => CpuType::EpycGenoa,
        "EpycGenoaV1" => CpuType::EpycGenoaV1,
        _ => CpuType::EpycV4, // Default to EpycV4
    };

    let vmm_type = config
        .as_ref()
        .and_then(|c| c.vmm_type.clone())
        .unwrap_or_else(|| matches.value_of("vmm_type").unwrap().to_owned());

    // Resolve the VMM type
    let vmm_type = match vmm_type.as_str() {
        "QEMU" => Some(VMMType::QEMU),
        "EC2" => Some(VMMType::EC2),
        "KRUN" => Some(VMMType::KRUN),
        _ => Some(VMMType::QEMU), // Default to QEMU
    };

    let guest_features_string = config
        .as_ref()
        .and_then(|c| c.guest_features.clone())
        .unwrap_or_else(|| matches.value_of("guest_features").unwrap().to_owned());

    let guest_features: u64 =
    u64::from_str_radix(&guest_features_string, 2).unwrap();


    // Step 1: Get the hash of the OVMF file
    let ovmf_hash = get_ovmf_hash_from_file(ovmf_file.clone().into()).unwrap();
    let ovmf_bytes: Vec<u8> = bincode::serialize(&ovmf_hash).unwrap();
    let ovmf_binding = ovmf_hash.get_hex_ld();
    
    println!("\n===== OVFM =====");
    println!("Bytes: {:x?}", ovmf_bytes);
    println!("Hash: {:x?}", ovmf_binding);

    // Step 2: Get the hash of the kernel, initrd, and cmdline
    let SevHashes {
        kernel_hash,
        initrd_hash,
        cmdline_hash,
    } = get_hashes_from_files(
        kernel_file.clone().into(),
        initrd_file.clone().map(|file| file.into()),
        Some(cmdline.as_str()),
    )
    .unwrap();

    println!("\n===== Kernel =====");
    println!("Bytes: {:x?}", kernel_hash);
    println!("Hash: {}", bytes_to_hex(&kernel_hash));

    println!("\n===== Initrd =====");
    println!("Bytes {:x?}", initrd_hash);
    println!("Hash: {}", bytes_to_hex(&initrd_hash));


    println!("\n===== Cmdline =====");
    println!("Bytes: {:x?}", cmdline_hash);
    println!("Hash: {}", bytes_to_hex(&cmdline_hash));

    // Step 3: Calculate the launch digest
    let arguments = SnpMeasurementArgs {
        ovmf_file: Some(PathBuf::from(ovmf_file)),
        kernel_file: None,
        initrd_file: None,
        append: None,

        vcpus,
        vcpu_type,
        vmm_type,
        guest_features: GuestFeatures(guest_features),

        ovmf_hash_str: Some(ovmf_binding.as_str()),
        kernel_hash: Some(kernel_hash),
        initrd_hash: Some(initrd_hash),
        append_hash: Some(cmdline_hash),
    };

    let expected_hash = calculate_launch_measurment(arguments).unwrap();

    println!("\n===== Expected =====");
    println!("Bytes: {:x?}", expected_hash);
    println!("Hash: {}", bytes_to_hex(&expected_hash));
}
