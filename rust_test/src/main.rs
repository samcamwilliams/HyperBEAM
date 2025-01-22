use sev::{
    error::MeasurementError, firmware::guest::*, measurement::{sev_hashes::SevHashes, snp::{calc_snp_ovmf_hash, SnpLaunchDigest}}
};

use sev::measurement::{
    snp::{snp_calc_launch_digest, SnpMeasurementArgs},
    vmsa::{GuestFeatures, VMMType},
    vcpu_types::CpuType
};

use std::path::PathBuf;
use snafu::{whatever, ResultExt, Whatever};

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

fn main() {
    let unique_data = [0u8; 64];

    let ovmf_file = "/home/peter/Workspace/snp-guard/build/snp-release/usr/local/share/qemu/DIRECT_BOOT_OVMF.fd".to_owned();
    let kernel_file = "/home/peter/Workspace/snp-guard/build/kernel/boot/vmlinuz-6.9.0-snp-guest-a38297e3fb01".to_owned();
    let initrd_file = "/home/peter/Workspace/snp-guard/build/initramfs.cpio.gz".to_owned();
    let kernal_cmdline = "console=ttyS0 earlyprintk=serial root=/dev/sda boot=verity verity_disk=/dev/sdb verity_roothash=4c619a3ba1b9821389a902ebf2fc318d839ca4af144c02a39cf257b03375d324".to_owned();
    
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

                // Display the report.
                println!("Hello");
                println!("Attestation Report: {:?}", report);

                                // Get first certificate from the certificate table.
                //let ark = Certificate::from_der(cert_table[0].data.as_slice()).unwrap();
                                //let vcek = Certificate::from_der(cert_table[1].data.as_slice()).unwrap();
                                //let ask = Certificate::from_der(cert_table[2].data.as_slice()).unwrap();

                /*let cert_chain = Chain {
                    ca: ca::Chain {
                        ark: ark,
                        ask: ask,
                    },
                    vek: vcek,
                };

                                // Verify the certificate chain.
                                match cert_chain.verify() {
                                        Ok(_) => println!("Certificate chain verified successfully."),
                                        Err(err) => eprintln!("Failed to verify certificate chain: {}", err),
                                }*/



        }
        Err(err) => eprintln!("Failed to generate attestation report: {}", err),
    }
}


                // println!("Certificate Table: {:?}", cert_table);

                                // The cert table has the following entries:
                                // 1. Ark Certificate
                                // 2. Ask Certificate
                                // 3. VCEK Certificate

                                // // Extract the ARK certificate from the certificate table.
                // let ark_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::ARK).unwrap();

                                // // Extract the ASK certificate from the certificate table.
                // let ask_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::ASK).unwrap();

                                // // Extract the VCEK certificate from the certificate table.
                                // let vcek_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::VCEK).unwrap();

                                // // Convert the ark certificate to pem format.
                                // let ark_pem = Certificate::from_bytes(ark_cert.data()).unwrap();

                // println!("Certificate Table: {:?}", cert_table);

				// The cert table has the following entries:
				// 1. Ark Certificate
				// 2. Ask Certificate
				// 3. VCEK Certificate

				// // Extract the ARK certificate from the certificate table.
                // let ark_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::ARK).unwrap();

				// // Extract the ASK certificate from the certificate table.
                // let ask_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::ASK).unwrap();

				// // Extract the VCEK certificate from the certificate table.
				// let vcek_cert = cert_table.iter().find(|entry| entry.cert_type == CertType::VCEK).unwrap();				

				// // Convert the ark certificate to pem format.
				// let ark_pem = Certificate::from_bytes(ark_cert.data()).unwrap();