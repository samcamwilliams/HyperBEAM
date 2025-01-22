use rustler::{NifMap, NifTuple, NifUnitEnum, NifTaggedEnum, NifUntaggedEnum};
use sev::{
    certs::snp::{ca, Certificate, Chain},
	firmware::guest::*,
    firmware::host::CertType,
	firmware::host::CertTableEntry
};
use openssl::{
    ecdsa::EcdsaSig,
    pkey::{PKey, Public},
    sha::Sha384,
    x509::X509,
};

use std::fs;
use std::convert::TryInto;

const KDS_CERT_SITE: &str = "https://kdsintf.amd.com";
const KDS_VCEK: &str = "/vcek/v1";
const KDS_CERT_CHAIN: &str = "cert_chain";

// Request the certificate chain.
pub fn request_cert_chain(sev_prod_name: &str) -> ca::Chain {
    let url = format!("{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{KDS_CERT_CHAIN}");
    println!("Requesting AMD certificate-chain from: {url}");
    let rsp = reqwest::blocking::get(&url).unwrap();
    let body = rsp.bytes().unwrap().to_vec();
    let chain = X509::stack_from_pem(&body).unwrap();
    ca::Chain::from_pem(&chain[1].to_pem().unwrap(), &chain[0].to_pem().unwrap())
}

// Request the VCEK.
pub fn request_vcek(chip_id: [u8; 64], reported_tcb: TcbVersion) -> Certificate {
    let hw_id = hex::encode(&chip_id);
    let url = format!(
        "{KDS_CERT_SITE}{KDS_VCEK}/{sev_prod_name}/{hw_id}?blSPL={:02}&teeSPL={:02}&snpSPL={:02}&ucodeSPL={:02}",
        reported_tcb.boot_loader, reported_tcb.microcode, reported_tcb.snp, reported_tcb.tee
    );
    println!("Requesting VCEK from: {url}");
    let rsp_bytes = reqwest::blocking::get(&url).unwrap().bytes().unwrap().to_vec();
    Certificate::from_der(&rsp_bytes).unwrap()
}

// Verify the guest Trusted Compute Base (TCB).
fn validate_cert_metadata(cert: &X509Certificate, report: &AttestationReport) -> bool {
    let extensions = cert.extensions_map().unwrap();

    let verify_extension = |oid: Oid, expected: u8| {
        extensions.get(&oid).map_or(false, |ext| ext.value.last() == Some(&expected))
    };

    verify_extension(SnpOid::BootLoader.oid(), report.reported_tcb.boot_loader)
        && verify_extension(SnpOid::Tee.oid(), report.reported_tcb.tee)
        && verify_extension(SnpOid::Snp.oid(), report.reported_tcb.snp)
        && verify_extension(SnpOid::Ucode.oid(), report.reported_tcb.microcode)
}

fn main() {
	// Create and supply 64 bytes of unique data to include in the attestation report.
	let unique_data: [u8; 64] = [
		65, 77, 68, 32, 105, 115, 32, 101, 120, 116, 114, 101, 109, 101, 108, 121, 32, 97, 119, 101, 
		115, 111, 109, 101, 33, 32, 87, 101, 32, 109, 97, 107, 101, 32, 116, 104, 101, 32, 98, 101, 
		115, 116, 32, 67, 80, 85, 115, 33, 32, 65, 77, 68, 32, 82, 111, 99, 107, 115, 33, 33, 33, 33, 
		33, 33,
	];

	// Connect to the firmware and request a SEV-SNP attestation report.
	let mut fw: Firmware = Firmware::open().unrwap(); // Open a connection to the firmware.
	let attestation_report: AttestationReport = fw.get_report(None, Some(unique_data), None);


	// Verify the Root of Trust.
	let ca_chain = request_cert_chain("milan");
	let vcek = request_vcek(chip_id, reported_tcb);
	let cert_chain = Chain { ca: ca_chain, vcek };
	cert_chain.verify().unwrap_or_else(|_| {
		eprintln!("Failed to verify the certificate chain!");
	});


	// Verify individual certificates (optional).
	let ark = cert_chain.ca.ark;
	let ask = cert_chain.ca.ask;
	if ark.verify(&ark).is_ok() {
		println!("The AMD ARK is self-signed.");
		if ark.verify(&ask).is_ok() {
			println!("The AMD ASK was signed by the AMD ARK.");
			if ask.verify(&vcek).is_ok() {
				println!("The VCEK was signed by the AMD ASK.");
			} else {
				eprintln!("The VCEK was NOT signed by the AMD ASK!");
			}
		} else {
			eprintln!("The AMD ASK was NOT signed by the AMD ARK!");
		}
	} else {
		eprintln!("The AMD ARK is NOT self-signed!");
	}


	// 6. Verify the attestation report signature.
	let ar_signature = EcdsaSig::try_from(&attestation_report.signature).unwrap();
	let signed_bytes = &bincode::serialize(&attestation_report).unwrap()[..0x2A0];
	let amd_vcek_pubkey = vcek.public_key().unwrap().ec_key().unwrap();
	let mut hasher = Sha384::new();
	hasher.update(signed_bytes);
	let digest = hasher.finish();

	if ar_signature.verify(digest.as_ref(), amd_vcek_pubkey.as_ref()).is_ok() {
		println!("VCEK signed the Attestation Report!");
	} else {
		eprintln!("VCEK did NOT sign the Attestation Report!");
	}

}





// /// Convert a byte array to a hexadecimal string.
// fn bytes_to_hex(bytes: &[u8]) -> String {
//     bytes.iter().map(|byte| format!("{:02x}", byte)).collect()
// }

// /// Save data to a file for manual inspection.
// fn save_to_file(file_name: &str, data: &[u8]) {
//     println!("Saving data to file: {}", file_name);
//     if let Err(e) = fs::write(file_name, data) {
//         eprintln!("Failed to save {}: {:?}", file_name, e);
//     } else {
//         println!("Saved {} for inspection.", file_name);
//     }
// }

// /// Generate an attestation report.
// fn generate_attestation_report(unique_data: [u8; 64], vmpl: u32) -> Result<(AttestationReport, Option<Vec<CertTableEntry>>), String> {
//     println!("Opening firmware for attestation report generation...");
//     let mut fw = Firmware::open().map_err(|e| format!("Failed to open firmware: {:?}", e))?;
//     println!("Generating attestation report...");
//     let (report, cert_table) = fw
//         .get_ext_report(None, Some(unique_data), Some(vmpl))
//         .map_err(|e| format!("Failed to generate attestation report: {:?}", e))?;

//     println!("Attestation report generated successfully.");
//     save_to_file("attestation_report.bin", &bincode::serialize(&report).unwrap());

//     if let Some(ref table) = cert_table {
//         for (i, entry) in table.iter().enumerate() {
//             println!("Saving certificate table entry {}...", i);
//             save_to_file(&format!("cert_table_{}.bin", i), entry.data());
//         }
//     } else {
//         println!("No certificate table available in the report.");
//     }

//     Ok((report, cert_table))
// }

// fn verify_cert_with_ark(ark: &X509, ask: &X509) -> Result<bool, String> {
//     println!("Verifying ASK with ARK...");

//     // Extract ARK's public key
//     let ark_pubkey = ark
//         .public_key()
//         .map_err(|_| "Failed to extract ARK public key".to_string())?;
//     println!("ARK Public Key: {:?}", ark_pubkey);

//     // Extract ASK's To-Be-Signed data
//     let ask_tbs = ask
//         .to_der()
//         .map_err(|_| "Failed to serialize ASK to DER".to_string())?;
//     println!("ASK To-Be-Signed Data (DER): {:?}", bytes_to_hex(&ask_tbs));

//     // Extract ASK's signature
//     let ask_signature = ask.signature().as_slice();
//     println!("ASK Signature: {:?}", bytes_to_hex(ask_signature));

//     // Create verifier for the ARK public key
//     let mut verifier = Verifier::new(MessageDigest::sha384(), &ark_pubkey)
//         .map_err(|_| "Failed to create verifier for ARK public key".to_string())?;

//     // Configure PSS padding
//     verifier
//         .set_rsa_padding(openssl::rsa::Padding::PKCS1_PSS)
//         .map_err(|_| "Failed to set RSA padding to PSS".to_string())?;
//     verifier
//         .set_rsa_pss_saltlen(openssl::sign::RsaPssSaltlen::custom(48))
//         .map_err(|_| "Failed to set PSS salt length".to_string())?;

//     // Verify the signature
//     verifier
//         .update(&ask_tbs)
//         .map_err(|_| "Failed to update verifier with ASK TBS data".to_string())?;
//     let is_valid = verifier
//         .verify(ask_signature)
//         .map_err(|e| format!("ASK verification failed: {:?}", e))?;

//     if is_valid {
//         println!("ASK successfully verified with ARK.");
//     } else {
//         println!("ASK verification failed using ARK public key.");
//     }

//     Ok(is_valid)
// }


// /// Verify the attestation report and certificate chain.
// fn verify_attestation_report(
//     attestation_report: &AttestationReport,
//     cert_table: Option<&[CertTableEntry]>,
// ) -> Result<bool, String> {
//     println!("Verifying certificate table...");
//     let cert_table = cert_table.ok_or("Certificate table is missing")?;
//     if cert_table.len() < 3 {
//         return Err("Insufficient certificates in the certificate table".to_string());
//     }

//     let ark_cert = &cert_table[0].data();
//     let ask_cert = &cert_table[1].data();
//     let vcek_cert = &cert_table[2].data();

//     println!("Saving certificates for verification...");
//     save_to_file("ark_cert.der", ark_cert);
//     save_to_file("ask_cert.der", ask_cert);
//     save_to_file("vcek_cert.der", vcek_cert);

//     println!("Parsing certificates...");
//     let ark = X509::from_der(ark_cert).map_err(|_| "Failed to parse ARK certificate".to_string())?;
//     let ask = X509::from_der(ask_cert).map_err(|_| "Failed to parse ASK certificate".to_string())?;
//     let vcek = X509::from_der(vcek_cert).map_err(|_| "Failed to parse VCEK certificate".to_string())?;

//     if !verify_cert_with_ark(&ark, &ask)? {
//         return Err("ASK verification failed using ARK public key.".to_string());
//     }

//     println!("Verifying VCEK with ASK public key...");
//     if !vcek.verify(&ask.public_key().unwrap()).unwrap_or(false) {
//         return Err("VCEK verification failed.".to_string());
//     }
//     println!("VCEK successfully verified against ASK.");

//     println!("Verifying attestation report signature...");
//     let report_bytes = bincode::serialize(&attestation_report)
//         .map_err(|e| format!("Failed to serialize attestation report: {:?}", e))?;
//     save_to_file("report_data.bin", &report_bytes);

//     let report_data = &report_bytes[0..0x2A0];
//     let report_signature = EcdsaSig::try_from(&attestation_report.signature)
//         .map_err(|_| "Failed to parse signature from attestation report".to_string())?;
//     let vcek_pubkey = vcek.public_key().unwrap();
//     let vcek_ec_key = vcek_pubkey.ec_key().map_err(|_| "Failed to extract EC key from VCEK".to_string())?;

//     if !report_signature.verify(report_data, &vcek_ec_key).unwrap_or(false) {
//         return Err("Attestation report signature verification failed.".to_string());
//     }

//     println!("Attestation report signature verified successfully.");
//     Ok(true)
// }

// /// Generate a derived key.
// fn generate_derived_key() -> Result<Vec<u8>, String> {
//     let mut fw = Firmware::open().map_err(|e| format!("Failed to open firmware: {:?}", e))?;
//     let request = DerivedKey::new(false, GuestFieldSelect(1), 0, 0, 0);
//     fw.get_derived_key(None, request)
//         .map(|key| key.to_vec())
//         .map_err(|e| format!("Failed to generate derived key: {:?}", e))
// }

// fn main() {
//     let unique_data = [0u8; 64];
//     let vmpl = 1;

//     println!("Starting attestation process...");
//     match generate_attestation_report(unique_data, vmpl) {
//         Ok((report, cert_table)) => {
//             if let Some(ref cert_table) = cert_table {
//                 println!("Certificate Table: {:?}", cert_table);

//                 println!("Generating derived key...");
//                 match generate_derived_key() {
//                     Ok(key) => println!("Derived Key: {}", bytes_to_hex(&key)),
//                     Err(err) => eprintln!("Failed to generate derived key: {}", err),
//                 }

//                 println!("Verifying attestation report...");
//                 match verify_attestation_report(&report, Some(cert_table)) {
//                     Ok(_) => println!("Attestation report verified successfully."),
//                     Err(e) => println!("Verification failed: {}", e),
//                 }
//             } else {
//                 println!("No certificate table provided.");
//             }
//         }
//         Err(err) => eprintln!("Failed to generate attestation report: {}", err),
//     }
// }
