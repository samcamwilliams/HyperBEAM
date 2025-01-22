use sev::firmware::guest::{AttestationReport, Firmware};

pub fn generate_attestation_report(unique_data: [u8; 64], vmpl: u32) -> Result<AttestationReport, String> {
    let mut fw = Firmware::open().map_err(|e| format!("Failed to open firmware: {:?}", e))?;
    fw.get_report(None, Some(unique_data), Some(vmpl))
        .map_err(|e| format!("Failed to generate attestation report: {:?}", e))
}