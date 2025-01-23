use sev::measurement::snp::{snp_calc_launch_digest, SnpMeasurementArgs};
use snafu::{ResultExt, Whatever};

pub fn calculate_launch_measurment(args: SnpMeasurementArgs) -> Result<Vec<u8>, Whatever> {
    let digest = snp_calc_launch_digest(args)
        .whatever_context("Failed to compute launch digest")?;
    let serialized = bincode::serialize(&digest)
        .whatever_context("Failed to serialize SnpLaunchDigest")?;
    Ok(serialized)
}