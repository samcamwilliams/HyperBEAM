# GCP & GoTPM Tools

## Table of Contents

- [GCP \& GoTPM Tools](#gcp--gotpm-tools)
  - [Table of Contents](#table-of-contents)
  - [Variables](#variables)
  - [Create an AMD SEV-SNP Instance](#create-an-amd-sev-snp-instance)
  - [Create an Intel TDX Instance](#create-an-intel-tdx-instance)
  - [Connecting to the Instance](#connecting-to-the-instance)
  - [Useful Commands](#useful-commands)
    - [List Available Machine Types](#list-available-machine-types)
    - [Generate Random Nonces](#generate-random-nonces)
    - [Run `gotpm` Attestation](#run-gotpm-attestation)
  - [Notes](#notes)

## Variables

```sh
export GCI_NAME=PETES-SEV-SNP-TEST-0
export GCI_PROJECT=arweave-437622
export GCI_IMAGE=packer-1730219529 
export GCI_ZONE=us-central1-a
```

## Create an AMD SEV-SNP Instance

```sh
gcloud compute instances create $GCI_NAME \
    --zone=$GCI_ZONE \
    --machine-type=n2d-standard-2 \
    --min-cpu-platform="AMD Milan" \
    --confidential-compute-type=SEV_SNP \
    --maintenance-policy=TERMINATE \
    --image-family=ubuntu-2404-lts-amd64 \
    --image-project=ubuntu-os-cloud \
    --project=$GCI_PROJECT \
    --network-interface=network-tier=PREMIUM,nic-type=GVNIC,stack-type=IPV4_ONLY,subnet=default \
    --tags=http-server,https-server \
    --shielded-secure-boot \
    --shielded-vtpm \
    --shielded-integrity-monitoring \
    --create-disk=auto-delete=yes,boot=yes,device-name=instance-20241030-131350,image=projects/$GCI_PROJECT/global/images/$GCI_IMAGE,mode=rw,size=20,type=pd-balanced
```

## Create an Intel TDX Instance

```sh
gcloud compute instances create $GCI_NAME \
    --zone=$GCI_ZONE \
    --machine-type=c3-standard-4 \
    --confidential-compute-type=TDX \
    --maintenance-policy=TERMINATE \
    --image-family=ubuntu-2204-lts \
    --image-project=ubuntu-os-cloud \
    --project=$GCI_PROJECT \
    --network-interface=network-tier=PREMIUM,nic-type=GVNIC,stack-type=IPV4_ONLY,subnet=default \
    --tags=http-server,https-server \
    --shielded-secure-boot \
    --shielded-vtpm \
    --shielded-integrity-monitoring \
    --create-disk=auto-delete=yes,boot=yes,device-name=instance-20241030-131350,image=projects/$GCI_PROJECT/global/images/$GCI_IMAGE,mode=rw,size=20,type=pd-balanced
```

## Connecting to the Instance

```sh
gcloud compute ssh --zone "$GCI_ZONE" "$GCI_NAME" --project "$GCI_PROJECT"
```

## Useful Commands

### List Available Machine Types

```sh
gcloud compute machine-types list --zones=$GCI_ZONE
```

### Generate Random Nonces

- **32-byte Nonce** (for `--nonce`):

  ```sh
  head -c 32 /dev/urandom | xxd -p -c 64
  ```

- **64-byte TEE Nonce** (for `--tee-nonce`):

  ```sh
  head -c 64 /dev/urandom | xxd -p -c 128
  ```

### Run `gotpm` Attestation

```sh
sudo gotpm attest --key AK --nonce <32 bytes (64 hex characters)> --tee-nonce <64 bytes (128 hex characters)> --tee-technology <sev-snp/tdx>
```

## Notes

> [!NOTE]
> The requirement to include both `--nonce` and `--tee-nonce` for the `gotpm attest` command, even when `--tee-technology` (e.g., `sev-snp` or `tdx`) is specified, indicates that **both TPM and TEE layers** of attestation are being validated in this command. Here’s why this is the case:
>
> ### Dual-Layer Attestation in Confidential VMs
>
> **TPM Attestation Layer**:
>
> - The **`--nonce`** parameter is required for the **TPM (Trusted Platform Module) attestation**. It acts as a freshness mechanism for the TPM-based portion of the attestation, preventing replay attacks by ensuring the response is unique to each request.
> - Even when the TEE technology is specified, `gotpm` still performs TPM-based attestation, which includes the TPM nonce (`--nonce`) in the attestation report.
>
> **TEE-Specific Attestation Layer**:
>
> - The **`--tee-nonce`** parameter is required for the **TEE (Trusted Execution Environment) attestation**. This layer provides hardware-backed isolation (e.g., Intel TDX or AMD SEV-SNP), and the larger 64-byte nonce uniquely identifies the TEE attestation report.
> - The `--tee-technology` option (e.g., `sev-snp` or `tdx`) specifies which TEE environment to use, and `--tee-nonce` is essential for proving the freshness of the TEE report in that specific environment.
>
> ### Why Both Nonces Are Required Together
>
> Since the command produces an attestation report containing **both TPM and TEE layers**, each layer requires its own nonce:
>
> - **`--nonce` (TPM layer)**: Required for the general TPM quote in the attestation.
> - **`--tee-nonce` (TEE layer)**: Required specifically for the TEE report tied to `sev-snp` or `tdx`.
>
> This design ensures that the attestation report is comprehensive and includes **freshness proofs** for both the TPM and TEE components, preventing replay attacks across both security layers. This dual nonce requirement is specific to environments where both TPM and TEE attestation are requested together, as in a Confidential VM using TEE.

---
