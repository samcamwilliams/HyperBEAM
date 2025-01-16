#!/bin/bash

# Get the directory the script is located at and change to it
SCRIPT_DIR=$(dirname "$(realpath "$0")")
cd $SCRIPT_DIR

# Globals
BUILD_RELEASE_DIR="$SCRIPT_DIR/_build/default/rel/hb"
RELEASE_DIR="$SCRIPT_DIR/release"

# Default values (replace these with your actual defaults)
DEFAULT_GCI_NAME="hyperbeam-os-instance"
DEFAULT_GCI_PROJECT="arweave-437622"
DEFAULT_GCI_IMAGE="hyperbeam-os-image"
DEFAULT_GCI_REGION="us-central1"
DEFAULT_GCI_ZONE="us-central1-c"

# Prompt the user for input and use the default if nothing is provided
echo "Please provide the following information (leave blank to accept the default values)."

read -p "Enter GCI_NAME (default: $DEFAULT_GCI_NAME): " GCI_NAME
GCI_NAME="${GCI_NAME:-$DEFAULT_GCI_NAME}"

read -p "Enter GCI_PROJECT (default: $DEFAULT_GCI_PROJECT): " GCI_PROJECT
GCI_PROJECT="${GCI_PROJECT:-$DEFAULT_GCI_PROJECT}"

read -p "Enter GCI_IMAGE (default: $DEFAULT_GCI_IMAGE): " GCI_IMAGE
GCI_IMAGE="${GCI_IMAGE:-$DEFAULT_GCI_IMAGE}"

read -p "Enter GCI_REGION (default: $DEFAULT_GCI_REGION): " GCI_REGION
GCI_REGION="${GCI_REGION:-$DEFAULT_GCI_REGION}"

read -p "Enter GCI_ZONE (default: $DEFAULT_GCI_ZONE): " GCI_ZONE
GCI_ZONE="${GCI_ZONE:-$DEFAULT_GCI_ZONE}"

# Print the values to be used for building the image and deploying instance
echo "Using the following values for building the image and deploying instance:"
echo "Using GCI_NAME: $GCI_NAME"
echo "Using GCI_PROJECT: $GCI_PROJECT"
echo "Using GCI_IMAGE: $GCI_IMAGE"
echo "Using GCI_REGION: $GCI_REGION"
echo "Using GCI_ZONE: $GCI_ZONE"

# Start the rebar3 release process
echo "=== Starting the rebar3 release process ==="
rebar3 release
echo "=== Rebar3 release process completed ==="

# Move the build release to the release directory
echo "Moving the build release to the release directory..."
rm -rf $RELEASE_DIR/hb
cp -r $BUILD_RELEASE_DIR $RELEASE_DIR/hb

# Create a tar.gz file of the release directory
echo "Creating a tar.gz file of the release directory..."
tar -czvf $SCRIPT_DIR/release.tar.gz -C $SCRIPT_DIR release

# Export the environment variables
echo "=== Exporting environment variables ==="
export GCI_NAME
export GCI_PROJECT
export GCI_IMAGE
export GCI_REGION
export GCI_ZONE
echo "=== Environment variables exported ==="

# Loop for Packer build and re-run if needed
while true; do
  # Ask user if they want to continue with the Packer build
  read -p "Do you want to continue with the Packer build? (y/n): " CONTINUE_PACKER
  if [[ "$CONTINUE_PACKER" =~ ^[Yy]$ ]]; then
    echo "Initializing Packer..."
    packer init .
    echo "Validating Packer template..."
    packer validate .

    echo "Building Packer image..."
    
    # Capture the output of the packer build command
    OUTPUT=$(packer build . 2>&1)

    # Check if the output contains the phrase "already exists in project"
    if echo "$OUTPUT" | grep -q "already exists in project"; then
      echo "The image already exists in the project."

      # Ask user if they want to force the build or select a different GCI_IMAGE
      read -p "Do you want to force the build (y) or select a different GCI_IMAGE (n)? " FORCE_BUILD
      if [[ "$FORCE_BUILD" =~ ^[Yy]$ ]]; then
        echo "Forcing the build to overwrite the existing image."
        packer build -force .
        break  # Exit the loop if the build is forced
      else
        read -p "Enter a new GCI_IMAGE: " GCI_IMAGE
        export GCI_IMAGE
        echo "Using the new GCI_IMAGE: $GCI_IMAGE"
        # Re-run the build with the new GCI_IMAGE (this will go back to the start of the loop)
      fi
    else
      echo "Packer build completed successfully."
      break  # Exit the loop if the build is successful
    fi
  else
    echo "Skipping Packer build."
    break  # Exit the loop if the user decides not to continue
  fi
done

# Ask user if they want to continue with the gcloud command
read -p "Do you want to continue with the Google Cloud instance creation? (y/n): " CONTINUE_GCLOUD
if [[ "$CONTINUE_GCLOUD" =~ ^[Yy]$ ]]; then
  echo "Creating Google Cloud instance..."
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
  echo "Google Cloud instance creation command executed."
else
  echo "Skipping Google Cloud instance creation."
fi

# Clean up
echo "Cleaning up..."

# Unset the environment variables
unset GCI_NAME
unset GCI_PROJECT
unset GCI_IMAGE
unset GCI_REGION
unset GCI_ZONE

# Remove the release/hb directory
rm -rf $RELEASE_DIR/hb

# Remove the release.tar.gz file
rm -f $SCRIPT_DIR/release.tar.gz

echo "Clean up completed."