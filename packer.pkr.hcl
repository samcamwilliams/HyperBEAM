packer {
  required_plugins {
    googlecompute = {
      version = ">= 1.1.6"
      source  = "github.com/hashicorp/googlecompute"
    }
  }
}

# Variables for customization
variable "project_id" {
  type    = string
  default = "${env("GCI_PROJECT")}" # Using env function for environment variable
}

variable "region" {
  description = "GCP region"
  type        = string
  default     = "${env("GCI_REGION")}" # Using env function for environment variable
}

variable "zone" {
  description = "GCP zone"
  type        = string
  default     = "${env("GCI_ZONE")}" # Using env function for environment variable
}

variable "image_name" {
  description = "Name of the custom image"
  type        = string
  default     = "${env("GCI_IMAGE")}" # Using env function for environment variable
}

# GCP Builder
source "googlecompute" "ubuntu" {
  project_id          = var.project_id
  source_image_family = "ubuntu-minimal-2204-lts" # Base image family
  image_name          = var.image_name
  zone                = var.zone
  machine_type        = "n1-standard-1"
  ssh_username        = "packer"
}


# Define the build stage
build {
  sources = ["source.googlecompute.ubuntu"]

  # Upload the pre-built release (with ERTS included) to the instance
  provisioner "file" {
    source      = "./release.tar.gz"
    destination = "/tmp/release.tar.gz"
  }

  provisioner "shell" {
    inline = [
      "echo 'Updating packages...'",
      "sudo apt-get update -y",

      # Install dependencies
      "echo 'Installing dependencies...'",
      "sudo apt-get install -y libglu1-mesa ufw tar",

      # Extract the release
      "echo 'Extracting the release...'",
      "tar -xzf /tmp/release.tar.gz -C /tmp",

      # Install snpguest
      "echo 'Installing snpguest...'",
      "sudo chmod +x /tmp/release/snpguest",
      "sudo cp /tmp/release/snpguest /usr/local/bin/snpguest",

      # Install Hyperbeam
      "echo 'Installing Hyperbeam...'",
      "sudo mv /tmp/release/hb /opt/hb",
      "sudo chmod -R 755 /opt/hb",
      "sudo ln -s /opt/hb/bin/hb /usr/local/bin/hb",

      # Create a systemd service to for Hyperbeam
      "echo '[Unit]' | sudo tee /etc/systemd/system/hb.service",
      "echo 'Description=Permaweb Node' | sudo tee -a /etc/systemd/system/hb.service",
      "echo '[Service]' | sudo tee -a /etc/systemd/system/hb.service",
      "echo 'Type=simple' | sudo tee -a /etc/systemd/system/hb.service",
      "echo 'ExecStart=/opt/hb/bin/hb foreground' | sudo tee -a /etc/systemd/system/hb.service",
      "echo 'Restart=on-failure' | sudo tee -a /etc/systemd/system/hb.service",
      "echo '[Install]' | sudo tee -a /etc/systemd/system/hb.service",
      "echo 'WantedBy=multi-user.target' | sudo tee -a /etc/systemd/system/hb.service",

      # Enable and start the Hyperbeam service
      "sudo systemctl enable hb",
      "sudo systemctl start hb",

      # Uninstall Snap files
      "sudo snap remove google-cloud-cli",
      "sudo snap remove core20",
      "sudo snap remove snapd",
      "sudo apt-get purge -y snapd",

      # Remove Home directories
      "sudo rm -rf /home/*",

      #   # Disable and remove ssh
      #   "sudo systemctl disable ssh",
      #   "sudo apt-get purge -y openssh",

      # Block all ports except 80, 443, and 8734 this disables SSH access
      "sudo ufw enable",
      "sudo ufw default deny incoming",
      "sudo ufw default allow outgoing",
      "sudo ufw allow 80",
      "sudo ufw allow 443",
      "sudo ufw allow 8734",

      # Clean up
      "sudo apt-get clean",

      "echo 'Provisioning complete!'"
    ]
  }

}

