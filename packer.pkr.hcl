packer {
  required_plugins {
    googlecompute = {
      version = ">= 1.1.6"
      source  = "github.com/hashicorp/googlecompute"
    }
  }
}

# Define required variables
variable "project_id" {
  type    = string
  default = "arweave-437622"
}

variable "region" {
  type    = string
  default = "us-east1"
}

variable "zone" {
  type    = string
  default = "us-east1-c"
}

variable "image_family" {
  type    = string
  default = "ao-image"
}

# Source block to define GCP builder
source "googlecompute" "ubuntu" {
  project_id          = var.project_id
  source_image_family = "ubuntu-2204-lts"
  zone                = var.zone
  machine_type        = "n1-standard-1"
  ssh_username        = "packer"
}

# Define the build stage
build {
  sources = ["source.googlecompute.ubuntu"]

  # Add a provisioner to download and install go-tpm-tools
  provisioner "shell" {
    inline = [
      "sudo apt-get update -y",
      "sudo apt-get install -y wget tar",

      # Download the go-tpm-tools binary archive
      "wget https://github.com/google/go-tpm-tools/releases/download/v0.4.4/go-tpm-tools_Linux_x86_64.tar.gz -O /tmp/go-tpm-tools.tar.gz",

      # Extract the binary
      "tar -xzf /tmp/go-tpm-tools.tar.gz -C /tmp",

      # Move the gotpm binary to /usr/local/bin
      "sudo mv /tmp/gotpm /usr/local/bin/",

      # Clean up
      "rm -f /tmp/go-tpm-tools.tar.gz /tmp/LICENSE /tmp/README.md"
    ]
  }


  # Upload the pre-built release (with ERTS included) to the instance
  provisioner "file" {
    source      = "./_build/default/rel/ao"
    destination = "/tmp/ao"
  }

  provisioner "shell" {
    inline = [
      # Move the release to /opt with sudo
      "sudo mv /tmp/ao /opt/ao",
      "sudo chmod -R 755 /opt/ao",

      # Create a symlink to make it easier to run the app
      "sudo ln -s /opt/ao/bin/ao /usr/local/bin/ao",

      # (Optional) If you want to create a systemd service to manage the app
      "echo '[Unit]' | sudo tee /etc/systemd/system/ao.service",
      "echo 'Description=Permaweb Node' | sudo tee -a /etc/systemd/system/ao.service",
      "echo '[Service]' | sudo tee -a /etc/systemd/system/ao.service",
      "echo 'Type=simple' | sudo tee -a /etc/systemd/system/ao.service",
      "echo 'ExecStart=/opt/ao/bin/ao foreground' | sudo tee -a /etc/systemd/system/ao.service",
      "echo 'Restart=on-failure' | sudo tee -a /etc/systemd/system/ao.service",
      "echo '[Install]' | sudo tee -a /etc/systemd/system/ao.service",
      "echo 'WantedBy=multi-user.target' | sudo tee -a /etc/systemd/system/ao.service",

      # Enable and start the service
      "sudo systemctl enable ao",
      "sudo systemctl start ao"
    ]
  }

  # Disable ssh
  # provisioner "shell" {
  #  inline = [
  #    "sudo systemctl stop ssh",
  #    "sudo systemctl disable ssh"
  #  ]
  # }
}

