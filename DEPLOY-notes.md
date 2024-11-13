# Deploy notes

This document describes the notes for deploying HyperBEAM using packer.

Packer is a tool that enables a single build to create images for multiple
distribution networks.

The current version is setup for gcp, so you will need to install your gcp.


* https://cloud.google.com/sdk/docs/install

* https://cloud.google.com/sdk/docs/initializing

* https://cloud.google.com/docs/authentication/provide-credentials-adc#how-to

Install Packer

* https://developer.hashicorp.com/packer/tutorials/docker-get-started/get-started-install-cli

```sh
rebar3 clean
rebar3 get-deps
rebar3 compile
rebar3 release
```

```sh
packer init .
packer validate .
packer build .
```

Install go-tpm-tools
Install erlinit

possibly build ao project using nerves

replace /sbin/init with erlinit