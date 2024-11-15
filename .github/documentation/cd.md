# CD

## Table of Contents
- [Description](#description)
- [Variables](#variables)
- [Project](#project)
- [Credentials](#credentials)

### Description 
This workflow is triggered when a push is made to the `main` branch 
and is responsible for building the application and deploying it to a confidental VM in GCP.

### Variables

The following variables are defined by the workflow:
- `GCP_PROJECT`: The GCP project to deploy the application to.
- `GCP_IAMGE_NAME`: The name of the Packer image that is built
- `GCP_CREDENTIALS_JSON`: The GCP credentials in JSON format.

### Project

The project is called `hyperbeam-cd` and is a project in the `arweave` organization on GCP.

### Credentials

The credentials are standard GCP service account credentials.
The service-account is created as follows:

```sh
$ gcloud iam service-accounts create hyperbeam-cd-gha \
    --description="Service account for the hyperbeam-cd project" \
    --display-name="hyperbeam-cd-gha"
```
Generally, these are stored in `.json` file, created as follows:

```sh
$ gcloud iam service-accounts keys create "hyperbeam-cd-gha.json" \
 --iam-account "hyperbeam-cd-gha@hyperbeam-cd.iam.gserviceaccount.com"
```

The credentials need to be securely stored, and hence they are stored in the repository as a secret.
Improvements here can be made to make sure the credentials have a short time-to-live (TTL), and are not necessarily kept as a GitHub secret.