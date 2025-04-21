# **hyperBEAM Repository Setup**

This guide provides step-by-step instructions for setting up and testing hyperBEAM.

!!! info "TEE-based Computation"
    If you intend to offer TEE-based computation of AO-Core devices, please see the [hyperBEAM OS repository](https://github.com/permaweb/hb-os) for details on configuration and deployment. Additional documentation on TEE setup and configuration will be added here in future updates.

## **Prerequisites**

Before you begin, ensure you have the following installed:

- The Erlang runtime (OTP 27)
- Rebar3
- Git

## **1. Clone the hyperBEAM Repository**

First, clone the `permaweb/hyperBEAM` repository from GitHub:

```bash
git clone https://github.com/permaweb/hyperBEAM
```

Navigate to the project directory:

```bash
cd hyperBEAM
```

## **2. Compile the Code with Rebar3**

To compile the hyperBEAM code, you'll need to use Rebar3. Run the following command to compile the project:

```bash
rebar3 compile
```

This will compile the necessary code to get hyperBEAM up and running.

## **3. Run hyperBEAM with Shell**

Once the code is compiled, you can start the hyperBEAM shell with Rebar3:

```bash
rebar3 shell
```

This will start hyperBEAM using the default configuration inside the hb_opts.erl,
which preloads all devices and sets up default stores. All of these can be configured using
the config.flat file with any overrides you specify.

To verify that your hyperBEAM node is running correctly, you can check:

```bash
curl http://localhost:10000/~meta@1.0/info
```
If you receive a response with node information, your hyperBEAM installation is working properly.

## **4. Run hyperBEAM with Mainnet**

To start hyperBEAM connected to the mainnet, you can use the `--eval` option with rebar3:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ port => 10001, priv_key_location => <<\"./wallet.json\">>})."
```

To verify that your hyperBEAM node is running correctly, you can check:

```bash
curl http://localhost:10001/~meta@1.0/info
```

If you receive a response with node information, your hyperBEAM installation is working properly.

## **Next Steps**

After setting up hyperBEAM, you should:

1. [Configure your installation](configuration.md) to match your requirements
2. [Run tests](testing.md) to verify everything is working correctly
3. [Connect to the Compute Unit](../compute-unit/setup.md) to complete your setup


