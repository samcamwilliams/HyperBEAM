

# Module dev_green_zone #
* [Description](#description)

The green zone device, which provides secure communication and identity
management between trusted nodes.

<a name="description"></a>

## Description ##
It handles node initialization, joining
existing green zones, key exchange, and node identity cloning. All operations
are protected by hardware commitment and encryption.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_trusted_node-4">add_trusted_node/4*</a></td><td>Add a joining node's details to the trusted nodes list.</td></tr><tr><td valign="top"><a href="#become-3">become/3</a></td><td>Clone the identity of a target node.</td></tr><tr><td valign="top"><a href="#calculate_node_message-3">calculate_node_message/3*</a></td><td>Generate the node message that should be set prior to joining a green zone.</td></tr><tr><td valign="top"><a href="#decrypt_zone_key-2">decrypt_zone_key/2*</a></td><td>Decrypt the zone AES key using the node's RSA private key.</td></tr><tr><td valign="top"><a href="#default_zone_required_opts-1">default_zone_required_opts/1*</a></td><td>The default required options for a green zone.</td></tr><tr><td valign="top"><a href="#encrypt_payload-2">encrypt_payload/2*</a></td><td>Encrypt the shared AES key with the requester's RSA public key.</td></tr><tr><td valign="top"><a href="#finalize_become-5">finalize_become/5*</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the green zone.</td></tr><tr><td valign="top"><a href="#join-3">join/3</a></td><td>Initiate the join process for a node (Node B).</td></tr><tr><td valign="top"><a href="#join_peer-5">join_peer/5*</a></td><td>Process an internal join request when a target peer is specified.</td></tr><tr><td valign="top"><a href="#key-3">key/3</a></td><td>Retrieve and encrypt the node's private key.</td></tr><tr><td valign="top"><a href="#maybe_set_zone_opts-4">maybe_set_zone_opts/4*</a></td><td>If the operator requests it, the node can automatically adopt the
necessary configuration to join a green zone.</td></tr><tr><td valign="top"><a href="#rsa_wallet_integration_test-0">rsa_wallet_integration_test/0*</a></td><td>Test RSA operations with the existing wallet structure.</td></tr><tr><td valign="top"><a href="#validate_join-3">validate_join/3*</a></td><td>Validate an incoming join request.</td></tr><tr><td valign="top"><a href="#validate_peer_opts-2">validate_peer_opts/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_trusted_node-4"></a>

### add_trusted_node/4 * ###

<pre><code>
add_trusted_node(NodeAddr::binary(), Report::map(), RequesterPubKey::term(), Opts::map()) -&gt; ok
</code></pre>
<br />

`NodeAddr`: The joining node's address.<br />`Report`: The commitment report provided by the joining node.<br />`RequesterPubKey`: The joining node's public key.<br />`Opts`: A map of configuration options.<br />

returns: ok.

Add a joining node's details to the trusted nodes list.
Updates the local configuration with the new trusted node's commitment report
and public key.

<a name="become-3"></a>

### become/3 ###

<pre><code>
become(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: The message containing the target node's encrypted private key and IV.<br />`M2`: Ignored parameter.<br />`Opts`: A map of configuration options. Must include `priv_green_zone_aes`.<br />

returns: {ok, Map} on success, where Map includes:
- status: 200
- message: confirmation text
- node: the target node's address
Returns {error, Reason} if the node is not part of the green zone.

Clone the identity of a target node.
Allows a node to adopt the identity of a target node by retrieving the target
node's encrypted private key and IV, decrypting it using the shared AES key,
and updating the local node's wallet with the target node's keypair.

<a name="calculate_node_message-3"></a>

### calculate_node_message/3 * ###

`calculate_node_message(RequiredOpts, Req, List) -> any()`

Generate the node message that should be set prior to joining a green zone.
This function takes a required opts message, a request message, and an `adopt-config`
value. The `adopt-config` value can be a boolean, a list of fields that should be
included in the node message from the request, or a binary string of fields to
include, separated by commas.

<a name="decrypt_zone_key-2"></a>

### decrypt_zone_key/2 * ###

<pre><code>
decrypt_zone_key(EncZoneKey::binary(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`EncZoneKey`: The encrypted zone AES key (Base64 encoded or binary).<br />`Opts`: A map of configuration options.<br />

returns: {ok, DecryptedKey} on success, where DecryptedKey is the shared AES key.

Decrypt the zone AES key using the node's RSA private key.
Decrypts the encrypted zone AES key using the RSA private key from the node's
wallet.

<a name="default_zone_required_opts-1"></a>

### default_zone_required_opts/1 * ###

`default_zone_required_opts(Opts) -> any()`

The default required options for a green zone. These are intended as
sane basic requirements for a green zone:
- The node will not load remote devices (or trust extra peers).
- The node will use only the default preloaded devices (found on the
initiating machine).
- The node uses the default store configuration.
- The node will not change its routes from the defaults.
- The peer's preprocessor and postprocessor are the same as the local node's.
- The node will not schedule messages. Without coordination, peers in the
green zone will schedule messages without regard for avoiding
double-assignment of slots.
- The node must be in a permanent state (no further configuration changes
being allowed).
Each of these options is derived from the present node's configuration.

<a name="encrypt_payload-2"></a>

### encrypt_payload/2 * ###

<pre><code>
encrypt_payload(AESKey::binary(), RequesterPubKey::term()) -&gt; binary()
</code></pre>
<br />

`AESKey`: The shared AES key (256-bit binary).<br />`RequesterPubKey`: The requester's public RSA key.<br />

returns: The AES key encrypted with the RSA public key.

Encrypt the shared AES key with the requester's RSA public key.
Encrypts the shared AES key using the RSA public key provided by the joining
node. The RSA public key is extracted from a tuple and converted into a
record suitable for encryption.

<a name="finalize_become-5"></a>

### finalize_become/5 * ###

`finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts) -> any()`

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(M1::term(), M2::term(), Opts::map()) -&gt; {ok, binary()}
</code></pre>
<br />

`M1`: Ignored parameter.<br />`M2`: Optionally contains a `required-config` map. If not provided, the
            default required config (derived from the nodes base configuration)
            will be used.<br />`Opts`: A map containing configuration options. If the wallet is not already
              provided (under key `priv_wallet`), a new one will be created.<br />

returns: {ok, Msg} where Msg is a binary confirmation message.

Initialize the green zone.
Sets up the node's cryptographic identity by ensuring that a wallet (keypair)
exists and generating a shared AES key for secure communication. The wallet,
AES key, and an empty trusted nodes list are stored in the node's configuration.

<a name="join-3"></a>

### join/3 ###

<pre><code>
join(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: The join request message containing a header with the target peer's
            address.<br />`M2`: Ignored parameter.<br />`Opts`: A map of configuration options.<br />

returns: {ok, Map} on success with join response details, or {error, Reason}
on failure.

Initiate the join process for a node (Node B).

When Node B wishes to join an existing green zone, it sends a GET request to
its local join endpoint.
This request includes a header with the target peer's address (Node A).

Based on the presence of a peer address:
- If the target peer is specified, Node B internally routes the request to
the join_peer flow, where it generates an commitment report and prepares
a POST request to forward to Node A.
- If no peer address is present, the join request is processed locally via
the validate_join flow.

<a name="join_peer-5"></a>

### join_peer/5 * ###

<pre><code>
join_peer(PeerLocation::binary(), PeerID::binary(), M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, map()}
</code></pre>
<br />

`M1`: Ignored parameter.<br />`M2`: Ignored parameter.<br />`Opts`: A map of configuration options.<br />

returns: {ok, Map} on success with a confirmation message, or {error, Map} on failure.

Process an internal join request when a target peer is specified.

In this flow (executed on Node B):
1. Node B generates an commitment report and prepares a POST request.
2. It then forwards the POST request to Node A's join endpoint.
3. Upon receiving a response from Node A, Node B decrypts the returned
zone-key (an encrypted shared AES key) using its local private key, then
updates its configuration with the shared AES key.

<a name="key-3"></a>

### key/3 ###

<pre><code>
key(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: Ignored parameter.<br />`M2`: Ignored parameter.<br />`Opts`: A map of configuration options. Must include keys `priv_wallet`
              and `priv_green_zone_aes`.<br />

returns: {ok, Map} on success, where Map contains:
- status: 200
- encrypted_key: the encrypted private key (Base64 encoded)
- iv: the initialization vector (Base64 encoded)
Returns {error, Reason} if the node is not part of the green zone.

Retrieve and encrypt the node's private key.
Encrypts the node's private key using the shared AES key in AES-256-GCM mode.
It returns the encrypted key along with the initialization vector (IV) needed
for decryption.

<a name="maybe_set_zone_opts-4"></a>

### maybe_set_zone_opts/4 * ###

`maybe_set_zone_opts(PeerLocation, PeerID, Req, InitOpts) -> any()`

If the operator requests it, the node can automatically adopt the
necessary configuration to join a green zone. `adopt-config` can be a boolean,
a list of fields that should be included in the node message, alongside the
required config of the green zone they are joining.

<a name="rsa_wallet_integration_test-0"></a>

### rsa_wallet_integration_test/0 * ###

`rsa_wallet_integration_test() -> any()`

Test RSA operations with the existing wallet structure.
This test function verifies that encryption and decryption using the RSA keys
from the wallet work correctly. It creates a new wallet, encrypts a test
message with the RSA public key, and then decrypts it with the RSA private
key, asserting that the decrypted message matches the original.

<a name="validate_join-3"></a>

### validate_join/3 * ###

<pre><code>
validate_join(M1::term(), Req::map(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: Ignored parameter.<br />`Req`: The join request message containing the commitment report and
           other join details.<br />`Opts`: A map of configuration options.<br />

returns: {ok, Map} on success with join response details, or {error, Reason}
if verification fails.

Validate an incoming join request.

When Node A receives a POST join request from Node B, this routine is executed:
1. It extracts the commitment report, the requesting node's address, and
the encoded public key.
2. It verifies the commitment report included in the request.
3. If the report is valid, Node A adds Node B to its list of trusted nodes.
4. Node A then encrypts the shared AES key (zone-key) with Node B's public
key and returns it along with its public key.

<a name="validate_peer_opts-2"></a>

### validate_peer_opts/2 * ###

`validate_peer_opts(Req, Opts) -> any()`

