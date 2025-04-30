# [Module dev_green_zone.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_green_zone.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_trusted_node-4">add_trusted_node/4*</a></td><td></td></tr><tr><td valign="top"><a href="#become-3">become/3</a></td><td></td></tr><tr><td valign="top"><a href="#calculate_node_message-3">calculate_node_message/3*</a></td><td></td></tr><tr><td valign="top"><a href="#decrypt_zone_key-2">decrypt_zone_key/2*</a></td><td></td></tr><tr><td valign="top"><a href="#default_zone_required_opts-1">default_zone_required_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#encrypt_payload-2">encrypt_payload/2*</a></td><td></td></tr><tr><td valign="top"><a href="#finalize_become-5">finalize_become/5*</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#join-3">join/3</a></td><td></td></tr><tr><td valign="top"><a href="#join_peer-5">join_peer/5*</a></td><td></td></tr><tr><td valign="top"><a href="#key-3">key/3</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_set_zone_opts-4">maybe_set_zone_opts/4*</a></td><td></td></tr><tr><td valign="top"><a href="#rsa_wallet_integration_test-0">rsa_wallet_integration_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#try_mount_encrypted_volume-2">try_mount_encrypted_volume/2*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_join-3">validate_join/3*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_peer_opts-2">validate_peer_opts/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_trusted_node-4"></a>

### add_trusted_node/4 * ###

<pre><code>
add_trusted_node(NodeAddr::binary(), Report::map(), RequesterPubKey::term(), Opts::map()) -&gt; ok
</code></pre>
<br />

<a name="become-3"></a>

### become/3 ###

<pre><code>
become(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="calculate_node_message-3"></a>

### calculate_node_message/3 * ###

`calculate_node_message(RequiredOpts, Req, List) -> any()`

<a name="decrypt_zone_key-2"></a>

### decrypt_zone_key/2 * ###

<pre><code>
decrypt_zone_key(EncZoneKey::binary(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

<a name="default_zone_required_opts-1"></a>

### default_zone_required_opts/1 * ###

`default_zone_required_opts(Opts) -> any()`

<a name="encrypt_payload-2"></a>

### encrypt_payload/2 * ###

<pre><code>
encrypt_payload(AESKey::binary(), RequesterPubKey::term()) -&gt; binary()
</code></pre>
<br />

<a name="finalize_become-5"></a>

### finalize_become/5 * ###

`finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts) -> any()`

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(M1::term(), M2::term(), Opts::map()) -&gt; {ok, binary()}
</code></pre>
<br />

<a name="join-3"></a>

### join/3 ###

<pre><code>
join(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="join_peer-5"></a>

### join_peer/5 * ###

<pre><code>
join_peer(PeerLocation::binary(), PeerID::binary(), M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, map()}
</code></pre>
<br />

<a name="key-3"></a>

### key/3 ###

<pre><code>
key(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="maybe_set_zone_opts-4"></a>

### maybe_set_zone_opts/4 * ###

`maybe_set_zone_opts(PeerLocation, PeerID, Req, InitOpts) -> any()`

<a name="rsa_wallet_integration_test-0"></a>

### rsa_wallet_integration_test/0 * ###

`rsa_wallet_integration_test() -> any()`

<a name="try_mount_encrypted_volume-2"></a>

### try_mount_encrypted_volume/2 * ###

`try_mount_encrypted_volume(AESKey, Opts) -> any()`

<a name="validate_join-3"></a>

### validate_join/3 * ###

<pre><code>
validate_join(M1::term(), Req::map(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

<a name="validate_peer_opts-2"></a>

### validate_peer_opts/2 * ###

`validate_peer_opts(Req, Opts) -> any()`

