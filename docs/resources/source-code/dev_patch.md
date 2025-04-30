# [Module dev_patch.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_patch.erl)




A device that finds `PATCH` requests in the `results/outbox`
of its message, and applies them to it.

<a name="description"></a>

## Description ##
This can be useful for processes
whose computation would like to manipulate data outside of the `results` key
of its message.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>Find <code>PATCH</code> requests in the <code>results/outbox</code> of the message, and apply
them to the state.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Default process device hooks.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td></td></tr><tr><td valign="top"><a href="#patch_to_submessage_test-0">patch_to_submessage_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td></td></tr><tr><td valign="top"><a href="#uninitialized_patch_test-0">uninitialized_patch_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compute-3"></a>

### compute/3 ###

`compute(Msg1, Msg2, Opts) -> any()`

Find `PATCH` requests in the `results/outbox` of the message, and apply
them to the state.

<a name="init-3"></a>

### init/3 ###

`init(Msg1, Msg2, Opts) -> any()`

Default process device hooks.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(Msg1, Msg2, Opts) -> any()`

<a name="patch_to_submessage_test-0"></a>

### patch_to_submessage_test/0 * ###

`patch_to_submessage_test() -> any()`

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(Msg1, Msg2, Opts) -> any()`

<a name="uninitialized_patch_test-0"></a>

### uninitialized_patch_test/0 * ###

`uninitialized_patch_test() -> any()`

