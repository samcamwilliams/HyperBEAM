-module(dev_meta).
-export([execute/1]).
-include("include/ao.hrl").

%%% The hyperbeam HTTP API meta device, which is the default entry point
%%% for all messages that do not otherwise specify a device.
execute(CarrierMsg) ->
    ?c({meta_execute, CarrierMsg}),
    {M, Keys} = parse_carrier_msg(CarrierMsg),
    {ok, execute_many(M, Keys)}.

execute_many(M, []) -> M;
execute_many(M, [Key|Keys]) ->
    {ok, Dev} = cu_device_loader:from_message(M),
    {ok, NewM} =
        cu_device:call(
            Dev, binary_to_existing_atom(Key, utf8), [M],
            #{ error_strategy => throw }),
    execute_many(NewM, Keys).

%% @doc Resolve the carrier message to an executable message, either by extracting
%% from its body or reading from its referenced ID.
parse_carrier_msg(Carrier = #tx { data = <<>> }) ->
    {Carrier#tx.data, extract_path_components(Carrier)};
parse_carrier_msg(CarrierMsg) ->
    [MsgID|Keys] = extract_path_components(CarrierMsg),
    {ao_cache:read(ao:get(store), MsgID), Keys}.

%% @doc Extract the components of the path from the carrier message.
extract_path_components(CarrierMsg) ->
    binary:split(CarrierMsg#tx.data, <<"/">>, [global]).