-module(cu_test).
-export([simple_stack_test/0, full_push_test/0, simple_load_test/0]).

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").
-ao_debug(print).

run(Proc, Msg) ->
    Scheduler = su_registry:find(Proc#tx.id, true),
    Assignment = su_process:schedule(Scheduler, Msg),
    cu_process:result(Proc#tx.id, Assignment#tx.id, ao:get(store), ao:wallet()).

%%% TESTS

simple_stack_test() ->
    {Proc, Msg} = generate_test_data(<<"return 42">>),
    {ok, Res} = run(Proc, Msg),
    ?c({res, Res}).

full_push_test_() ->
    {timeout, 150, ?_assert(full_push_test())}.

full_push_test() ->
    ?c(full_push_test_started),
    {_, Msg} = generate_test_data(ping_poing_script()),
    ao_cache:write(ao:get(store), Msg),
    ao_client:push(Msg, none).

simple_load_test() ->
    ?c(scheduling_many_items),
    Messages = 30,
    Msg = generate_test_data(ping_poing_script()),
    ao_cache:write(ao:get(store), Msg),
    Start = ao:now(),
    Assignments = lists:map(
        fun(_) -> ao_client:schedule(Msg) end,
        lists:seq(1, Messages)
    ),
    Scheduled = ao:now(),
    {ok, LastAssignment} = lists:last(Assignments),
    ?c({scheduling_many_items_done_s, ((Scheduled - Start) / Messages) / 1000}),
    ao_client:compute(LastAssignment),
    Computed = ao:now(),
    ?c({compute_time_s, ((Computed - Scheduled) / Messages) / 1000}),
    ?c({total_time_s, ((Computed - Start) / Messages) / 1000}),
    ?c({processed_messages, Messages}).

default_test_img(Wallet) ->
    Store = ao:get(store),
    {ok, Module} = file:read_file("test/aos-2-pure.wasm"),
    ao_cache:write(
        Store,
        Img = ar_bundles:sign_item(
            #tx {
                tags = [
                    {<<"Protocol">>, <<"ao">>},
                    {<<"Variant">>, <<"ao.tn.2">>},
                    {<<"Type">>, <<"Image">>}
                ],
                data = Module
            },
            Wallet
        )
    ),
    Img.

default_test_devices(Wallet, Img) ->
    ID = ar_wallet:to_address(Wallet),
    [
        {<<"Protocol">>, <<"ao">>},
        {<<"Variant">>, <<"ao.tn.2">>},
        {<<"Type">>, <<"Process">>},
        {<<"Authority">>, ar_util:encode(ID)},
        {<<"Device">>, <<"Scheduler">>},
        {<<"Location">>, ar_util:encode(ID)},
        {<<"Device">>, <<"JSON-Interface">>},
        {<<"Device">>, <<"VFS">>},
        {<<"Device">>, <<"WASM64-pure">>},
        {<<"Image">>, ar_util:encode(Img#tx.id)},
        {<<"Module">>, <<"aos-2-pure">>},
        {<<"Device">>, <<"Cron">>},
        {<<"Time">>, <<"100-Milliseconds">>}
    ].

ping_poing_script() ->
    <<
        "\n"
        "Handlers.add(\"Ping\", function(m) Send({ Target = ao.id, Action = \"Ping\" }); print(\"Sent Ping\"); end)\n"
        "Send({ Target = ao.id, Action = \"Ping\" })\n"
    >>.

generate_test_data(Script) ->
    generate_test_data(Script, ao:wallet()).
generate_test_data(Script, Wallet) ->
    Img = default_test_img(Wallet),
    generate_test_data(Script, Wallet, Img).
generate_test_data(Script, Wallet, Img) ->
    Devs = default_test_devices(Wallet, Img),
    generate_test_data(Script, Wallet, Img, Devs).
generate_test_data(Script, Wallet, _Img, Devs) ->
    Store = ao:get(store),
    ao_cache:write(
        Store,
        Signed = ar_bundles:sign_item(
            #tx{ tags = Devs },
            Wallet
        )
    ),
    Msg = ar_bundles:sign_item(
        #tx{
            target = Signed#tx.id,
            tags = [
                {<<"Protocol">>, <<"ao">>},
                {<<"Variant">>, <<"ao.tn.2">>},
                {<<"Type">>, <<"Message">>},
                {<<"Action">>, <<"Eval">>}
            ],
            data = Script
        },
        Wallet
    ),
    ao_cache:write(Store, Msg),
    ?c({test_data_written, {proc, ar_util:encode(Signed#tx.id)}, {msg, ar_util:encode(Msg#tx.id)}}),
    {Signed, Msg}.
