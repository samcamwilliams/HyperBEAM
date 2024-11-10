-module(cu_test).
-export([simple_stack_test/0, full_push_test/0, simple_load_test/0]).
-export([init/0, generate_test_data/1, run/2]).

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

init() ->
    application:ensure_all_started(ao),
    ok.

run(Proc, Msg) ->
    run(Proc, Msg, #{}).
run(Proc, Msg, _Opts) ->
    ao_cache:write(ao:get(store), Msg),
    ao_cache:write(ao:get(store), Proc),
    Scheduler = su_registry:find(ar_util:id(Proc, signed), true),
    Assignment = su_process:schedule(Scheduler, Msg),
    cu_process:result(ar_util:id(Proc, signed), ar_util:id(Assignment, unsigned), ao:get(store), ao:wallet()).

%%% TESTS

simple_stack_test() ->
    init(),
    {Proc, Msg} = generate_test_data(<<"return 42">>),
    {ok, Result} = run(Proc, Msg, #{ on_idle => terminate }),
    #tx { data = <<"42">> } = maps:get(<<"/Data">>, Result),
    ok.

full_push_test_() ->
    {timeout, 150, ?_assert(full_push_test())}.

full_push_test() ->
    init(),
    ?c(full_push_test_started),
    {_, Msg} = generate_test_data(ping_ping_script()),
    ao_cache:write(ao:get(store), Msg),
    ao_client:push(Msg, none).

simple_load_test() ->
    init(),
    ?c(scheduling_many_items),
    Messages = 30,
    Msg = generate_test_data(ping_ping_script()),
    ao_cache:write(ao:get(store), Msg),
    Start = ao:now(),
    Assignments = lists:map(
        fun(_) -> ao_client:schedule(Msg) end,
        lists:seq(1, Messages)
    ),
    Scheduled = ao:now(),
    {ok, LastAssignment} = lists:last(Assignments),
    ?c({scheduling_many_items_done_s, ((Scheduled - Start) / Messages) / 1000}),
    ao_client:compute(LastAssignment, Msg),
    Computed = ao:now(),
    ?c({compute_time_s, ((Computed - Scheduled) / Messages) / 1000}),
    ?c({total_time_s, ((Computed - Start) / Messages) / 1000}),
    ?c({processed_messages, Messages}).

default_test_img(Wallet) ->
    Store = ao:get(store),
    {ok, Module} = file:read_file("test/aos-2-pure-xs.wasm"),
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


default_test_devices(Wallet, Opts) ->
    ID = ar_wallet:to_address(Wallet),
    Img = maps:get(image, Opts),
    Quorum = maps:get(quorum, Opts, 2),
    LocalAddress = ao:address(),
    [
        {<<"Protocol">>, <<"ao">>},
        {<<"Variant">>, <<"ao.tn.2">>},
        {<<"Type">>, <<"Process">>},
        {<<"Device">>, <<"Stack">>},
        {<<"Device">>, <<"Scheduler">>},
        {<<"Location">>, ar_util:id(ID)},
        {<<"Device">>, <<"PODA">>},
        {<<"Quorum">>, integer_to_binary(Quorum)}
    ] ++
    [
        {<<"Authority">>, Addr} ||
            Addr <- maps:keys(maps:get(compute, ao:get(nodes))),
            Addr =/= '_',
            Addr =/= LocalAddress
    ] ++
    [
        {<<"Device">>, <<"JSON-Interface">>},
        {<<"Device">>, <<"VFS">>},
        {<<"Device">>, <<"WASM64-pure">>},
        {<<"Module">>, <<"aos-2-pure">>},
        {<<"Image">>, ar_util:id(Img)},
        {<<"Device">>, <<"Cron">>},
        {<<"Time">>, <<"100-Milliseconds">>},
        {<<"Device">>, <<"Multipass">>},
        {<<"Passes">>, <<"3">>}
    ].

ping_ping_script() ->
    <<
        "\n"
        "Handlers.add(\"Ping\", function(m) Send({ Target = ao.id, Action = \"Ping\" }); print(\"Sent Ping\"); end)\n"
        "Send({ Target = ao.id, Action = \"Ping\" })\n"
    >>.

generate_test_data(Script) ->
    generate_test_data(Script, ao:wallet()).
generate_test_data(Script, Wallet) ->
    Img = default_test_img(Wallet),
    generate_test_data(Script, Wallet, #{image => Img}).
generate_test_data(Script, Wallet, Opts) ->
    Devs = default_test_devices(Wallet, Opts),
    generate_test_data(Script, Wallet, Opts, Devs).
generate_test_data(Script, Wallet, _Opts, Devs) ->
    Store = ao:get(store),
    ao_cache:write(
        Store,
        SignedProcess = ar_bundles:sign_item(
            #tx{ tags = Devs },
            Wallet
        )
    ),
    Msg = ar_bundles:sign_item(
        #tx{
            target = ar_bundles:id(SignedProcess, signed),
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
    ?c({test_data_written, {proc, ar_util:id(SignedProcess, signed)}, {msg, ar_util:id(Msg, unsigned)}}),
    {SignedProcess, Msg}.
