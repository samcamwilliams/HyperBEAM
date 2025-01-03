%%% @doc Implements the legacy/classic exception handling pattern of the 
%%% Emscripten WASM execution environment.
%%% 
%%% Emscripten has many subtly different ways of handling exceptions.
%%% For the avoidance of doubt, the pattern in question works as follows:
%%% 
%%% ```
%%% function invoke_vjj(index, a1, a2) {
%%%   var sp = stackSave();
%%%   try {
%%%     getWasmTableEntry(Number(index))(a1, a2);
%%%   } catch (e) {
%%%     stackRestore(sp);
%%%     if (e !== e + 0) throw e;
%%%     _setThrew(1, 0);
%%%   }
%%% }
%%% '''
%%% 
%%% Where '_vjj' represents the type spec of the function.
-module(dev_emscripten).
-export([info/1, init/3]).

info(_) ->
    #{
        handler => fun router/4
    }.

%% @doc On-boot, initialize the virtual file system with:
%% - Empty stdio files
init(M1, _M2, Opts) ->
    MsgWithLib =
        hb_converge:set(
            M1,
            #{
                <<"WASM/stdlib/_emscripten_system">> =>
                    #{ device => <<"Emscripten/1.0">>}
            },
            Opts
        ),
    {ok, MsgWithLib}.

router(<<"invoke_v", _/binary>>, Msg1, Msg2, Opts) ->
    State = hb_converge:get(<<"State">>, Msg1, #{ hashpath => ignore }),
    WASM = dev_wasm:instance(State, Msg2, Opts),
    [Index|Args] = hb_converge:get(args, Msg2, #{ hashpath => ignore }),
    {ok, SP, _} = hb_beamr:call(WASM, <<"emscripten_stack_get_current">>, []),
    ImportResolver = hb_private:get(<<"WASM/Import-Resolver">>, State, Opts),
    try 
        hb_beamr:call(WASM, <<"_indirect:", Index/binary>>, Args, ImportResolver, State, Opts)
    catch
        _:Error ->
            hb_beamr:call(WASM, <<"_emscripten_stack_restore">>, [SP]),
            hb_beamr:call(WASM, <<"setThrew">>, [1, 0]),
            {error, Error}
    end.