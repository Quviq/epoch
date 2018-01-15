%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% @doc QuickCheck model for aec_conductor
%%%      This was created to compare against aec_miner, the predecessor for aec_conductor
%%%
%%%      Now we also include the chain API
%%%      We have different chains of blocks (start with only one, then add forks).
%%%
%%% @end
%%% Created : 18 Nov 2017 by Thomas Arts
%%% Revised : 04 Dec 2017 by Thomas Arts (new version of aec_conductor)

-module(aec_conductor_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile([export_all, nowarn_export_all]).
-define(SUT, aec_conductor).


%% -- State ------------------------------------------------------------------
-record(state,{pid, 
               mining = [],   %% nonces used for blocks being mined
               received = [],
               on_chain = [],
               known_blocks = [],
               suspended = false,
               use_pool = true,
               block,
               have_key = false,
               nonces_used = []
              }).

%% --- Generators ------------

nonce() ->
    ?LET(X, noshrink(largeint()), abs(X)).


initial_state() ->
    #state{}.

%% -- Common pre-/post-conditions --------------------------------------------
command_precondition_common(#state{pid = undefined}, Cmd) ->
    lists:member(Cmd, [start]);
command_precondition_common(_, Cmd) ->
    Cmd =/= start.

%% -- Operations -------------------------------------------------------------

add_mining_next(S, _, [{Nonce, Res} = Tuple, BlockCandidate]) ->
    S#state{mining = S#state.mining ++ [Tuple],
            block = BlockCandidate,
            nonces_used = S#state.nonces_used ++ [ Nonce || Res =/= crash]
           }.


wait_for_key_callouts(S, []) ->
    ?WHEN(not S#state.have_key,
          begin
              ?CALLOUT(aec_keys, pubkey, [], {ok, <<1,2,3,4>>}),
              ?APPLY(set_key, [true])
          end).

wait_for_key_features(S, _Args, _Res) ->
    [ {wait_for_key, S#state.have_key} ].



%% We now start a miner via our block_provider. The minor blocks until we tell it to provide an answer.
%% block_provider:mine(_, _)
mine_new_callouts(S, []) ->
    ?APPLY(wait_for_key, []),
    case S#state.suspended of
        true ->
            ?APPLY(set_block, [undefined]);
        false ->
            %% Keys generator must shrink to 'true' to guarantee termination
            Top = length(S#state.on_chain),
            ?MATCH_GEN([{Block, Nonce}, Keys], [{{main, Top}, nonce()}, 
                                                weighted_default({8, true}, {2, false})]),
            %% By picking a block in the chain, we can create {fork, X}
            ?APPLY(set_key, [Keys]),
            case Keys of
                true -> %% keys available
                    %% I could check whether block candidate argument is top block
                    ?MATCH({TopBlock, _}, ?CALLOUT(aec_mining, create_block_candidate, [?VAR], {ok, block_provider:chain(Block), Nonce})),
                    %% ?WHEN(TopBlock =/= block_provider:chain(main, Top-1), 
                    %%       ?FAIL({not_mining_at_top, TopBlock, block_provider:chain(main, Top-1)})),
                    %% Check does not work: generation versus runtime!
                    ?APPLY(mine_again, [Block]);
                false ->
                    ?CALLOUT(aec_mining, create_block_candidate, [?WILDCARD], {error, key_not_found}),
                    ?APPLY(mine_new, [])
            end
    end.

mine_again_callouts(S, [Block]) ->
    ?WHEN(S#state.have_key,
          begin
              ?CALLOUT(aec_events, publish, [start_mining, ?WILDCARD], ok),
              ?MATCH({Nonce, Result}, ?CALLOUT(mock, mine, [?WILDCARD, ?VAR], elements([ok, {error, no_solution}, crash]))),
              ?APPLY(add_mining, [{Nonce, Result}, Block])
          end).

delta_header_height_callouts(S, []) ->
    Idx = length(S#state.on_chain),
    ?CALLOUT(aec_target, determine_delta_header_height, [?WILDCARD], 
             case Idx - aec_governance:blocks_to_check_difficulty_count() of
                 I when I < 0  -> {error, chain_too_short_to_recalculate_target};
                 I -> {ok, I}
             end).

set_suspend_next(S, _, [Bool]) ->
    S#state{ suspended = Bool }.

set_use_pool_next(S, _, [Bool]) ->
    S#state{ use_pool = Bool }.

set_block_next(S, _, [Block]) ->
    S#state{ block = Block }.

set_key_next(S, _, [Bool]) ->
    S#state{ have_key = Bool }.

on_chain_next(S, _, [Term]) ->
    S#state{ on_chain = S#state.on_chain ++ [Term]}.

received_next(S, _, [Term]) ->
    case lists:member(Term,  S#state.received) of
        true  -> S;
        false -> S#state{ received = S#state.received ++ [Term] }
    end.

known_block_next(S, _, [Term]) ->
    S#state{ known_blocks = S#state.known_blocks ++ [Term] }.

kill_miners_next(S, _, []) ->
    S#state{ mining = [] }.

add_if_new_callouts(S, [Block, _Hash]) ->
    ?WHEN(not lists:member(Block, S#state.on_chain),   %% New block has been found
          begin
              %% ?CALLOUT(aec_events, publish, [mining_preempted, ?WILDCARD], ok),
              ?APPLY(kill_miners, []),
              ?MATCH({Txs1, Txs2, ok}, ?CALLOUT(aec_tx_pool, fork_update, [?VAR, ?VAR], ok)),
              %% All transactions are removed from the pool, even if they verify false (but then they are most likely not there)
              %% unless all transactions verify false, then different logic applies ????
              %% ?WHEN(Txs2 =/= [], ?FAIL(forking)),
              ?APPLY(mine_new, [])
          end).




%% --- Operation: start ---
start_args(_S) ->
    %% [[{autostart, false}], {main, 0}].
    ?LET([B1, B2], [default(true, bool()), default(true, bool())],
         [oneof([sublist([{autostart, B1}, {fetch_new_txs_from_pool, B2}]), no_args]), {main, 0}]).

%% TODO test that we can also change behaviour with environment variables
start(Options, _) ->
    application:unset_env(aeccore, autostart),
    application:unset_env(aeccore, fetch_new_txs_from_pool),
    {ok, Pid} = 
        case Options of
            no_args -> ?SUT:start_link();
            _ -> ?SUT:start_link(Options)
        end,
    unlink(Pid),
    timer:sleep(200),
    Pid.

start_callouts(_S, [Options, {main, 0} = Top]) ->
    ?CALLOUT(aec_persistence, get_chain, [], []),  %% since this forces to read genesis block
    ?CALLOUT(aec_block_genesis, genesis_block, [], block_provider:chain(main, 0)),
    ?APPLY(delta_header_height, []),
    ?APPLY(on_chain, [Top]),
    ?APPLY(set_suspend, [Options =/= no_args andalso lists:member({autostart, false}, Options)]),
    ?APPLY(set_use_pool, [not (Options =/= no_args andalso lists:member({fetch_new_txs_from_pool, false}, Options))]),
    ?APPLY(mine_new, []).

start_next(S, Pid, [_Options, _]) ->
    S#state{pid = Pid}.

%% --- Operation: suspend ---
suspend_args(_S) ->
    [].

suspend() ->
    ?SUT:stop_mining().

suspend_next(S, _Value, []) ->
    S#state{suspended = true}.

suspend_post(_S, [], Res) ->
    eq(Res, ok).

suspend_features(_S, [], _Res) ->
    [ suspended_mining ].



%% --- Operation: resume ---
resume_args(_S) ->
    [].

resume() ->
    ?SUT:start_mining(),
    timer:sleep(30).

resume_callouts(#state{suspended = false}, []) ->
    ?CALLOUT(aec_events, publish, [start_mining, ?WILDCARD], ok);  %% but further a No-Op
resume_callouts(#state{suspended = true} = S, []) ->
    ?APPLY(set_suspend, [false]),
    case S#state.block of
        undefined ->
            ?APPLY(mine_new, []);
        _ ->
            ?APPLY(kill_miners, []),  %% kept alive to test race condition
            ?APPLY(mine_new, [])
    end.

resume_post(_S, [], Res) ->
    eq(Res, ok).

resume_features(S, [], _Res) ->
    [ resumed_mining_when_running ||  not S#state.suspended ] ++
        [ resumed_mining_without_running_miners || S#state.block == undefined ] ++
        [ resumed_mining  || S#state.block =/= undefined andalso S#state.suspended ].


%% --- Operation: mining ---
is_mining_args(_S) ->
    [].

is_mining() ->
    ?SUT:get_mining_state().

is_mining_post(S, [], Res) ->
    case Res of
        running -> not S#state.suspended;
        stopped -> S#state.suspended
    end.




%% --- Operation: finish_mining ---
finish_mining_pre(S) ->
    S#state.mining =/= [].

finish_mining_args(S) ->
    ?LET({Nonce, Result}, elements(S#state.mining),
         [Nonce, Result]).

finish_mining_pre(S, [Nonce, Result]) ->
    lists:member({Nonce, Result}, S#state.mining) 
        andalso length(S#state.on_chain) < 4.

finish_mining_adapt(S, [Nonce, Result]) ->
    case S#state.mining of
        [{N, R}] -> [N, R];
        _ -> [Nonce, Result]
    end.


finish_mining(Nonce, Result) ->
    block_provider:continue(Nonce),
    timer:sleep(100),
    Result.

finish_mining_callouts(#state{suspended = true}, [_Nonce, _]) ->
    ?EMPTY;
finish_mining_callouts(#state{suspended = false} = S, [_Nonce, ok]) ->
    ?MATCH({HD, ok}, ?CALLOUT(aec_headers, validate, [?VAR], ok)),
    ?APPLY(delta_header_height, []),
    ?REPLICATE(?CALLOUT(aec_keys, verify, [?WILDCARD, ?WILDCARD], true)),
    ?MATCH({BlockMined, ok}, ?CALLOUT(aec_events, publish, [block_created, ?VAR], ok)),
    ?APPLY(on_chain, [S#state.block]),
    ?APPLY(kill_miners, []),
    ?MATCH({Txs1, Txs2, ok}, ?CALLOUT(aec_tx_pool, fork_update, [?VAR, ?VAR], ok)),
    ?APPLY(mine_new, []); %% continue mining anyway
finish_mining_callouts(#state{block = Block, use_pool = Pool}, [_Nonce, {error, no_solution}]) ->
    case Pool of
        true ->
            ?MATCH(Regenerate, ?CALLOUT(aec_mining, need_to_regenerate, [?WILDCARD], bool())),  %% depends on present top as well!
            case Regenerate of
                true ->
                    ?APPLY(mine_new, []);
                false ->
                    ?APPLY(mine_again, [Block])
            end;
        false ->
            ?APPLY(mine_again, [Block])
    end;
finish_mining_callouts(#state{block = Block}, [_Nonce, crash]) ->
    %% if the crash is due to the input then a new post_block will get us out of this state
    ?APPLY(mine_again, [Block]).

finish_mining_next(S, _Value, [Nonce, Result]) ->
    S#state{mining = S#state.mining -- [{Nonce, Result}]}.


finish_mining_features(S, [_Nonce, Result], _Res) ->
    [ {finished_mining_while_suspended, Result} || S#state.suspended ] ++
        [{mining, Result} || not S#state.suspended ].


%% --- Operation: post ---
%% post_args(_S) ->
%%     ?LET(Kind, main, 
%%          ?LET(N, choose(0, length(block_provider:chain(Kind))-1),
%%               begin
%%                   Block = block_provider:chain(Kind, N),
%%                   [{main, N}, aec_blocks:txs(Block)]
%%               end)).

post_pre(#state{block = {main, M}}, [{main, N}, STxs]) ->
    N =< M andalso STxs == aec_blocks:txs(block_provider:chain(main, N)); 
    %% We cannot predict the outcome of mining, but we can mine faster
post_pre(_, [{Kind, N}, STxs]) ->
    STxs == aec_blocks:txs(block_provider:chain(Kind, N)).

post_adapt(_S, [{Kind, Idx}, _]) ->
    Block = block_provider:chain(Kind, Idx),
    [{Kind, Idx}, aec_blocks:txs(Block)].

post({Kind, Idx}, _) ->
    Block = block_provider:chain(Kind, Idx),
    ?SUT:post_block(Block),
    timer:sleep(100).

post_callouts(S, [{Kind, Idx}, STxs]) ->
    case [ {K, I} || {K, I} <- S#state.received ++ S#state.on_chain, K == Kind, I == Idx] of
        [] ->
            ?MATCH({HD, Valid}, ?CALLOUT(aec_headers, validate, [?VAR], elements([ok, {error, invalid_header}]))),
            ?WHEN(Valid == ok,
                  begin
                      case lists:last(S#state.on_chain) == {Kind, Idx - 1} of  %% only main on chain
                          true ->
                              ?APPLY(delta_header_height, []),
                              ?APPLY(received, [{Kind, Idx}]),
                              ?APPLY(verify_transactions, [Idx, STxs, HD, true]);                              
                          false ->
                              ?APPLY(received, [{Kind, Idx}]),  %% only when header validates!
                              %% ?MATCH({Block, ok}, ?CALLOUT(aec_events, publish, [block_received, ?VAR], ok)),
                              ?APPLY(known_block, [{Kind, Idx, STxs, HD}])
                      end
                  end);
        _ ->   %% got existing
            ?EMPTY
    end.

verify_transactions_callouts(S, [Idx, STxs, HD, VerifiedLast]) ->
    ?MATCH_GEN(Verifies, lists:zip([ Tx || {signed_tx, Tx, _Key} <- STxs ],  %% abstraction violation!
                                   [true | vector(length(STxs)-1, true) ])), %% default(true, bool()))])),  %% MASK BUG
    ?PAR([ ?CALLOUT(aec_keys, verify, [?WILDCARD, Tx], Bool) || {Tx, Bool} <- Verifies]),  %% all Txs that don't verify are dropped
    case [ Bool || {Tx, Bool} <- Verifies, element(1, Tx) == coinbase_tx ] of
        [ false ] ->
            ?EMPTY;
        %% ?MATCH({_Block, ok}, ?CALLOUT(aec_events, publish, [top_changed, ?VAR], ok));
        [ true ] ->
            case VerifiedLast orelse length(Verifies) == 1 of  %% and has new transactions
                 true ->
                    ?APPLY(on_chain, [{main, Idx}]),
                    case [ {I, NewTxs, H} || {main, I, NewTxs, H} <- S#state.known_blocks, I == Idx + 1 ] of
                        [] ->
                            ?MATCH({Block, ok}, ?CALLOUT(aec_events, publish, [top_changed, ?VAR], ok)),
                            ?APPLY(add_if_new, [Block, HD]);  
                        [{I, NewTxs, H}] ->
                            ?APPLY(verify_transactions, [I, NewTxs, H, lists:all(fun({_, X}) -> X end, Verifies)])
                    end;
                false -> %% Now we hash of previous block changed
                    ?MATCH({_, ok}, ?CALLOUT(aec_events, publish, [top_changed, ?VAR], ok)),
                    ?EMPTY
            end;
        [] ->
            ?FAIL(verify_transactions)
    end.

consecutive(Idx, [{I, Txs}|Rest]) when I == Idx+1 ->
    Txs ++ consecutive(I, Rest);
consecutive(_, _) ->
    [].





%% --- ... more operations

%% -- Property ---------------------------------------------------------------
%% invariant(_S) ->
%% true.

weight(_S, suspend) -> 1;
weight(S, resume) -> if S#state.suspended -> 5; true -> 1 end;
weight(_S, start) -> 1;
weight(_S, is_mining) -> 1;
weight(_S, post_block) -> 10;
weight(_S, _Cmd) -> 30.

prop_mine() ->
    eqc:dont_print_counterexample(
      ?SETUP(
         fun() ->
                 %% Setup mocking, etc.
                 %% error_logger:tty(false),
                 eqc_mocking:start_mocking(api_spec()),
                 %% Return the teardwown function
                 fun() -> 
                         error_logger:tty(true),
                         ok 
                 end
         end,
         ?FORALL(Cmds, commands(?MODULE),
                 begin
                     BP = block_provider:start(),
                     Run = run_commands(Cmds),
                     block_provider:stop(),
                     FinalS = run_state(Run),
                     NoCrash = FinalS#state.pid == undefined orelse is_process_alive(FinalS#state.pid),
                     Top = (catch aec_blocks:height(aec_conductor:top())), 
                     cleanup(BP),
                     ?WHENFAIL(eqc:format("Top = ~p\n", [Top]),
                                check_command_names(Cmds,
                                measure(length, commands_length(Cmds),
                                aggregate(call_features(run_history(Run)),
                                pretty_commands(?MODULE, Cmds, Run,
                                                conjunction(
                                                  [{result, run_result(Run) == ok}] ++
                                                      [{top, {main, Top} == lists:last(FinalS#state.on_chain)} || FinalS#state.on_chain =/= []] ++
                                                      [{unique_nonces,
                                                        lists:member(suspended_mining, call_features(run_history(Run))) orelse
                                                        FinalS#state.nonces_used -- lists:usort(FinalS#state.nonces_used) ==  []},
                                                       {crash, NoCrash}]))))))
                 end))).

run_result({_H, _S, Res}) ->
    Res.
run_state({_H, S, _Res}) ->
    S.
run_history({H, _S, _Res}) ->
    H.

%% Moet niet zichzelf aanroepen, maar een monitor starten voor de miner en met
%% info een DOWN observeren.
%% De code is op het moment on-testbaar. 

cleanup(BP) ->
    catch ?SUT:stop(),
    catch unregister(block_provider),
    catch exit(BP, kill).

bugs() -> bugs(10).

bugs(N) -> bugs(N, []).

bugs(Time, Bugs) ->
    more_bugs(eqc:testing_time(Time, prop_mine()), 20, Bugs).

%% -- API-spec ---------------------------------------------------------------
api_spec() -> 
    #api_spec{ language = erlang, mocking = eqc_mocking, 
               modules = [ 
                           aec_mining(),
                           aec_events(),
                           aec_chain(),
                           aec_tx_pool(),
                           aec_headers(),
                           aec_keys(),
                           aec_persistence(),
                           aec_block_genesis(),
                           aec_target(),
                           mock()
                         ] }.

aec_mining() ->
    #api_module{ 
       name = aec_mining, fallback = block_provider,
       functions = 
           [ 
             #api_fun{ name = create_block_candidate, arity = 1 },
             #api_fun{ name = need_to_regenerate, arity = 1 },
             %% #api_fun{ name = mine, arity = 2, fallback = true},
             #api_fun{ name = start_miner, arity = 3 }
           ] }.

aec_pow() ->
    #api_module{ name = aec_pow, fallback = aec_pow_mock }.

aec_persistence() ->
    #api_module{ 
       name = aec_persistence,
       functions = 
           [
            #api_fun{ name = get_chain, arity = 0}
           ]
      }.

aec_block_genesis() ->
    #api_module{ 
       name = aec_block_genesis,
       functions = 
           [
            #api_fun{ name = genesis_block, arity = 0},
            #api_fun{ name = populated_trees, arity = 0}
           ]
      }.

aec_target() ->
    #api_module{ 
       name = aec_target,
       functions = 
           [
            #api_fun{ name = determine_delta_header_height, arity = 1}
           ]
      }.
%% {ok, non_neg_integer()} | {error, chain_too_short_to_recalculate_target}.

aec_keys() ->
    #api_module{ 
       name = aec_keys,
       functions = 
           [
            #api_fun{ name = pubkey, arity = 0},
            #api_fun{ name = verify, arity = 2}
           ] }.

aec_headers() ->
    #api_module{ 
       name = aec_headers, fallback = aec_headers,
       functions = 
           [
            #api_fun{ name = validate, arity = 1}
           ] }.

aec_tx_pool() ->
    #api_module{ 
       name = aec_tx_pool,
       functions = 
           [
            #api_fun{ name = fork_update, arity = 2}
           ] }.

aec_events() ->
    #api_module{ 
       name = aec_events, 
       %% fallback = aec_events_mock}.

functions = 
    [
     #api_fun{ name = publish, arity = 2}
    ] }.

aec_chain() ->
    #api_module{ 
       name = aec_chain,
       functions = 
           [
            #api_fun{ name = insert_header, arity = 1},
            #api_fun{ name = top, arity = 0},
            #api_fun{ name = top_block_hash, arity = 0},
            #api_fun{ name = common_ancestor, arity = 2},
            #api_fun{ name = get_block_by_hash, arity = 1},
            #api_fun{ name = get_transactions_between, arity = 2},
            #api_fun{ name = write_block, arity = 1}
           ] }.

mock() ->
    #api_module{ 
       name = mock,
       functions = 
           [
            #api_fun{ name = mine, arity = 2}
           ] }.


