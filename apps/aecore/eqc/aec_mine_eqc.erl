%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 20 Nov 2017 by Thomas Arts <thomas@ThomasComputer.local>

-module(aec_mine_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_mocking.hrl").

-compile([export_all, nowarn_export_all]).

prop_mine() ->
  ?SETUP(fun() ->
             eqc_mocking:start_mocking(api_spec()),
             fun() -> ok end
         end,
  ?FORALL({Sleep, Options}, {500, [{autostart, true}]}, 
  ?FORALL(Lang, lang(Sleep, Options), 
     begin
       application:unset_env(aeccore, autostart),
       application:unset_env(aeccore, fetch_new_txs_from_pool),
       eqc_mocking:init_lang(Lang, api_spec()),
       block_provider:start(),
       {ok, Pid} = aec_miner:start_link(Options),
       unlink(Pid),
       timer:sleep(Sleep),
       Alive = is_process_alive(Pid),
       catch aec_miner:stop(),
       block_provider:stop(),
       conjunction([{crash, Alive},
                    {trace, equals(eqc_mocking:check_callouts(Lang), true)}])
     end))).

%% Generator
lang(_Sleep, _Options) ->
  ?SEQ(
     [ ?EVENT(aec_mining, create_block_candidate, [], {ok, block, ?LET(X, largeint(), abs(X))}),
       ?EVENT(aec_chain, insert_header, [?WILDCARD], ok),
       ?EVENT(aec_chain, top, [], {ok, top}),
       ?EVENT(aec_chain, write_block, [?WILDCARD], ok),
       ?EVENT(aec_events, publish, [block_created, ?WILDCARD], ok),
       %% Now we don't want to compute a lot of stuff and we do as if we do not need to update the top.
       ?EVENT(aec_chain, top, [], {ok, top})  %% but in fact we added a new one
     ]).





api_spec() -> 
  #api_spec{ language = erlang, mocking = eqc_mocking, 
             modules = [ aec_mining(), 
                         aec_keys(),  %% for the chain generator
                         aec_events(),
                         aec_chain(),
                         mock()
                       ] }.

aec_mining() ->
  #api_module{ 
     name = aec_mining, fallback = block_provider,
     functions = 
       [ 
         #api_fun{ name = create_block_candidate, arity = 0 },
         #api_fun{ name = mine, arity = 2, fallback = true},
         #api_fun{ name = start_miner, arity = 3 }
       ] }.

aec_keys() ->
  #api_module{ 
       name = aec_keys,
       functions = 
           [
            #api_fun{ name = verify, arity = 2}
           ] }.

aec_events() ->
  #api_module{ 
       name = aec_events,
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
            #api_fun{ name = write_block, arity = 1}
           ] }.

mock() ->
  #api_module{ 
       name = mock,
       functions = 
           [
            #api_fun{ name = mine, arity = 2}
           ] }.


%%%% a chain generator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% aec_keys should be mocked to return true.
prop_chain() ->
   ?SETUP(fun() ->
             eqc_mocking:start_mocking(api_spec()),
             fun() -> eqc_mocking:stop_mocking() end
          end,
  ?FORALL({Chain, Trace}, noshrink(chain()), conjunction([{list, is_list(Chain)},
                                                {trace, length(Trace) < 20 }]))).


%% aec_keys should be mocked to return true.
%% Returns a chain and the expected verifications
chain() ->
  CryptoMap =
    #{curve => secp256k1, algo => ecdsa, digest => sha256, type => ecdh},
  KeyPairs = 
    sign_eqc:keypairs(4, CryptoMap),
  ?LET(N, nat(),
       begin
         eqc_mocking:init_lang(verify_lang(N), api_spec()),
         Chain = chain(N, KeyPairs, CryptoMap, [aec_block_genesis:genesis_block()]),
         {Chain, eqc_mocking:get_trace(api_spec())}
       end).

verify_lang(N) ->
  ?REPLICATE(?EVENT(aec_keys, verify, [?WILDCARD, ?WILDCARD], true)).

chain(0, KeyPairs, CryptoMap, Chain) ->
  Chain;
chain(N, KeyPairs, CryptoMap, Chain) ->
  ?LET(Txs, [ tx(KeyPairs, coinbase) | list(tx(KeyPairs, spend))],
       begin
         SignedTxs = [ aec_tx_sign:new(Tx, Privkey, CryptoMap) ||
                       {Privkey, Tx} <- Txs ],
         io:format("Block: ~p Txs = ~p\n", [length(Chain), Txs]),
         Block = aec_blocks:new(lists:last(Chain), SignedTxs),
         chain(N - 1, KeyPairs, CryptoMap, Chain ++ [ Block ])
       end).
  
tx(Keys, spend) ->
  ?LET([From, To | _], shuffle(Keys),
       {maps:get(private, From), sign_eqc:spend_tx(maps:get(public, From), maps:get(public, To))});
tx(Keys, coinbase) ->
  ?LET(From, elements(Keys),
       {maps:get(private, From), sign_eqc:coinbase_tx(maps:get(public, From))}).



