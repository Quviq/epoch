%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%% common_ancestor/2,
%%          get_block_by_hash/1,
%%          get_block_by_height/1,
%%          get_header_by_hash/1,
%%          get_header_by_height/1,
%%          get_total_difficulty/0,
%%          get_transactions_between/2,
%%          insert_header/1,
%%          top/0,
%%          top_header/0,
%%          write_block/1
%% %
%% @end
%%% Created : 14 Nov 2017 by Thomas Arts <thomas@ThomasComputer.local>

-module(aec_chain_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-compile([export_all, nowarn_export_all]).

-record(eblock, {index, block}).


%% -- State ------------------------------------------------------------------
initial_state() ->
  undefined.

command_precondition_common(S, start) ->
  S == undefined;
command_precondition_common(S, _) ->
  S =/= undefined.



%% -- Operations -------------------------------------------------------------

%% --- Operation: start ---
start_args(_S) ->
  %% Should generate a random block ??
  [aec_block_genesis:genesis_block()].

start(Block) ->
  {ok, Pid} = aec_chain:start_link(Block),
  unlink(Pid),
  ok.

start_next(_Chain, _, [Block]) ->
  add([], Block).


%% --- Operation: stop ---
stop_args(_S) ->
  [].

stop() ->
  aec_chain_server:stop().

stop_next(_S, _Value, []) ->
  initial_state().

%% --- Operation: top --

top_args(_S) ->
  [].

top() ->
  {ok, Block} = aec_chain:top(),
  Block.

top_post(Chain, [], Res) ->
  #eblock{ block = B} = lists:last(Chain),
  eq(Res, B).


%% --- Operation: top_header ---
top_header_args(_Chain) ->
  [].

top_header() ->
  {ok, Header} = aec_chain:top_header(),
  Header.

top_header_post(Chain, [], Res) ->
  #eblock{ block = B} = lists:last(Chain),
  eq(Res, aec_blocks:to_header(B)).


%% --- Operation: get_transactions_between ---
get_transactions_between_args(Chain) ->
  [elements(Chain),  elements(Chain)].

get_transactions_between_pre(_S, [#eblock{index = End}, #eblock{index = Start}]) ->
  Start =< End.

get_transactions_between(#eblock{block = BEnd}, #eblock{block = BStart}) ->
  Hash1 = hash_of_block(BEnd),
  Hash2 = hash_of_block(BStart),
  {ok, Txs} = aec_chain:get_transactions_between(Hash1, Hash2),
  Txs.

get_transactions_between_post(Chain, [#eblock{index = End}, #eblock{index = Start}], Res) ->
  %% This works, because block index is one lower than position in list
  {_, Pref} = lists:split(Start + 1, Chain),
  {Blocks, _} = lists:split(End - Start, Pref),
  eq(Res, lists:append([ aec_blocks:txs(B#eblock.block) || B <- Blocks ])).


%% --- Operation: get_total_difficulty ---
get_total_difficulty_args(_Chain) ->
  [].

get_total_difficulty() ->
  aec_chain:get_total_difficulty().

get_total_difficulty_post(Chain, [], Res) ->
  case Res of
    {ok, {N, _}} ->
      N >= 1.0 * length(Chain);
    _ ->
      eq(Res, error)
  end.




%% --- Operation: get_block_by_height ---

get_block_by_height_args(Chain) ->
  [choose(0, length(Chain) + 2)].

get_block_by_height(H) ->
  aec_chain:get_block_by_height(H).

get_block_by_height_post(Chain, [H], Res) ->
  case Res of
    {ok, Block} ->
      #eblock{index = I, block = B} = lists:nth(H+1, Chain),
      eqc_statem:conj([eq(H, I), eq(Block, B)]);
    {error, {chain_too_short, _}} ->
      H >= length(Chain)
      %% Do we need to check the error here?
      %% It contains the length of the chain and the top_header, but is that part of API?
  end.


%% --- Operation: get_block_by_hash ---
get_block_by_hash_args(Chain) ->
  [oneof([?LET(B, elements(Chain), {call, ?MODULE, hash_of_block, [B#eblock.block]}),
          binary()])].

get_block_by_hash(Hash) ->
  aec_chain:get_block_by_hash(Hash).

get_block_by_hash_post(Chain, [Hash], Res) ->
  Found = [ B || B <- Chain, hash_of_block(B#eblock.block) == Hash],
  case {Res, Found} of
    {{ok, Block}, [#eblock{block = B}]} ->
      eq(Block, B);
    {{error, {block_not_found, _}}, []} ->
      true;
    _ -> false
  end.

%% --- Operation: get_header_by_hash ---
%% Hash of block works for headers as well (ref. Tobias)
get_header_by_hash_args(Chain) ->
  [oneof([?LET(B, elements(Chain), {call, ?MODULE, hash_of_block, [B#eblock.block]}),
          binary()])].

get_header_by_hash(Hash) ->
  aec_chain:get_header_by_hash(Hash).

get_header_by_hash_post(Chain, [Hash], Res) ->
  Found = [ B || B <- Chain, hash_of_block(B#eblock.block) == Hash],
  case {Res, Found} of
    {{ok, Header}, [#eblock{block = B}]} ->
      eqc_statem:conj([eq(Header, aec_blocks:to_header(B))]);
    {{error, {header_not_found, _}}, []} ->
      true;
    _ ->
      false
  end.

%% --- Operation: write_block ---
%% - Cannot build a tree from empty list of transactions
%% - If no coinbase transaction present, then write_block crashes
%% - If coinbase transaction is not first transaction, then crashes
%%   exit({function_clause,
%%          [{aec_tx, coinbase_tx_account_pubkey,
write_block_args(Chain) ->
  Params = #{curve => secp256k1, algo => ecdsa, digest => sha256, type => ecdh},
  KeyPairs = sign_eqc:keypairs(4, Params),
  [ lists:last(Chain), [tx(KeyPairs, coinbase)| list(tx(KeyPairs, spend))], Params, true ]. %% make non_empty(list(...))) and bool()

write_block(#eblock{ block = LastBlock }, Txs, CryptoMap, _) ->
  SignedTxs = [ aec_tx_sign:new(Tx, Privkey, CryptoMap) || {Privkey, Tx} <- Txs ],
  Block = aec_blocks:new(LastBlock, SignedTxs), %% Here I call verify!!
  Res = aec_chain:write_block(Block),
  {Res, Block}.

write_block_callouts(Chain, [#eblock{ index = I }, Txs, _, Verify]) ->
  ?SEQ([ ?CALLOUT(aec_keys, verify, [?WILDCARD, Tx], true) || {_, Tx} <- Txs ]), %% creating the block
  ?SEQ([ ?CALLOUT(aec_keys, verify, [?WILDCARD, Tx], Verify) || {_, Tx} <- Txs ]), %% in write_block
  ?WHEN(length(Chain) rem 10 == 0, %% Checkpoints ??? Need specification.
        ?SEQ([ ?CALLOUT(aec_keys, verify, [?WILDCARD, Tx], Verify) || {_, Tx} <- Txs ])).  
  %% as long as verify is true


write_block_next(Chain, Value, [#eblock{ index = I }, _, _, Verify]) ->
  case Verify andalso (I == length(Chain) - 1) of
    false -> 
      Chain;
    true ->
      add(Chain, {call, erlang, element, [2, Value]})
  end.


write_block_post(Chain, [#eblock{ index = I }, _, _, Verify], {Res, _Block}) ->
  case Res of
    ok -> 
      eqc_statem:conj([eq(I, length(Chain) - 1), eqc_statem:tag(verify, Verify)]);
    {error, {root_hash_mismatch, _, _}} -> 
      eqc_statem:conj([eq(I, length(Chain) - 1), eqc_statem:tag(verify, not Verify)]);
    {error, height_inconsistent_with_previous_hash} ->
      not(I == length(Chain) - 1)

%% {aec_tx, coinbase_tx_account_pubkey,  Need exactly one Coninbase in each Txs
  end.

write_block_features(_S, [_, _, _, _], {Res, _}) ->
  [ case Res of
      ok -> ok;
      {error, {root_hash_mismatch, _, _}} -> root_hash_mismatch;
      {error, height_inconsistent_with_previous_hash} -> height_inconsistent_with_previous_hash;
      Other -> Other
    end ].


add_next(Chain, _, [Block]) ->
  add(Chain, Block).

add(Chain, Block) ->
  Chain ++ [#eblock{index = length(Chain), block = Block}].

tx(Keys, spend) ->
  ?LET([From, To | _], shuffle(Keys),
       {maps:get(private, From), sign_eqc:spend_tx(maps:get(public, From), maps:get(public, To))});
tx(Keys, coinbase) ->
  ?LET(From, elements(Keys),
       {maps:get(private, From), sign_eqc:coinbase_tx(maps:get(public, From))}).


%% --- ... more operations

%% This function should be in aec_blocks, not in aec_sync!
hash_of_block(Block) ->
  {ok, Hash} = aec_blocks:hash_internal_representation(Block),
  Hash.

%% -- Property ---------------------------------------------------------------
%% invariant(_S) ->
%% true.

weight(_S, write_block) -> 10;
weight(_S, _Cmd) -> 1.

prop_aec_chain() ->
  ?SETUP(
    fun() ->
        %% Setup mocking, etc.
        cleanup(),
        eqc_mocking:start_mocking(api_spec()),
        %% Return the teardwown function
        fun() -> ok end
    end,
  ?FORALL(Cmds, commands(?MODULE),
  begin
    {H, S, Res} = run_commands(Cmds),
    cleanup(),
    check_command_names(Cmds,
      measure(length, commands_length(Cmds),
        pretty_commands(?MODULE, Cmds, {H, S, Res},
                        Res == ok)))
  end)).

cleanup() ->
  catch stop().

bugs() -> bugs(10).

bugs(N) -> bugs(N, []).

bugs(Time, Bugs) ->
  more_bugs(eqc:testing_time(Time, prop_aec_chain()), 20, Bugs).

%% -- API-spec ---------------------------------------------------------------
api_spec() -> 
  #api_spec{ language = erlang, mocking = eqc_mocking, 
             modules = [aec_keys(), aec_persistence()] }.

aec_keys() ->
  #api_module{ 
       name = aec_keys,
       functions = 
           [
            #api_fun{ name = verify, arity = 2},
            #api_fun{ name = pubkey, arity = 0}
           ] }.

aec_persistence() ->
  #api_module{ 
       name = aec_persistence,
       functions = 
           [
            #api_fun{ name = pow_module, arity = 0}
           ] }.

