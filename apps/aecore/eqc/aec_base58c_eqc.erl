%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2018, Thomas Arts
%%% @doc QuickCheck property for aec_base58c.erl
%%%
%%% @end
%%% Created : 14 Jan 2018 by Thomas Arts <thomas@ThomasComputer.lan>

-module(aec_base58c_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

type() ->
  elements([ block_hash
           , block_tx_hash
           , block_state_hash
           , transaction
           , oracle_pubkey
           , account_pubkey ]).

prop_encode_safe_decode() ->
  ?FORALL({T1, T2, B}, {type(), type(), binary()},
          case aec_base58c:safe_decode(T1, aec_base58c:encode(T2, B)) of
            {ok, B} -> T1 =:= T2;
            {error, _} -> T1 =/= T2
          end).

prop_encode_decode() ->
  ?FORALL({T, B}, {type(), binary()},
          equals(aec_base58c:decode(aec_base58c:encode(T, B)), 
                 {T, B})).

