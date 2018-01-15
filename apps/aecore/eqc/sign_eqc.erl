%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc Generator for signed transactions
%%%
%%% @end
%%% Created : 15 Nov 2017 by Thomas Arts

-module(sign_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).


%% Use two Key pairs
spend_tx(SenderPubkey, RecipientPubkey) ->
  ?LET({Amount, Fee, Nonce}, {nat(), nat(), nat()},
       begin
         {ok, Tx} = aec_spend_tx:new(#{sender => SenderPubkey,
                                       recipient => RecipientPubkey,
                                       amount => Amount,
                                       fee => Fee,
                                       nonce => Nonce}),
         return(Tx)
       end).

coinbase_tx(Pubkey) ->
  {ok, Tx} = aec_coinbase_tx:new(#{account => Pubkey}),
  return(Tx).

%% Use two Key pairs
signed_tx(#{public := SenderPubkey, private := SenderPrivkey}, #{public := RecipientPubkey}, CryptoMap) ->
  ?LET(Tx, oneof([spend_tx(SenderPubkey, RecipientPubkey), coinbase_tx(SenderPubkey)]),
       aec_tx_sign:sign(Tx, SenderPrivkey, CryptoMap)).

keypairs(N, CryptoMap) ->
  [ keypair(CryptoMap) || _ <- lists:seq(1, N) ].

keypair(#{curve := Curve, type := Type}) ->
  {Pub, Priv} = crypto:generate_key(Type, crypto:ec_curve(Curve)),
  #{public => Pub, private => Priv}.

prop_verify_signed_tx(KeyPairs, #{curve := Curve, algo := Algo, digest := Digest} = CryptoMap) ->
  ?FORALL([#{public := PubKey} = Sender, Receiver], KeyPairs,
    ?FORALL(SignedTx, signed_tx(Sender, Receiver, CryptoMap),
            begin
              [Signature] = aec_tx_sign:signatures(SignedTx),
              Data = aec_tx_sign:data(SignedTx),
              Bin = aec_tx:serialize_to_binary(Data),
              crypto:verify(
                Algo, Digest, Bin, Signature,  [PubKey, crypto:ec_curve(Curve)])
            end)).

prop_verify_signed_tx() ->
  Params = #{curve => secp256k1, algo => ecdsa, digest => sha256, type => ecdh},
  prop_verify_signed_tx(keypairs(2, Params), Params).

test() ->
  Params = #{curve => secp256k1, algo => ecdsa, digest => sha256, type => ecdh},
  eqc:quickcheck(prop_verify_signed_tx(keypairs(2, Params), Params)).

