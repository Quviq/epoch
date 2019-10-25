%%
%% Running several lottery contracts
%%
%%
-module(lottery_SUITE).

%% common_test exports
-export(
   [ all/0, groups/0, init_per_suite/1, end_per_suite/1,
     init_per_testcase/2, end_per_testcase/2]).

%% test case exports
-export(
   [ create_lottery1_test/1,
     lottery1_negative_create_test/1
   ]).

-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").


-define(assertReturn(Vm, Result, Expected),
        case Vm of
            aevm -> ok;
            fate -> ?assertEqual(Expected, maps:get(return, Result, no_return))
        end).

all() ->
    [ create_lottery1_test,
      lottery1_negative_create_test
    ].

groups() ->
    [ {all_tests, [sequence],
        [ {group, lottery1}
        ]}
    , {lottery1, [sequence],
        [ create_lottery1_test, lottery1_negative_create_test ]}
    ].

init_per_suite(Cfg) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN -> {skip, no_fate_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, no_fate_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, no_fate_in_fortuna};
        ?LIMA_PROTOCOL_VSN -> [ {vm, fate}, {protocol, lima} | Cfg ];
        ?IRIS_PROTOCOL_VSN -> [ {vm, fate}, {protocol, iris} | Cfg ]
    end.

end_per_suite(_Cfg) ->
    ok.

init_per_testcase(TC, Config) ->
    meck:new(aefa_chain_api, [passthrough]),
    meck:expect(aefa_chain_api, blockhash,
                fun(N, S) when is_integer(N) ->
                        %% Just to ensure the arg format
                        _ = aefa_chain_api:generation(S),
                        aeb_fate_data:make_hash(<<N:256>>)
                end),
    Config.

end_per_testcase(TC, Config) ->
    meck:unload(aefa_chain_api),
    ok.

create_lottery1_test(Cfg) ->
    Name = "lottery1.aes",
    VM = proplists:get_value(vm, Cfg),
    Compiled = compile(Name, VM),
    Blocks = 4,

    {Trees, Env}   = init(),
    ct:pal("Mining init ~p", [Env]),

    CallData = mk_calldata(#{backend => VM, code => Compiled, function => "init", args => []}),
    {Trees1, Env1} = create_contract(Trees, Env, #{backend => VM, code => Compiled, sender => 1,
                                                   call_data => CallData}),
    ContractId = id(1,1),
    CallTx =  #{backend => VM, code => Compiled, sender => 1, contract_id => aeser_id:create(contract, ContractId)},
    ct:log("Account ~p now ~p (total cost: ~p)", [1, balance(Trees1, 1),
                                                  balance_dec(Trees, Trees1, 1)]),

    {Trees2, Env2, [Call1]} = call_contract(Trees1, Env1, CallTx#{function => "start", args => ["2"]}),
    ?assertReturn(VM, Call1, {tuple,{}}),

    {Trees3, Env3, [Call2]} = call_contract(Trees2, Env2,  CallTx#{function => "get_state", args => []}),
    ?assertReturn(VM, Call2, {tuple,{[], 0, 3}}),

    %% Can start again! As long as nobody has bought a ticket (race with TX pool?)
    %% Extend lottery to Blocks in future
    {Trees4, Env4, [Call5]} = call_contract(Trees3, Env3, CallTx#{function => "start", args => [integer_to_list(Blocks)]}),
    ?assertReturn(VM, Call5, {tuple,{}}),

    {_, _, [Call6]} = call_contract(Trees4, Env4, CallTx#{function => "get_state", args => []}),
    ?assertReturn(VM, Call6, {tuple,{[], 0, Blocks + 1}}),

    {Trees5, _Env5, [Call7]} = call_contract(Trees4, Env4, CallTx#{sender => 2, amount => 10, function => "buy", args => []}),
    ?assertReturn(VM, Call7, Blocks + 1),

    {Trees6, Env6} = mine(1, Trees5),

    {Trees7, Env7, [Call8]} = call_contract(Trees6, Env6, CallTx#{sender => 3, amount => 10, function => "buy", args => []}),
    ?assertReturn(VM, Call8, Blocks + 1),
    {Trees8, Env8, [Call9]} = call_contract(Trees7, Env7, CallTx#{sender => 4, amount => 10, function => "buy", args => []}),
    ?assertReturn(VM, Call9, Blocks + 1),
    {Trees9, Env9, [Call10]} = call_contract(Trees8, Env8, CallTx#{sender => 1, function => "get_state", args => []}),

    {Trees10, Env10} = lists:foldl(fun(H, {Ts, E}) ->
                                           mine(H, Ts) end,
                                   {Trees9, Env9}, lists:seq(2, Blocks)),
    ct:pal("Env ~p", [Env10]),

    {_, _, [Call11]} = call_contract(Trees10, Env10, CallTx#{sender => 4, amount => 10, function => "buy", args => []}),
    ?assertReturn(VM, Call11, {revert, <<"lottery closed">>}),

    case proplists:get_value(protocol, Cfg) of
        lima ->
            {_, _, [Call11a]} = call_contract(Trees10, Env10, CallTx#{sender => 2, function => "claim", args => []}),
            ?assertReturn(VM, Call11a, {revert, <<"result not yet known">>});
        iris ->
            ok
    end,

    {Trees11, Env11} = mine(Blocks + 1, Trees10),
    ct:pal("Env ~p", [Env11]),

    {Trees12, Env12, [Call12]} = call_contract(Trees11, Env11, CallTx#{sender => 2, function => "claim", args => []}),
    ?assertReturn(VM, Call12,  {variant, [0,1], 1,
                                {{address, <<2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}}}),

    ok.


lottery1_negative_create_test(Cfg) ->
    Name = "lottery1.aes",
    VM = proplists:get_value(vm, Cfg),
    Compiled = compile(Name, VM),

    {Trees, Env}   = init(),
    CallData = mk_calldata(#{backend => VM, code => Compiled, function => "init", args => []}),
    {Trees1, _Env1} = create_contract(Trees, Env, #{backend => VM, code => Compiled, sender => 1,
                                                   call_data => CallData}),
    ContractId = id(1,1),
    CallTx =  #{backend => VM, code => Compiled, sender => 1, contract_id => aeser_id:create(contract, ContractId)},

    %% Buying a ticket before start fails
    {_, _, [Call0]} = call_contract(Trees1, Env, CallTx#{sender => 2, function => "buy", args => []}),
    ?assertReturn(VM, Call0, {revert, <<"lottery closed">>}),

    {_, _, [Call1]} = call_contract(Trees1, Env, CallTx#{sender => 2, function => "start", args => ["2"]}),
    ?assertReturn(VM, Call1, {revert, <<"not creator">>}),

    {Trees2, _, [Call2]} = call_contract(Trees1, Env, CallTx#{function => "start", args => ["2"]}),
    ?assertReturn(VM, Call2, {tuple, {}}),

    {_, _, [Call3]} = call_contract(Trees1, Env, CallTx#{function => "start", args => ["0"]}),
    ?assertReturn(VM, Call3, {revert, <<"block in future">>}),

    %% Try to buy and start again
    {_, _, [Call4]} = call_contract(Trees2, Env, CallTx#{sender => 2, amount => 5, function => "buy", args => []}),
    ?assertReturn(VM, Call4, {revert, <<"price ticket 10">>}),

    {Trees3, _, [Call5]} = call_contract(Trees2, Env, CallTx#{sender => 2, amount => 10, function => "buy", args => []}),
    ?assertReturn(VM, Call5, 3),

    {_, _, [Call6]} = call_contract(Trees3, Env, CallTx#{function => "start", args => ["2"]}),
    ?assertReturn(VM, Call6, {revert, <<"lottery ongoing">>}),

    ok.




create_contract(Trees, Env, #{backend := Backend, code := Code, sender := Sender} = Map) ->
    Account = account(Sender),
    Tx =
        maps:merge(#{owner_id => aeser_id:create(account, Account),
                     vm_version  => case Backend of
                                        aevm -> aect_test_utils:latest_sophia_vm_version();
                                        fate -> ?VM_FATE_SOPHIA_1
                                    end,
                     abi_version => case Backend of aevm -> 1; fate -> 3 end,
                     fee => 100000 * 1500000 * 20,
                     gas_price => 1000000,
                     gas => 20000000,
                     nonce => nonce(Trees, Account),
                     deposit => 0,
                     amount => 0,
                     code => aect_sophia:serialize(Code, aect_test_utils:latest_sophia_contract_version())
                    }, maps:without([backend, code, sender], Map)),
    {ok, AeTx} = aect_create_tx:new(Tx),
    {ok, Trees1, Env1} =  aetx:process(AeTx, Trees, Env),

    NewCalls = calls_produced(Trees, Trees1, Backend),
    ct:pal("~p calls init ~p", [Backend, NewCalls]),
    {Trees1, Env1}.


mk_calldata(#{backend := Backend, code := Code, function := Fun, args := Args}) ->
    #{contract_source := Contract} =  Code,
    {ok, CallData} = encode_call_data(Contract, Fun, Args, Backend),
    CallData.

call_contract(Trees, Env, #{backend := Backend, sender := Sender, function := Fun, args := Args} = Map) ->
    Account = account(Sender),
    CallData = mk_calldata(Map),

    Tx =
        maps:merge(#{caller_id => aeser_id:create(account, Account),
                     abi_version => case Backend of aevm -> ?ABI_AEVM_SOPHIA_1; fate -> ?ABI_FATE_SOPHIA_1 end,
                     fee =>  500000 * 1000000 * 20,
                     gas_price => 1000000,
                     gas => 6000000,
                     nonce => nonce(Trees, Account),
                     amount => 0,
                     call_data => CallData}, maps:without([backend, code, sender, function, args], Map)),
    {ok, AeTx} = aect_call_tx:new(Tx),
    {ok, Trees2, Env2} = aetx:process(AeTx, Trees, Env),

    NewCalls = calls_produced(Trees, Trees2, Backend),
    ct:pal("~p call ~p ~p ~p", [Backend, Fun, Args, NewCalls]),

    {Trees2, Env2, NewCalls}.


compile(File, VM) ->
    CodeDir = filename:join(code:lib_dir(aecore), "../../extras/test/contracts"),
    FileName = filename:join(CodeDir, File),
    {ok, Cwd} = file:get_cwd(),
    {ok, Compiled} =  aeso_compiler:file(FileName, [{backend, VM}, {include, {file_system, [Cwd, CodeDir]}}]),
    ct:pal("Size ~p: ~p", [VM, byte_size(aect_sophia:serialize(Compiled, aect_test_utils:latest_sophia_contract_version()))]),
    Compiled.

init() ->
    init([{account, account(N), 20000000000000000} || N<-lists:seq(1,9)]).

init(Accounts) ->
    mine(0, trees_with_accounts(Accounts)).

%% Mine 1 at height Height
mine(Height, Trees) ->
    NextHeight = Height + 1,
    TxEnv = aetx_env:tx_env(NextHeight),
    Protocol = aec_hard_forks:protocol_effective_at_height(Height),
    Trees1 = aec_trees:perform_pre_transformations(Trees, TxEnv, Protocol),
    {Trees1, TxEnv}.

encode_call_data(Code, Fun, Args, Backend) ->
    aeso_compiler:create_calldata(Code, Fun, Args, [{backend, Backend}]).

account(N) ->
    <<N,0:248>>.

nonce(Trees, Sender) ->
    {value, Account} = aec_accounts_trees:lookup(Sender, aec_trees:accounts(Trees)),
    aec_accounts:nonce(Account) + 1.

%% Contract Id from account and nonce
id(Account, Nonce) ->
    aect_contracts:compute_contract_pubkey(account(Account), Nonce).


balance(Trees, N) ->
    {value, Account} = aec_accounts_trees:lookup(account(N), aec_trees:accounts(Trees)),
    aec_accounts:balance(Account).

%% We call it decrease, because we assume spending between Trees1 and Trees2
balance_dec(Trees1, Trees2, N) ->
    balance(Trees1, N) - balance(Trees2, N).

%% calls new in Trees2 not yet in Trees1
calls_produced(Trees1, Trees2, Backend) ->
    lists:usort([#{gas => aect_call:gas_used(Call),
                   return => case Backend of
                                 aevm -> aect_call:return_value(Call);
                                 fate ->
                                     case {aect_call:return_type(Call), aect_call:return_value(Call)} of
                                         {ok, Bin} when Bin =/= <<>> -> aeb_fate_encoding:deserialize(Bin);
                                         {revert, Bin} when Bin =/= <<>> -> {revert, aeb_fate_encoding:deserialize(Bin)};
                                         {_, Bin} -> Bin
                                     end
                             end } || {_, Call} <- aect_call_state_tree:to_list(aec_trees:calls(Trees2)) --
                                                                  aect_call_state_tree:to_list(aec_trees:calls(Trees1))]).

trees_with_accounts(Accounts) ->
    trees_with_accounts(Accounts, aec_trees:new_without_backend()).

trees_with_accounts([], Trees) ->
    Trees;
trees_with_accounts([{account, Acc, Amount}|Rest], Trees) ->
    Account = aec_accounts:new(Acc, Amount),
    AccountTrees = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    trees_with_accounts(Rest, aec_trees:set_accounts(Trees, AccountTrees)).
