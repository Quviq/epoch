%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Translator from Aering Icode to Aevm Assebly
%%% @end
%%% Created : 21 Dec 2017
%%% 
%%%-------------------------------------------------------------------
-module(aer_icode_to_asm).

-export([convert/2]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aer_icode.hrl").

convert(#{ contract_name := _ContractName
         , functions := Functions
              },
        _Options) ->
    %% Create a function environment
    Funs = [{Name, length(Args), make_ref()}
    	    || {Name, Args, _Body} <- Functions],
    %% Create dummy code to call the main function with one argument
    %% taken from the stack
    StopLabel = make_ref(),
    MainFunction = lookup_fun(Funs,"main",1),
    DummyCode = [%% push a return address to stop
		 {push_label,StopLabel},
		 %% swap argument to the top of the stack
		 ?SWAP1,
		 {push_label,MainFunction},
		 ?JUMP,
		 {?JUMPDEST,StopLabel},
		 ?STOP
		],
   
    %% Code is a deep list of instructions, containing labels and
    %% references to them.
    Code = [assemble_function(Funs,Name,Args,Body) 
	    || {Name,Args,Body} <- Functions],
    resolve_references(
        [%% aeb_opcodes:mnemonic(?COMMENT), "CONTRACT: " ++ ContractName,
	 DummyCode,
	 Code]).

assemble_function(Funs,Name,Args,Body) ->
    [{?JUMPDEST,lookup_fun(Funs,Name,length(Args))},
     assemble_expr(Funs, lists:reverse(Args), Body),
     %% swap return value and first argument
     [swap(length(Args)) || Args/=[]],
     [?POP || _ <- Args],
     swap(1),
     ?JUMP].

assemble_expr(_Funs,Stack,{var_ref,Id}) ->
    dup(lookup_var(Id,Stack));
assemble_expr(_Funs,_Stack,{integer,N}) ->
    push(N);
assemble_expr(Funs,Stack,{binop,'&&',A,B}) ->
    assemble_expr(Funs,Stack,{ifte,A,B,{integer,0}});
assemble_expr(Funs,Stack,{binop,'||',A,B}) ->
    assemble_expr(Funs,Stack,{ifte,A,{integer,1},B});
assemble_expr(Funs,Stack,{binop,Op,A,B}) ->
    %% EEVM binary instructions take their first argument from the top
    %% of the stack, so to get operands on the stack in the right
    %% order, we evaluate from right to left.
    [assemble_expr(Funs,Stack,B),
     assemble_expr(Funs,[dummy|Stack],A),
     assemble_infix(Op)];
assemble_expr(Funs,Stack,{ifte,Decision,Then,Else}) ->
    Close = make_ref(),
    ThenL  = make_ref(),
    ElseL  = make_ref(),
    [assemble_decision(Funs,Stack,Decision,ThenL,ElseL),
     {?JUMPDEST,ElseL},
     assemble_expr(Funs,Stack,Else),
     {push_label,Close},
     ?JUMP,
     {?JUMPDEST,ThenL},
     assemble_expr(Funs,Stack,Then),
     {?JUMPDEST,Close}
    ].

assemble_decision(Funs,Stack,{binop,'&&',A,B},Then,Else) ->
    Label = make_ref(),
    [assemble_decision(Funs,Stack,A,Label,Else),
     {?JUMPDEST,Label},
     assemble_decision(Funs,Stack,B,Then,Else)];
assemble_decision(Funs,Stack,{binop,'||',A,B},Then,Else) ->
    Label = make_ref(),
    [assemble_decision(Funs,Stack,A,Then,Label),
     {?JUMPDEST,Label},
     assemble_decision(Funs,Stack,B,Then,Else)];
assemble_decision(Funs,Stack,Decision,Then,Else) ->
    [assemble_expr(Funs,Stack,Decision),
     {push_label,Then}, ?JUMPI,
     {push_label,Else}, ?JUMP].

assemble_infix('+') -> ?ADD;
assemble_infix('-') -> ?SUB;
assemble_infix('*') -> ?MUL;
assemble_infix('/') -> ?SDIV;
assemble_infix('bor') -> ?OR;
assemble_infix('band') -> ?AND;
assemble_infix('bxor') -> ?XOR;
assemble_infix('<') -> ?SLT;    %% comparisons are SIGNED
assemble_infix('>') -> ?SGT;
assemble_infix('==') -> ?EQ;
assemble_infix('<=') -> [?SGT,?ISZERO];
assemble_infix('>=') -> [?SLT,?ISZERO];
assemble_infix('!=') -> [?EQ,?ISZERO].

lookup_fun(Funs,Name,Arity) ->
    case [Ref || {Name1,Arity1,Ref} <- Funs,
		 {Name,Arity} == {Name1,Arity1}] of
	[Ref] ->
	    Ref;
	[] ->
	    error({undefined_function,Name,Arity})
    end.

lookup_var(Id,Stack) ->
    lookup_var(1,Id,Stack).

lookup_var(N,Id,[{Id,_Type}|_]) ->
    N;
lookup_var(N,Id,[_|Stack]) ->
    lookup_var(N+1,Id,Stack);
lookup_var(_,Id,[]) ->
    error({var_not_in_scope,Id}).

%% Smart instruction generation

dup(N) when N=<16 ->
    ?DUP1 + N-1.

push(N) ->
    Bytes = binary:encode_unsigned(N),
    true = size(Bytes) =< 32,
    [aeb_opcodes:mnemonic(?PUSH1 + size(Bytes)-1) |
     binary_to_list(Bytes)].
 
swap(N) when N=<16 ->
    ?SWAP1 + N-1.

%% Resolve references, and convert code from deep list to flat list.
%% List elements are:
%%   Opcodes
%%   Byte values
%%   {?JUMPDEST,Ref}   -- assembles to ?JUMPDEST and sets Ref
%%   {push_label,Ref}  -- assembles to ?PUSHN address bytes

%% For now, we assemble all code addresses as three bytes.

resolve_references(Code) ->
    Instrs = lists:flatten(Code),
    Labels = define_labels(0,Instrs),
    lists:flatten([use_labels(Labels,I) || I <- Instrs]).

define_labels(Addr,[{?JUMPDEST,Lab}|More]) ->
    [{Lab,Addr}|define_labels(Addr+1,More)];
define_labels(Addr,[{push_label,_}|More]) ->
    define_labels(Addr+4,More);
define_labels(Addr,[_|More]) ->
    define_labels(Addr+1,More);
define_labels(_,[]) ->
    [].

use_labels(_,{?JUMPDEST,_}) ->
    ?JUMPDEST;
use_labels(Labels,{push_label,Ref}) ->
    case proplists:get_value(Ref,Labels) of
	undefined ->
	    error({undefined_label,Ref});
	Addr when is_integer(Addr) ->
	    [?PUSH3,Addr div 65536,(Addr div 256) rem 256, Addr rem 256]
    end;
use_labels(_,I) ->
    I.


