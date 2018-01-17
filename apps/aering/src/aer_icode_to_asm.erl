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
		 aeb_opcodes:mnemonic(?SWAP1),
		 {push_label,MainFunction},
		 aeb_opcodes:mnemonic(?JUMP),
		 {aeb_opcodes:mnemonic(?JUMPDEST),StopLabel},
		 aeb_opcodes:mnemonic(?STOP)
		],
   
    %% Code is a deep list of instructions, containing labels and
    %% references to them. Labels take the form {'JUMPDEST',Ref}, and
    %% references take the form {push_label,Ref}, which is translated
    %% into a PUSH instruction.
    Code = [assemble_function(Funs,Name,Args,Body) 
	    || {Name,Args,Body} <- Functions],
    resolve_references(
        [%% aeb_opcodes:mnemonic(?COMMENT), "CONTRACT: " ++ ContractName,
	 DummyCode,
	 Code]).

assemble_function(Funs,Name,Args,Body) ->
    [{aeb_opcodes:mnemonic(?JUMPDEST),lookup_fun(Funs,Name,length(Args))},
     assemble_expr(Funs, lists:reverse(Args), Body),
     %% swap return value and first argument
     [swap(length(Args)) || Args/=[]],
     [aeb_opcodes:mnemonic(?POP) || _ <- Args],
     swap(1),
     aeb_opcodes:mnemonic(?JUMP)].

assemble_expr(Funs,Stack,{var_ref,Id}) ->
    case lists:keyfind(Id,1,Funs) of
	{Id,_Arity,Label} ->
	    {push_label,Label};
	false ->
	    dup(lookup_var(Id,Stack))
    end;
assemble_expr(_Funs,_Stack,{integer,N}) ->
    push(N);
assemble_expr(Funs,Stack,{unop,'!',A}) ->
    case A of
	{binop,Logical,_,_} when Logical=='&&'; Logical=='||' ->
	    assemble_expr(Funs,Stack,{ifte,A,{integer,0},{integer,1}});
	_ ->
	    [assemble_expr(Funs,Stack,A),
	     aeb_opcodes:mnemonic(?ISZERO)
	    ]
    end;
assemble_expr(Funs,Stack,{unop,Op,A}) ->
    [assemble_expr(Funs,Stack,A),
     assemble_prefix(Op)];
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
assemble_expr(Funs,Stack,{funcall,Fun,Args}) ->
    %% TODO: tail-call optimization!
    Return = make_ref(),
    [{push_label,Return},
     assemble_exprs(Funs,[return_address|Stack],Args++[Fun]),
     'JUMP',
     {'JUMPDEST',Return}];
assemble_expr(Funs,Stack,{ifte,Decision,Then,Else}) ->
    %% This compilation scheme introduces a lot of labels and
    %% jumps. Unnecessary ones are removed later in
    %% resolve_references.
    Close = make_ref(),
    ThenL  = make_ref(),
    ElseL  = make_ref(),
    [assemble_decision(Funs,Stack,Decision,ThenL,ElseL),
     {aeb_opcodes:mnemonic(?JUMPDEST),ElseL},
     assemble_expr(Funs,Stack,Else),
     {push_label,Close},
     aeb_opcodes:mnemonic(?JUMP),
     {aeb_opcodes:mnemonic(?JUMPDEST),ThenL},
     assemble_expr(Funs,Stack,Then),
     {aeb_opcodes:mnemonic(?JUMPDEST),Close}
    ].

assemble_exprs(_Funs,_Stack,[]) ->
    [];
assemble_exprs(Funs,Stack,[E|Es]) ->
    [assemble_expr(Funs,Stack,E),
     assemble_exprs(Funs,[dummy|Stack],Es)].

assemble_decision(Funs,Stack,{binop,'&&',A,B},Then,Else) ->
    Label = make_ref(),
    [assemble_decision(Funs,Stack,A,Label,Else),
     {aeb_opcodes:mnemonic(?JUMPDEST),Label},
     assemble_decision(Funs,Stack,B,Then,Else)];
assemble_decision(Funs,Stack,{binop,'||',A,B},Then,Else) ->
    Label = make_ref(),
    [assemble_decision(Funs,Stack,A,Then,Label),
     {aeb_opcodes:mnemonic(?JUMPDEST),Label},
     assemble_decision(Funs,Stack,B,Then,Else)];
assemble_decision(Funs,Stack,{unop,'!',A},Then,Else) ->
    assemble_decision(Funs,Stack,A,Else,Then);
assemble_decision(Funs,Stack,{ifte,A,B,C},Then,Else) ->
    TrueL = make_ref(),
    FalseL = make_ref(),
    [assemble_decision(Funs,Stack,A,TrueL,FalseL),
     {?JUMPDEST,TrueL},assemble_decision(Funs,Stack,B,Then,Else),
     {?JUMPDEST,FalseL},assemble_decision(Funs,Stack,C,Then,Else)];
assemble_decision(Funs,Stack,Decision,Then,Else) ->
    [assemble_expr(Funs,Stack,Decision),
     {push_label,Then}, aeb_opcodes:mnemonic(?JUMPI),
     {push_label,Else}, aeb_opcodes:mnemonic(?JUMP)].

assemble_prefix('-') -> [push(0),aeb_opcodes:mnemonic(?SUB)];
assemble_prefix('bnot') -> aeb_opcodes:mnemonic(?NOT).

assemble_infix('+') -> aeb_opcodes:mnemonic(?ADD);
assemble_infix('-') -> aeb_opcodes:mnemonic(?SUB);
assemble_infix('*') -> aeb_opcodes:mnemonic(?MUL);
assemble_infix('/') -> aeb_opcodes:mnemonic(?SDIV);
assemble_infix('bor') -> aeb_opcodes:mnemonic(?OR);
assemble_infix('band') -> aeb_opcodes:mnemonic(?AND);
assemble_infix('bxor') -> aeb_opcodes:mnemonic(?XOR);
assemble_infix('<') -> aeb_opcodes:mnemonic(?SLT);    %% comparisons are SIGNED
assemble_infix('>') -> aeb_opcodes:mnemonic(?SGT);
assemble_infix('==') -> aeb_opcodes:mnemonic(?EQ);
assemble_infix('<=') -> [aeb_opcodes:mnemonic(?SGT),aeb_opcodes:mnemonic(?ISZERO)];
assemble_infix('>=') -> [aeb_opcodes:mnemonic(?SLT),aeb_opcodes:mnemonic(?ISZERO)];
assemble_infix('!=') -> [aeb_opcodes:mnemonic(?EQ),aeb_opcodes:mnemonic(?ISZERO)].

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

%% TODO: handle references to the stack beyond depth 16. Perhaps the
%% best way is to repush variables that will be needed in
%% subexpressions before evaluating he subexpression... i.e. fix the
%% problem in assemble_expr, rather than here. A fix here would have
%% to save the top elements of the stack in memory, duplicate the
%% targetted element, and then repush the values from memory.
dup(N) when N=<16 ->
    aeb_opcodes:mnemonic(?DUP1 + N-1).

push(N) ->
    Bytes = binary:encode_unsigned(N),
    true = size(Bytes) =< 32,
    [aeb_opcodes:mnemonic(?PUSH1 + size(Bytes)-1) |
     binary_to_list(Bytes)].
 
swap(N) when N=<16 ->
    aeb_opcodes:mnemonic(?SWAP1 + N-1).

%% Resolve references, and convert code from deep list to flat list.
%% List elements are:
%%   Opcodes
%%   Byte values
%%   {'JUMPDEST',Ref}   -- assembles to ?JUMPDEST and sets Ref
%%   {push_label,Ref}  -- assembles to ?PUSHN address bytes

%% For now, we assemble all code addresses as three bytes.

resolve_references(Code) ->
    Peephole = peep_hole(lists:flatten(Code)),
    %% WARNING: Optimizing jumps reorders the code and deletes
    %% instructions. When debugging the assemble_ functions, it can be
    %% useful to replace the next line by:
    %%   Instrs = lists:flatten(Code)
    %% thus disabling the optimization.
    Instrs = optimize_jumps(Peephole),
    Labels = define_labels(0,Instrs),
    lists:flatten([use_labels(Labels,I) || I <- Instrs]).

define_labels(Addr,[{'JUMPDEST',Lab}|More]) ->
    [{Lab,Addr}|define_labels(Addr+1,More)];
define_labels(Addr,[{push_label,_}|More]) ->
    define_labels(Addr+4,More);
define_labels(Addr,[_|More]) ->
    define_labels(Addr+1,More);
define_labels(_,[]) ->
    [].

use_labels(_,{'JUMPDEST',_}) ->
    'JUMPDEST';
use_labels(Labels,{push_label,Ref}) ->
    case proplists:get_value(Ref,Labels) of
	undefined ->
	    error({undefined_label,Ref});
	Addr when is_integer(Addr) ->
	    [aeb_opcodes:mnemonic(?PUSH3),
	     Addr div 65536,(Addr div 256) rem 256, Addr rem 256]
    end;
use_labels(_,I) ->
    I.

%% Peep-hole optimization.
%% The compilation of conditionals can introduce jumps depending on
%% constants 1 and 0. These are removed by peep-hole optimization. 

peep_hole(['PUSH1',0,{push_label,_},'JUMP1'|More]) ->
    peep_hole(More);
peep_hole(['PUSH1',1,{push_label,Lab},'JUMP1'|More]) ->
    [{push_label,Lab},'JUMP'|peep_hole(More)];
peep_hole([I|More]) ->
    [I|peep_hole(More)];
peep_hole([]) ->
    [].

%% Jump optimization:
%%   Replaces a jump to a jump with a jump to the final destination
%%   Moves basic blocks to eliminate an unconditional jump to them.

%% The compilation of conditionals generates a lot of labels and
%% jumps, some of them unnecessary. This optimization phase reorders
%% code so that as many jumps as possible can be eliminated, and
%% replaced by just falling through to the destination label. This
%% both optimizes the code generated by conditionals, and converts one
%% call of a function into falling through into its code--so it
%% reorders code quite aggressively. Function returns are indirect
%% jumps, however, and are never optimized away.

%% IMPORTANT: since execution begins at address zero, then the first
%% block of code must never be moved elsewhere. The code below has
%% this property, because it processes blocks from left to right, and
%% because the first block does not begin with a label, and so can
%% never be jumped to--hence no code can be inserted before it.

%% The optimization works by taking one block of code at a time, and
%% then prepending blocks that jump directly to it, and appending
%% blocks that it jumps directly to, resulting in a jump-free sequence
%% that is as long as possible. To do so, we store blocks in the form
%% {OptionalLabel,Body,OptionalJump} which represents the code block
%% OptionalLabel++Body++OptionalJump; the optional parts are the empty
%% list of instructions if not present.  Two blocks can be merged if
%% the first ends in an OptionalJump to the OptionalLabel beginning
%% the second; the OptionalJump can then be removed (and the
%% OptionalLabel if there are no other references to it--this happens
%% during dead code elimination.

%% TODO: the present implementation is QUADRATIC, because we search
%% repeatedly for matching blocks to merge with the first one, storing
%% the blocks in a list. A near linear time implementation could use
%% two ets tables, one keyed on the labels, and the other keyed on the
%% final jumps.

optimize_jumps(Code) ->
    JJs = jumps_to_jumps(Code),
    ShortCircuited = [short_circuit_jumps(JJs,Instr) || Instr <- Code],
    NoDeadCode = eliminate_dead_code(ShortCircuited),
    MovedCode = merge_blocks(moveable_blocks(NoDeadCode)),
    %% Moving code may have made some labels superfluous.
    eliminate_dead_code(MovedCode). 


jumps_to_jumps([{'JUMPDEST',Label},{push_label,Target},'JUMP'|More]) ->
    [{Label,Target}|jumps_to_jumps(More)];
jumps_to_jumps([{'JUMPDEST',Label},{'JUMPDEST',Target}|More]) ->
    [{Label,Target}|jumps_to_jumps([{'JUMPDEST',Target}|More])];
jumps_to_jumps([_|More]) ->
    jumps_to_jumps(More);
jumps_to_jumps([]) ->
    [].

short_circuit_jumps(JJs,{push_label,Lab}) ->
    case proplists:get_value(Lab,JJs) of
	undefined ->
	    {push_label,Lab};
	Target ->
	    %% I wonder if this will ever loop infinitely?
	    short_circuit_jumps(JJs,{push_label,Target})
    end;
short_circuit_jumps(_JJs,Instr) ->
    Instr.

eliminate_dead_code(Code) ->
    Jumps = lists:usort([Lab || {push_label,Lab} <- Code]),
    NewCode = live_code(Jumps,Code),
    if Code==NewCode ->
	    Code;
       true ->
	    eliminate_dead_code(NewCode)
    end.

live_code(Jumps,['JUMP'|More]) ->
    ['JUMP'|dead_code(Jumps,More)];
live_code(Jumps,['STOP'|More]) ->
    ['STOP'|dead_code(Jumps,More)];
live_code(Jumps,[{'JUMPDEST',Lab}|More]) ->
    case lists:member(Lab,Jumps) of
	true ->
	    [{'JUMPDEST',Lab}|live_code(Jumps,More)];
	false ->
	    live_code(Jumps,More)
    end;
live_code(Jumps,[I|More]) ->
    [I|live_code(Jumps,More)];
live_code(_,[]) ->
    [].

dead_code(Jumps,[{'JUMPDEST',Lab}|More]) ->
    case lists:member(Lab,Jumps) of
	true ->
	    [{'JUMPDEST',Lab}|live_code(Jumps,More)];
	false ->
	    dead_code(Jumps,More)
    end;
dead_code(Jumps,[_I|More]) ->
    dead_code(Jumps,More);
dead_code(_,[]) ->
    [].

%% Split the code into "moveable blocks" that control flow only
%% reaches via jumps.
moveable_blocks([]) ->
    [];
moveable_blocks([I]) ->
    [[I]];
moveable_blocks([Jump|More]) when Jump=='JUMP'; Jump=='STOP' ->
    [[Jump]|moveable_blocks(More)];
moveable_blocks([I|More]) ->
    [Block|MoreBlocks] = moveable_blocks(More),
    [[I|Block]|MoreBlocks].

%% Merge blocks to eliminate jumps where possible.
merge_blocks(Blocks) ->
    BlocksAndTargets = [label_and_jump(B) || B <- Blocks],
    [I || {Pref,Body,Suff} <- merge_after(BlocksAndTargets),
	  I <- Pref++Body++Suff].

%% Merge the first block with other blocks that come after it
merge_after(All=[{Label,Body,[{push_label,Target},'JUMP']}|BlocksAndTargets]) ->
    case [{B,J} || {[{'JUMPDEST',L}],B,J} <- BlocksAndTargets,
		   L == Target] of
	[{B,J}|_] ->
	    merge_after([{Label,Body++[{'JUMPDEST',Target}]++B,J}|
			 lists:delete({[{'JUMPDEST',Target}],B,J},
				      BlocksAndTargets)]);
	[] ->
	    merge_before(All)
    end;
merge_after(All) ->
    merge_before(All).

%% The first block cannot be merged with any blocks that it jumps
%% to... but maybe it can be merged with a block that jumps to it!
merge_before([Block={[{'JUMPDEST',Label}],Body,Jump}|BlocksAndTargets]) ->
    case [{L,B,T} || {L,B,[{push_label,T},'JUMP']} <- BlocksAndTargets,
		     T == Label] of
	[{L,B,T}|_] ->
	    merge_before([{L,B++[{'JUMPDEST',Label}]++Body,Jump}
			  |lists:delete({L,B,[{push_label,T},'JUMP']},BlocksAndTargets)]);
	_ ->
	    [Block | merge_after(BlocksAndTargets)]
    end;
merge_before([Block|BlocksAndTargets]) ->
    [Block | merge_after(BlocksAndTargets)];
merge_before([]) ->
    [].

%% Convert each block to a PREFIX, which is a label or empty, a
%% middle, and a SUFFIX which is a JUMP to a label, or empty.
label_and_jump(B) ->
    {Label,B1} = case B of
		     [{'JUMPDEST',L}|More1] ->
			 {[{'JUMPDEST',L}],More1};
		     _ ->
			 {[],B}
		 end,
    {Target,B2} = case lists:reverse(B1) of
		      ['JUMP',{push_label,T}|More2] ->
			  {[{push_label,T},'JUMP'],lists:reverse(More2)};
		      _ ->
			  {[],B1}
		  end,
    {Label,B2,Target}.
