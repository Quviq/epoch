%% This contains a test for me to run on my laptop--John
-module(setup).
-compile(export_all).

setup() ->
  code:add_patha("../../aebytecode/ebin"),
  code:add_patha("../../aevm/ebin").

dummy_state(Code,Arg) ->
  #{ gas        => 10000,
     code       => Code,
     cp         => 0,
     memory     => #{},
     stack      => [Arg,0],
     do_trace   => false,    %% set to true for step-by-step tracing
     trace      => [],
     trace_fun  => fun io:format/2
  }.

test() ->
  Code = aer_compiler:file(test,[pp_ast,pp_icode]),
  io:format("\nCompiled code:\n"),
  io:format("~p\n\n",[Code]),
  ok = aeb_disassemble:pp(Code),
  io:format("Running:\n"),
  State = aevm_eeevm:eval(dummy_state(Code, 42)),
  %%io:format("\nFinal state:\n~p\n",[State]),
  io:format("\nFinal stack: ~p\n",[maps:get(stack,State)]),
  ok.
  

