%%% @author Thomas Arts <thomas@ThomasComputer.local>
%%% @copyright (C) 2017, Thomas Arts
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2017 by Thomas Arts <thomas@ThomasComputer.local>

-module(gbtree_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

remove_by_value(Value, Tree) ->
    remove_by_value(Value, gb_trees:next(gb_trees:iterator(Tree)), gb_trees:empty()).

remove_by_value(_Value, none, NewTree) ->
    NewTree;
remove_by_value(Value, {_K, Value, Iter}, NewTree) ->
    remove_by_value(Value, gb_trees:next(Iter), NewTree);
remove_by_value(Value, {K, V, Iter}, NewTree) ->
   remove_by_value(Value, gb_trees:next(Iter), gb_trees:enter(K, V, NewTree)).

key() ->
  elements([a, b, c, d, e]).

value() ->
  choose(0,8).

prop_ok() ->
  ?FORALL({KVs, Value}, {list({key(), value()}), value()},
          begin
            Tree = 
              lists:foldl(fun({K,V},T) -> 
                                 gb_trees:enter(K, V, T)
                          end, gb_trees:empty(), KVs),
            NewTree = remove_by_value(Value, Tree),
            ?WHENFAIL(eqc:format("tree ~p -> ~p~n", [Tree, NewTree]),
                      not lists:keymember(Value, 2, gb_trees:to_list(NewTree)))
              
          end).
