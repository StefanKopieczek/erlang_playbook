-module(bintree).
-import(lists, [reverse/1]).
-export([new/0, add/2, find/2, from_list/1, flatten/1, flatten/2, fold/4]).

new() -> leaf.

add(leaf, NewVal) -> {node, NewVal, leaf, leaf};
add({node, Value, Left, Right}, NewVal) when NewVal =< Value ->
    {node, Value, add(Left, NewVal), Right};
add({node, Value, Left, Right}, NewVal) when NewVal > Value ->
   {node, Value, Left, add(Right, NewVal)}.

from_list([]) -> leaf;
from_list([H|T]) -> add(from_list(T), H).

find(Tree, Target) -> reverse(find(Tree, Target, [root])).
find(leaf, _, _) -> [nomatch];
find({node, Target, _, _}, Target, Path) -> Path;
find({node, _, Left, Right}, Target, Path) ->
    LeftMatch = find(Left, Target, [l|Path]),
    if LeftMatch == [nomatch] ->
        find(Right, Target, [r|Path]);
    true ->
        LeftMatch
    end.

flatten(Tree) -> flatten(Tree, inorder).

flatten(Tree, preorder) -> preorder(Tree);
flatten(Tree, inorder) -> inorder(Tree);
flatten(Tree, postorder) -> postorder(Tree).

preorder(leaf) -> [];
preorder({node, Value, Left, Right}) ->
    [Value | (preorder(Left) ++ preorder(Right))].

inorder(leaf) -> [];
inorder({node, Value, Left, Right}) ->
    inorder(Left) ++ [Value | inorder(Right)].

postorder(leaf) -> [];
postorder({node, Value, Left, Right}) ->
    %% Wow, this is inefficient.
    postorder(Left) ++ postorder(Right) ++ [Value].

fold(Tree, Func, Initial, preorder) -> fold_preorder(Tree, Func, Initial);
fold(Tree, Func, Initial, inorder) -> fold_inorder(Tree, Func, Initial);
fold(Tree, Func, Initial, postorder) -> fold_postorder(Tree, Func, Initial).

fold_preorder(leaf, _, Initial) -> Initial;
fold_preorder({node, Value, Left, Right}, Func, Initial) ->
    fold_preorder(Right, Func, fold_preorder(Left, Func, Func(Initial, Value))).

fold_inorder(leaf, _, Initial) -> Initial;
fold_inorder({node, Value, Left, Right}, Func, Initial) ->
    fold_inorder(Right, Func, Func(fold_inorder(Left, Func, Initial), Value)).

fold_postorder(leaf, _, Initial) -> Initial;
fold_postorder({node, Value, Left, Right}, Func, Initial) ->
    Func(fold_postorder(Right, Func, fold_postorder(Left, Func, Initial)), Value).
