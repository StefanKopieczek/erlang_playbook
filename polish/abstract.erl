-module(abstract).
-export([eval/1, parse/1, split/1]).
-import(lists, [reverse/1]).

eval({value, Val}) -> Val;
eval({expr, Op, A}) -> (operator(Op))(A);
eval({expr, Op, A, B}) -> (operator(Op))(A, B).

operator(add) -> fun(A, B) -> A + B end;
operator(sub) -> fun(A, B) -> A - B end;
operator(mult) -> fun(A, B) -> A * B end;
operator(divd) -> fun(A, B) -> A / B end.

parse(PolishString) ->
    split(PolishString).

split(String) -> split(String, [], "", 0).
split([], _Parts, _Acc, Depth) when Depth > 0 -> throw({parse_error, "Expected ')'"});
split([], Parts, "", 0) -> reverse(Parts);
split([], Parts, Acc, 0) -> reverse([reverse(Acc) | Parts]);
split([$)|_Rest], _Parts, _Acc, 0) -> throw({parse_error, "Unexpected ')'"});
split([$)|Rest], Parts, "", 1) -> split(Rest, Parts, "", 0);
split([$)|Rest], Parts, Acc, 1) -> split(Rest, [reverse([$)|Acc])|Parts], "", 0);
split([$(|Rest], Parts, "", 0) -> split(Rest, Parts, "", 1);
split([$(|Rest], Parts, "", 1) -> split(Rest, Parts, "(", 2);
split([$(|Rest], Parts, Acc, 1) -> split(Rest, [reverse(Acc)|Parts], "(", 2);
split([$ |Rest], Parts, Acc, 1) -> split(Rest, [reverse(Acc)|Parts], "", 1);
split([$(|Rest], Parts, Acc, Depth) -> split(Rest, Parts, [$(|Acc], Depth+1);
split([$)|Rest], Parts, Acc, Depth) -> split(Rest, Parts, [$)|Acc], Depth-1);
split([Char|Rest], Parts, Acc, Depth) -> split(Rest, Parts, [Char|Acc], Depth).
