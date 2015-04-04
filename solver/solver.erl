-module(solver).
-export([newton_iter/4, forward_diff/2]).

newton_iter(_, _, X, 0) -> X;
newton_iter(Func, Diff, X, N) ->
    newton_iter(Func, Diff, X - (Func(X) / (Diff(Func))(X)), N - 1).

forward_diff(Func, H) -> (fun(X) -> (Func(X + H) - Func(X)) / H end).
