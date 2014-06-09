% buffer utilities
% these are not "real" buffers though..

-module(util).
-compile(export_all).

% chain a list of functions using last argument
chain_last(X, [F_head]) -> F_head(X);
chain_last(X, [F_head|F_tail]) -> chain_last(F_head(X), F_tail).

% generate a list of [0, 1,2,...X-1]
int_to_list(0) -> [0];
int_to_list(X) when X > 0 -> [X-1|int_to_list(X-1)].

