-module(healthcheck_yaml).

-behaviour(conf).

%% conf callbacks
-export([validator/0]).

-import(yval, [options/2, int/2, ip/0]).


%%%===================================================================
%%% conf callbacks
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{listen =>
            options(
              #{port => int(0, 65535),
                ip => ip()},
              [unique, {return, map}, {defaults, default(listen)}])},
      [unique, {return, map},
       {defaults, #{listen => default(listen)}}]).


%%%===================================================================
%%% private
%%%===================================================================
default(listen) ->
    #{port => 8080,
      ip => {0, 0, 0, 0}}.
