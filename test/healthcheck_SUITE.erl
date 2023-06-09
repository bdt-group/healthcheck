-module(healthcheck_SUITE).

-compile(export_all).

-define(HEALTHCHECK_URI, "http://127.0.0.1:8080/healthcheck").


suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [check].

init_per_suite(Config) ->
    application:set_env(healthcheck, listen, #{ip => {127, 0, 0, 1}, port => 8080}),
    ok = healthcheck:start(),
    Config.

end_per_suite(_Config) ->
    ok = healthcheck:stop().

init_per_testcase(Config) ->
    Config.

end_per_testcase(_Config) ->
    ok.

check(_Config) ->
    {ok, {{_, 204, _}, _, ""}} = httpc:request(get, {?HEALTHCHECK_URI, []}, [], []),
    ok = healthcheck:set_state(200, <<"OK">>),
    {ok, {{_, 200, _}, _, "OK"}} = httpc:request(?HEALTHCHECK_URI),
    ok = healthcheck:set_state(500, <<"Server error: foo bar">>),
    {ok, {{_, 500, _}, _, "Server error: foo bar"}} = httpc:request(?HEALTHCHECK_URI).
