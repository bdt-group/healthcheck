-module(healthcheck).

-behaviour(application).

%% public API
-export([start/0, stop/0, start_table/0]).
-export([get_state/0, set_state/2]).
%% Application callbacks
-export([start/2, stop/1, prep_stop/1, config_change/3]).

-type http_code() :: 100..599.
-type http_body() :: empty | binary().


%%%===================================================================
%%% public API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    case application:stop(?MODULE) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        {error, _} = Err -> Err
    end.

-spec get_state() -> {http_code(), http_body()}.
get_state() ->
    [{?MODULE, State}] = ets:lookup(?MODULE, ?MODULE),
    State.

-spec set_state(http_code(), http_body()) -> ok.
set_state(Code, Content) ->
    true = ets:insert(?MODULE, {?MODULE, {Code, Content}}),
    ok.

-spec start_table() -> ok.
start_table() ->
    ?MODULE = ets:new(?MODULE, [named_table, public, set]),
    set_state(204, empty),
    ok.


%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case healthcheck_sup:start_link() of
        {ok, _} = Ok ->
            case healthcheck_httpd:start() of
                ok -> Ok;
                Err -> Err
            end;
        Err ->
            Err
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    _ = healthcheck_httpd:stop(),
    State.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(Changed, New, Removed) ->
    _ = healthcheck_httpd:config_change(Changed, New, Removed),
    ok.
