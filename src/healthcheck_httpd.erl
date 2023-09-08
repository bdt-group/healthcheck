-module(healthcheck_httpd).

-include_lib("kernel/include/logger.hrl").

%% public API
-export([start/0, stop/0, config_change/3]).
%% cowboy handler callback
-export([init/2]).

-type options() :: #{ip := inet:ip_address(),
                     port := inet:port_number()}.
-type http_method() :: binary().

-define(VERSION_FILE, "SEMVER").

%%%===================================================================
%%% public API
%%%===================================================================
-spec start() -> ok | {error, term()}.
start() ->
    case application:get_env(healthcheck, listen) of
        {ok, Opts} ->
            case start(Opts) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end;
        undefined ->
            ok
    end.

-spec start(options()) -> {ok, pid()} | {error, term()}.
start(#{ip := IP, port := Port}) ->
    ?LOG_NOTICE("Starting HTTP healthcheck listener at ~s:~B", [format_ip(IP), Port]),
    Dispatch = cowboy_router:compile([{'_', routes()}]),
    Env = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(?MODULE, [{port, Port}, {ip, IP}], Env).

-spec stop() -> ok | {error, term()}.
stop() ->
    ?LOG_NOTICE("Stopping HTTP metrics listener"),
    cowboy:stop_listener(?MODULE).

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok | {error, term()}.
config_change(Changed, New, Removed) ->
    case {proplists:get_value(listen, Changed),
          proplists:get_value(listen, New),
          lists:member(listen, Removed)} of
        {undefined, undefined, false} -> ok;
        {undefined, undefined, true} -> stop();
        {undefined, Opts, _} -> start(Opts);
        {Opts, _, _} ->
            _ = stop(),
            start(Opts)
    end.


%%%===================================================================
%%% cowboy handler API
%%%===================================================================
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Resp = try handle(Method, Req) of
               Ok -> Ok
           catch
               C:R:S ->
                   ?LOG_ERROR("handler unhandled exception: ~p:~p at ~p",
                              [C, R, S]),
                   json_response({error, internal}, Req)
           end,
    {ok, Resp, Opts}.


%%%===================================================================
%%% private
%%%===================================================================

read_semver_file(Path) ->
    case file:read_file(Path) of
        {ok, Data} -> Data;
        {error, _} -> <<"file error">>
    end.

get_records(Path) ->
    Data = binary:split(read_semver_file(Path), [<<"\n">>], [global, trim_all]),
    case Data of
        [VersionNumber, VersionDatetime, ServiceName] -> Data;
        _ -> [<<"undefined">>, <<"undefined">>, <<"undefined">>]
    end.

get_version_file(Filename) ->
    case file:get_cwd() of
        {ok, CurrentDirectory} ->  filename:join(CurrentDirectory, Filename);
        {error, _} -> "undefined"
    end.

-spec routes() -> [{string(), module(), term()}].
routes() ->
    [{"/healthcheck", ?MODULE, []} | meter:cowboy_routes()].

-spec format_ip(inet:ip_address()) -> string().
format_ip({_, _, _, _} = IP4) ->
    inet_parse:ntoa(IP4);
format_ip(IP6) ->
    "[" ++ inet_parse:ntoa(IP6) ++ "]".

-spec handle(http_method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    [VersionNumber, VersionDatetime, ServiceName] = get_records(get_version_file(?VERSION_FILE)),
    Headers = #{<<"content-type">> => <<"text/plain">>,
                <<"version_number">> => VersionNumber,
                <<"version_datetime">> => VersionDatetime,
                <<"service_name">> => ServiceName},
    case cowboy_req:has_body(Req) of
        true ->
            json_response({error, body_present, Headers}, Req);
        false ->
            State = healthcheck:get_state(),
            json_response({ok, State, Headers}, Req)
    end;
handle(_, Req) ->
    json_response({error, notfound}, Req).

-spec json_response(_, cowboy_req:req()) -> cowboy_req:req().
json_response({ok, {Code, Content}, Headers}, Req) when is_binary(Content) ->
    cowboy_req:reply(Code, Headers, Content, Req);
json_response({ok, Other, Headers}, Req) ->
    ?LOG_WARNING("Unable to render current state: ~p", [Other]),
    cowboy_req:reply(500, Headers,
                     <<"Server error: unable to render current state, check logs!">>, Req);
json_response({error, body_present, Headers}, Req) ->
    cowboy_req:reply(400, Headers,
                     <<"Unexpected body">>, Req);
json_response({error, notfound}, Req) ->
    cowboy_req:reply(404, Req);
json_response({error, internal}, Req) ->
    cowboy_req:reply(520, Req).
