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

read_semver_file(Path, Number) ->
    case file:read_file(Path) of
            {ok, Data} -> lists:nth(Number, binary:split(Data, [<<"\n">>], [global]));
            {error, _} -> <<"file error">>
    end.

find_version_number(Path) ->
    Data = read_semver_file(Path, 1),
    case re:run(binary_to_list(Data),
                <<"(\\d{1,}).(\\d{1,}).(\\d{1,})-(\\S{0,}|$)">>,
                [{capture, all_but_first, list}]) of
        {match, [Major, Minor, Patch, Label]} ->
                Major ++ "." ++ Minor ++ "." ++ Patch ++ "-" ++ Label;
        nomatch -> "undefined"
    end.

find_version_datetime(Path) ->
    Data = read_semver_file(Path, 2),
    case re:run(binary_to_list(Data),
                <<"(\\d{1,2})/(\\d{1,2}).(\\d{2,4}) (\\d{2}):(\\d{2}):(\\d{2})">>,
                [{capture, all_but_first, list}]) of
        {match, [Day, Month, Year, Hours, Minutes, Seconds]} ->
                Day ++ "/" ++ Month ++ "/" ++ Year ++ " " ++ Hours ++ ":" Minutes ++ ":" Seconds;
        nomatch -> "undefined"
    end.


find_service_name(Path) ->
    Data = read_semver_file(Path, 3),
    case re:run(binary_to_list(Data),
                <<"(\\S{0,}|$)">>,
                [{capture, all_but_first, list}]) of
        {match, [ServiceName]} -> ServiceName;
        nomatch -> "undefined"
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
    Headers = #{<<"content-type">> => <<"text/plain">>,
                <<"version_number">> => list_to_binary(find_version_number(get_version_file(?VERSION_FILE)))},
                <<"version_datetime">> => list_to_binary(find_version_datetime(get_version_file(?VERSION_FILE)))},
                <<"service_name">> => list_to_binary(find_service_name(get_version_file(?VERSION_FILE)))},
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
