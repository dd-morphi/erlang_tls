-module(client).

-behaviour(gen_server).

%% API export
-export([start_link/2, send/1, example_certificates/0, example_simple/0,
         example_session_tickets/0, reconnect/0, stop/0]).
%% Callbacks export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2,
         code_change/3, terminate/2]).

-record(state, {socket, config, port}).

%% API
example_simple() ->
    start_link(9999, []).

example_certificates() ->
    Config = [{certfile, "client_certificate.pem"}, {keyfile, "client_key.pem"}],
    start_link(9999, Config).

example_session_tickets() ->
    Config =
        [{session_tickets, auto},
         {certfile, "client_certificate.pem"},
         {keyfile, "client_key.pem"},
         {log_level, info}],
    start_link(9999, Config).

reconnect() ->
    gen_server:cast(?MODULE, reconnect).

start_link(Port, AddConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AddConfig], []).

send(Data) ->
    gen_server:cast(?MODULE, {send, Data}).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

%% Callbacks
init([Port, AddConfig]) ->
    common:configure_logger("client_debug.log"),
    Config = [{verify_fun, {my_verify_fun(), []}}] ++ AddConfig,
    {ok, _} = application:ensure_all_started(ssl),
    {ok, #state{config = Config, port = Port}, {continue, connect}}.

handle_continue(connect, #state{config = Config, port = Port} = State) ->
    case connect(Port, Config) of
        {ok, Socket} ->
            {noreply, State#state{socket = Socket}};
        _ ->
            {noreply, State}
    end.

handle_call(Command, _From, State) ->
    logger:debug("Call: ~p", [Command]),
    {reply, ok, State}.

handle_cast({send, Data}, #state{socket = Socket} = State) ->
    logger:debug("Sending data: ~p", [Data]),
    ssl:send(Socket, Data),
    {noreply, State};
handle_cast(reconnect, #state{port = Port, config = Config} = State) ->
    logger:debug("Disconnecting"),
    ssl:close(State#state.socket),
    %% if we cannot connect let it crash
    {ok, NewSocket} = connect(Port, Config),
    {noreply, State#state{socket = NewSocket}};
handle_cast({stop, Reason}, State) ->
    logger:debug("Stopping with reason ~w", [Reason]),
    {stop, Reason, State};
handle_cast(Command, State) ->
    logger:debug("Cast: ~p", [Command]),
    {noreply, State}.

handle_info({ssl, _Socket, Data}, State) ->
    logger:debug("SSL Received ~p", [Data]),
    {noreply, State};
handle_info({ssl_error, _Socket, Error}, State) ->
    logger:debug("SSL Error ~p", [Error]),
    {noreply, State};
handle_info({ssl_closed, _Socket}, State) ->
    logger:debug("SSL Closed"),
    {noreply, State};
handle_info(Message, State) ->
    logger:debug("Info: ~p", [Message]),
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    logger:debug("Terminate: ~p", [Reason]),
    {ok, Reason, State}.

my_verify_fun() ->
    fun(_, Any, UserState) ->
       logger:debug("Certification verification on ~p", [Any]),
       {valid, UserState}
    end.

connect(Port, Config) ->
    logger:debug("Connecting to port ~w", [Port]),
    case ssl:connect("localhost", Port, Config, infinity) of
        {ok, _Socket} = Result ->
            logger:debug("Connected"),
            Result;
        {error, _} = Error ->
            logger:debug("Could not connect ~p", [Error]),
            Error
    end.
