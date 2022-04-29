-module(client).

-behaviour(gen_server).

%% API export
-export([start_link/2, send/1, example_certificates/0, example_simple/0]).
%% Callbacks export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {socket}).

%% API
example_simple() ->
    start_link(9999, []).

example_certificates() ->
    Config = [{certfile, "client_certificate.pem"}, {keyfile, "client_key.pem"}],
    start_link(9999, Config).

start_link(Port, AddConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AddConfig], []).

send(Data) ->
    gen_server:cast(?MODULE, {send, Data}).

%% Callbacks
init([Port, AddConfig]) ->
    common:configure_logger("client_debug.log"),
    Config = [{verify_fun, {my_verify_fun(), []}}] ++ AddConfig,
    ssl:start(),
    {ok, Socket} = ssl:connect("localhost", Port, Config, infinity),
    logger:debug("Connected"),
    {ok, #state{socket = Socket}}.

handle_call(Command, _From, State) ->
    logger:debug("Call: ~p", [Command]),
    {reply, ok, State}.

handle_cast({send, Data}, #state{socket = Socket} = State) ->
    logger:debug("Sending data: ~p", [Data]),
    ssl:send(Socket, Data),
    {noreply, State};
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
