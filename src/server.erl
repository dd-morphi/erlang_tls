-module(server).

-behaviour(gen_server).

%% API export
-export([start_link/2, send/1, any/0, require_user_certificates/0]).
%% Callbacks export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2,
         handle_continue/2]).

-record(state, {port, listen_socket, client_socket, tls_options}).

%% API
any() ->
    start_link(9999, []).

require_user_certificates() ->
    Config = [{verify, verify_peer}, {fail_if_no_peer_cert, true}],
    start_link(9999, Config).

start_link(Port, AddConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AddConfig], []).

send(Data) ->
    gen_server:cast(?MODULE, {send, Data}).

%% Callbacks
init([Port, AddConfig]) ->
    common:configure_logger("server_debug.log"),
    Config =
        [{certfile, "server_certificate.pem"},
         {keyfile, "server_key.pem"},
         {reuseaddr, true},
         %% {log_level, all}, %% It adds all logs
         {verify_fun, {my_verify_fun(), []}}]
        ++ AddConfig,
    {ok, #state{port = Port, tls_options = Config}, {continue, listen}}.

handle_continue(listen, #state{port = Port, tls_options = TlsOptions} = State) ->
    ssl:start(),
    {ok, ListenSocket} = ssl:listen(Port, TlsOptions),
    logger:debug("Accepting on port ~p", [Port]),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    logger:debug("Socket connected"),
    {ok, Socket} = ssl:handshake(TLSTransportSocket),
    {noreply, State#state{listen_socket = ListenSocket, client_socket = Socket}}.

handle_call(Command, _From, State) ->
    logger:debug("Call: ~p", [Command]),
    {reply, ok, State}.

handle_cast({send, Data}, #state{client_socket = Socket} = State) ->
    logger:debug("Sending data to client: ~p", [Data]),
    ssl:send(Socket, Data),
    {noreply, State};
handle_cast(Command, State) ->
    logger:debug("Cast: ~p", [Command]),
    {noreply, State}.

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
       logger:debug("Client certificate verification on ~p", [Any]),
       {valid, UserState}
    end.

 %%    fun(_,{bad_cert, _}, UserState) ->
 %%     {valid, UserState};
 %%    %% (_,{extension, #'Extension'{critical = true}}, UserState) ->
 %%    %%  {valid, UserState};
 %%    (_,{extension, _}, UserState) ->
 %%     {unknown, UserState};
 %%    (_, valid, UserState) ->
 %%     {valid, UserState};
 %%    (_, valid_peer, UserState) ->
 %%         {valid, UserState}
 %% end.
