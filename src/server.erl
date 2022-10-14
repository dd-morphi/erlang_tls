-module(server).

-behaviour(gen_server).

%% API export
-export([start_link/2, send/1, any/0, require_user_certificates/0,
         stateless_session_tickets/0, stop/0, accept/0]).
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

stateless_session_tickets() ->
    %% Config = [{versions, ['tlsv1.3']}, {handshake, hello}, {session_tickets, stateless}],
    Config = [{versions, ['tlsv1.3']}, {session_tickets, stateless}, {verify, verify_peer}, {fail_if_no_peer_cert, true}, {log_level, debug}],
    start_link(9999, Config).

start_link(Port, AddConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AddConfig], []).

send(Data) ->
    gen_server:cast(?MODULE, {send, Data}).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

accept() ->
    gen_server:cast(?MODULE, accept).

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
    {ok, _} = application:ensure_all_started(ssl),
    {ok, ListenSocket} = ssl:listen(Port, TlsOptions),
    logger:debug("Accepting on port ~p", [Port]),
    case accept(ListenSocket) of
        {ok, Socket} ->
            {noreply, State#state{listen_socket = ListenSocket, client_socket = Socket}};
        _ ->
            {noreply, State#state{listen_socket = ListenSocket}}
    end.

handle_call(Command, _From, State) ->
    logger:debug("Call: ~p", [Command]),
    {reply, ok, State}.

handle_cast({send, Data}, #state{client_socket = Socket} = State) ->
    logger:debug("Sending data to client: ~p", [Data]),
    ssl:send(Socket, Data),
    {noreply, State};
handle_cast(accept, #state{listen_socket = ListenSocket} = State) ->
    %% if could not accept then let it crash
    {ok, Socket} = accept(ListenSocket),
    {noreply, State#state{client_socket = Socket}};
handle_cast({stop, Reason}, State) ->
    logger:debug("Stopping with reason ~w", [Reason]),
    {stop, Reason, State};
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

accept(ListenSocket) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, TLSTransportSocket} ->
            logger:debug("Client socket connected"),
            case ssl:handshake(TLSTransportSocket) of
                {ok, Socket} ->
                    {ok, Socket};
                {error, Error} ->
                    logger:error("Error while doing handshake ~p", [Error]),
                    {error, Error};
                SomethingElse ->
                    logger:warning("Unknown response from handshake ~p", [SomethingElse]),
                    {error, unknown_response}
            end;
        {error, Error} ->
            logger:error("Error while accepting socket ~p", [Error]),
            {error, Error};
        SomethingElse ->
            logger:warning("Unknown response during accept ~p", [SomethingElse]),
            {error, unknown_response}
    end.
