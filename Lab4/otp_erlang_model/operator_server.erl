-module(operator_server).
-author("Semen").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(operator_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #operator_server_state{}} | {ok, State :: #operator_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #operator_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #operator_server_state{}) ->
  {reply, Reply :: term(), NewState :: #operator_server_state{}} |
  {reply, Reply :: term(), NewState :: #operator_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #operator_server_state{}} |
  {noreply, NewState :: #operator_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #operator_server_state{}} |
  {stop, Reason :: term(), NewState :: #operator_server_state{}}).

handle_call({call_client, N}, _From, State) ->
  timer:sleep(1000),
  Reply = operator_lib:call_client(N),
  gen_server:cast(client_server,{go_to_operator, N}),
  {reply, Reply, State};

handle_call({take_me_card, N}, _From, State) ->
  timer:sleep(1000),
  Reply = operator_lib:take_me_card(N),
  gen_server:cast(client_server,{give_your_card, N}),
  {reply, Reply, State};

handle_call(_Request, _From, State = #operator_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #operator_server_state{}) ->
  {noreply, NewState :: #operator_server_state{}} |
  {noreply, NewState :: #operator_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #operator_server_state{}}).
handle_cast(_Request, State = #operator_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #operator_server_state{}) ->
  {noreply, NewState :: #operator_server_state{}} |
  {noreply, NewState :: #operator_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #operator_server_state{}}).
handle_info(_Info, State = #operator_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #operator_server_state{}) -> term()).
terminate(_Reason, _State = #operator_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #operator_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #operator_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #operator_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
