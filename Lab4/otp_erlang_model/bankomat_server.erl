-module(bankomat_server).
-author("Semen").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(bankomat_server_state, {}).

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
  {ok, State :: #bankomat_server_state{}} | {ok, State :: #bankomat_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #bankomat_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #bankomat_server_state{}) ->
  {reply, Reply :: term(), NewState :: #bankomat_server_state{}} |
  {reply, Reply :: term(), NewState :: #bankomat_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #bankomat_server_state{}} |
  {noreply, NewState :: #bankomat_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #bankomat_server_state{}} |
  {stop, Reason :: term(), NewState :: #bankomat_server_state{}}).
handle_call(_Request, _From, State = #bankomat_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #bankomat_server_state{}) ->
  {noreply, NewState :: #bankomat_server_state{}} |
  {noreply, NewState :: #bankomat_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #bankomat_server_state{}}).

handle_cast({take_me_money, N}, State) ->
  timer:sleep(1000),
  bankomat_lib:take_me_money(N),
  gen_server:cast(client_server,{give_your_money, N}),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #bankomat_server_state{}) ->
  {noreply, NewState :: #bankomat_server_state{}} |
  {noreply, NewState :: #bankomat_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #bankomat_server_state{}}).
handle_info(_Info, State = #bankomat_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #bankomat_server_state{}) -> term()).
terminate(_Reason, _State = #bankomat_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #bankomat_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #bankomat_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #bankomat_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
