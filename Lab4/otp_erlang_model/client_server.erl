-module(client_server).
-author("Semen").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(client_server_state, {}).

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
  {ok, State :: #client_server_state{}} | {ok, State :: #client_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, client_lib}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #client_server_state{}) ->
  {reply, Reply :: term(), NewState :: #client_server_state{}} |
  {reply, Reply :: term(), NewState :: #client_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #client_server_state{}} |
  {noreply, NewState :: #client_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #client_server_state{}} |
  {stop, Reason :: term(), NewState :: #client_server_state{}}).

handle_call({give_me_ticket, N}, _From, State) ->
    {reply, client_lib:give_me_ticket(N), State};
handle_call(_Request, _From, State = #client_server_state{}) ->
  {reply, ok, State}.



%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #client_server_state{}) ->
  {noreply, NewState :: #client_server_state{}} |
  {noreply, NewState :: #client_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_server_state{}}).

handle_cast({go_to_operator, N}, State) ->
  timer:sleep(1000),
  client_lib:go_to_operator(N),
  gen_server:call(operator_server,{take_me_card, N}),
  {noreply, State};

handle_cast({give_your_card, N}, State) ->
  timer:sleep(1000),
  client_lib:take_your_card(N),
  gen_server:cast(bankomat_server,{take_me_money, N}),
  {noreply, State};

handle_cast({give_your_money, N}, _Mod) ->
  timer:sleep(1000),
  _Mod:take_your_money(N),
  gen_server:call(main_server, {client_go_to_home, N}),
  {noreply, _Mod};

handle_cast(_Request, State = #client_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #client_server_state{}) ->
  {noreply, NewState :: #client_server_state{}} |
  {noreply, NewState :: #client_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_server_state{}}).
handle_info(_Info, State = #client_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #client_server_state{}) -> term()).
terminate(_Reason, _State = #client_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #client_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #client_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, _Mod, _Extra) ->
  gen_event:notify(client_event, {code_change, _OldVsn}),
  {ok, _Extra}.

%%sys:suspend(client_server).
%%code:purge(client_server).
%%code:load_file(client_server).
%%sys:change_code(client_server, client_server, "0", client_lib2).
%%sys:resume(client_server).
%%application:loaded_applications().
%%application:start(main).
%%gen_server:call(main_server,{client_go_to_bank, 5}).


%%%===================================================================
%%% Internal functions
%%%===================================================================
