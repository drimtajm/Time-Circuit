%%%-------------------------------------------------------------------
%%% @author  <pi@datorbebis>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Apr 2014 by  <pi@datorbebis>
%%%-------------------------------------------------------------------
-module(countdown_clock).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, set_destination_time/2]).
-export([start_link/0, stop/0, set_destination_time/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,
	 calculate_time_difference/2]).

-define(SERVER, ?MODULE).
-define(HW_ADDRESS, 16#64).
-define(REMOTE_NODE, 'time_circuit@192.168.2.3').

-define(DEBUG, true).
-define(debug(String, Args),
	case ?DEBUG of
	    true  -> io:format("Trace: ~p: " ++ String,
			       [?MODULE | Args]);
	    _Else -> ok
	end).
-define(debug(String), ?debug(String, [])).

-define(do(M, F, A, Distributed),
	case Distributed of
	    {distributed, true}  -> rpc:call(?REMOTE_NODE, M, F, A);
	    {distributed, false} -> erlang:apply(M, F, A)
	end).

-record(state, {destination_time :: calendar:datetime(),
		current_time     :: calendar:datetime(),
		timediff         :: {integer(), integer(),
				     calendar:time()},
	        i2c_handle       :: pos_integer(),
		timer_ref        :: timer:tref()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?debug("start_link/0~n"),
    start_link({distributed, false}).
start_link({distributed, _Distributed}=D) ->
    ?debug("start_link/1, ~p~n", [D]),
    ?do(gen_server, start_link, [{global, ?SERVER}, ?MODULE, [], []], D).

stop() ->
    ?debug("stop/0~n"),
    stop({distributed, false}).
stop({distributed, _Distributed} = D) ->
    ?debug("stop/1, ~p~n", [D]),
    ?do(gen_server, call, [{global, ?SERVER}, stop], D).

set_destination_time(DestinationTime) ->
    ?debug("set_destination_time/1~n"),
    set_destination_time(DestinationTime, {distributed, false}).
set_destination_time(DestinationTime, {distributed, _Distributed} = D) ->
    ?debug("set_destination_time/2 ~p~ndestination time: ~p~n",
	   [D, DestinationTime]),
    ?do(gen_server, call, [{global, ?SERVER},
                           {set_destination_time, DestinationTime}], D).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Handle} = i2c_interface:open_i2c_bus(?HW_ADDRESS),
    i2c_interface:write_i2c_smbus_byte(Handle, 1, 0),
    %% go to normal operation, global intensity
    i2c_interface:write_i2c_smbus_byte(Handle, 4, 1),
    %% set global intensity
    i2c_interface:write_i2c_smbus_byte(Handle, 2, 15),
    {ok, #state{i2c_handle=Handle, destination_time=no_value}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({set_destination_time, DT}, _From,
	    #state{i2c_handle = Handle} = State) ->
    ?debug("Destination time: ~p~n", [DT]),
    {CT, Diff} = update_destination_time(DT, Handle),
    {ok, TRef} = timer:send_interval(100, check_time),
    {reply, ok, State#state{current_time=CT, destination_time=DT,
			    timer_ref = TRef,
			    timediff = Diff}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unexpected_message_received, Msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(ten_second_countdown, #state{destination_time=DT,
					 timediff=OldTimeDiff,
					 i2c_handle=Handle}=State) ->
    {_MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    TimeDiff = calculate_time_diff({Seconds, MicroSeconds div 10000}, DT),
    case TimeDiff of
	stop  ->
	    {ok, cancel} = timer:cancel(State#state.timer_ref),
	    time_circuit_lib:write_seconds_extended(Handle, {0, 0}),
	    {noreply, State#state{timer_ref=undefined}};
	OldTimeDiff ->
	    {noreply, State};
	_Else ->
	    time_circuit_lib:write_seconds_extended(Handle, TimeDiff),
	    {noreply, State#state{timediff=TimeDiff}}
    end;
    
handle_info(check_time, #state{destination_time = {_Date, {_H, _M, _S0}},
			       timer_ref=TimerRef,
			       timediff={0, 0, {0, 0, Ss}}}=State)
  when Ss =< 10 ->
    {ok, cancel} = timer:cancel(TimerRef),
    {_MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    DestinationTime = {Seconds+10, 0},
    TimeDiff = calculate_time_diff({Seconds, MicroSeconds div 10000},
				   DestinationTime),
    {ok, TRef} = timer:send_interval(5, ten_second_countdown),
    {noreply, State#state{timer_ref=TRef, destination_time=DestinationTime,
			  timediff=TimeDiff}};

handle_info(check_time, #state{current_time=CT,
			       timediff={Ys, Ds, {Hs, Ms, Ss}},
			       i2c_handle=Handle}=State) ->
    NewCT = erlang:localtime(),
    case NewCT of
	CT ->
	    {noreply, State};
	_Else ->            %% Suppose we don't miss a second
	    NewTimeDiff =
		case {Ms, Ss} of
		    {0, 0} ->
			NewHs = Hs - 1,
			NewMs = 59,
			NewSs = 59,
			time_circuit_lib:write_hours(Handle, NewHs),
			time_circuit_lib:write_minutes(Handle, NewMs),
			time_circuit_lib:write_seconds(Handle, NewSs),
			{Ys, Ds, {NewHs, NewMs, NewSs}};
		    {_, 0} ->
			NewMs = Ms - 1,
			NewSs = 59,
			time_circuit_lib:write_minutes(Handle, NewMs),
			time_circuit_lib:write_seconds(Handle, NewSs),
			{Ys, Ds, {Hs, NewMs, NewSs}};
		    {_, _} ->
			NewSs = Ss - 1,
			time_circuit_lib:write_seconds(Handle, NewSs),
			{Ys, Ds, {Hs, Ms, NewSs}}
		end,
	    {noreply, State#state{current_time=NewCT,
				  timediff=NewTimeDiff}}
    end;
handle_info(check_time, State) ->
    %% unflushed check_time under 10 second countdown, ignore
    {noreply, State};
handle_info(Msg, State) ->
    {stop, {unexpected_info_received, Msg}, State}.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{i2c_handle=Handle,
			  timer_ref=TRef}=_State) ->
    ?debug("in terminate~n"),
    timer:cancel(TRef),
    i2c_interface:write_i2c_smbus_byte(Handle, 4, 0),
    i2c_interface:close_i2c_bus(Handle),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(calculate_time_difference(Time1 :: calendar:datetime(),
				Time2 :: calendar:datetime()) ->
	     {ok, Sign, {Years :: pos_integer(),
			 Days  :: pos_integer(),
			 HoursMinutesSeconds :: calendar:time()}} when
      Sign :: '+' | '-'). %% Plus means that Time1 is greater/later than Time2
calculate_time_difference(Time1, Time2) ->
    Seconds1 = calendar:datetime_to_gregorian_seconds(Time1),
    Seconds2 = calendar:datetime_to_gregorian_seconds(Time2),
    Seconds = Seconds1 - Seconds2,
    if (Seconds < 0) ->
	    %% Just change the sign of the calculated difference
	    {ok, '+', Diff} = calculate_time_difference(Time2, Time1),
	    {ok, '-', Diff};
       true ->
	    {_Days0, Time} = calendar:seconds_to_daystime(Seconds),
	    {{Year1, Month1, Day1}, _T1} = Time1,
	    {{Year2, Month2, Day2}, _T2} = Time2,
	    case is_after_or_on({Month1, Day1}, {Month2, Day2}) of
		true ->
		    D1a = calendar:date_to_gregorian_days({Year1, Month1, Day1}),
		    D2a = calendar:date_to_gregorian_days({Year1, Month2, Day2}),
		    Daysa = D1a - D2a,
		    Yearsa = Year1 - Year2,
		    {ok, '+', {Yearsa, Daysa, Time}};
		false ->
		    D1b = calendar:date_to_gregorian_days({Year1+1, Month2, Day2}),
		    D2b = calendar:date_to_gregorian_days({Year1, Month1, Day1}),
		    Daysb = D1b - D2b,
		    Yearsb = Year1 - Year2 - 1,
		    {ok, '+', {Yearsb, Daysb, Time}}
	    end
    end.

is_after_or_on(Date, Date) ->
    true;
is_after_or_on({Month1, _Day1}, {Month2, _Day2}) when Month1 > Month2 ->
    true;
is_after_or_on({Month, Day1}, {Month, Day2}) when Day1 > Day2 ->
    true;
is_after_or_on(_Date1, _Date2) ->
    false.

update_destination_time(DestinationTime, I2CHandle) ->
    CurrentTime = erlang:localtime(),
    {ok, _Sign, Diff} = calculate_time_difference(DestinationTime, CurrentTime),
    {Years, Days, {Hours, Minutes, Seconds}} = Diff,
    time_circuit_lib:write_years(I2CHandle, Years),%% Sign),
    time_circuit_lib:write_days(I2CHandle, Days),
    time_circuit_lib:write_hours(I2CHandle, Hours),
    time_circuit_lib:write_minutes(I2CHandle, Minutes),
    time_circuit_lib:write_seconds(I2CHandle, Seconds),
    time_circuit_lib:clear_centiseconds(I2CHandle),
    {CurrentTime, Diff}.
    
calculate_time_diff({Seconds1, _Hundreds}=_CurrentTime,
		    {Seconds2, 0}=_DestinationTime) when Seconds1 >= Seconds2 ->
    stop;
calculate_time_diff({Seconds1, 0}=_CurrentTime,
		    {Seconds2, 0}=_DestinationTime) ->
    {Seconds2-Seconds1, 0};
calculate_time_diff({Seconds1, Hundreds}=_CurrentTime,
		    {Seconds2, 0}=_DestinationTime) ->
    {Seconds2-Seconds1-1, 100-Hundreds}.
