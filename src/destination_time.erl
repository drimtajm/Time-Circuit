%%%-------------------------------------------------------------------
%%% @author  <pi@datorbebis>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Apr 2014 by  <pi@datorbebis>
%%%-------------------------------------------------------------------
-module(destination_time).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/0, change_destination_time/1,
	 change_destination_time/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HW_ADDRESS, 16#61).
-define(LED_PIN, 23).
-define(INITIAL_LED_VALUE, 0).

-record(state, {destination_time :: calendar:datetime(),
	        i2c_handle       :: pos_integer(),
		led_value        :: 0..1,
		timer_ref        :: timer:tref()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(DestinationTime) ->
	     {ok, Pid} | ignore | {error, Error} when
      DestinationTime :: calendar:datetime(),
      Pid             :: pid(),
      Error           :: term()).
start_link(DestinationTime) ->
    start_link(DestinationTime, {distributed, false}).
start_link({{_YY, _MM, _DD}, {_HH, _Mi, _SS}} = DestinationTime,
	   {distributed, _Distributed} = D) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DestinationTime, D], []).

stop() ->
    gen_server:call(?SERVER, stop).

change_destination_time(DestinationTime) ->
    change_destination_time(DestinationTime, {distributed, false}).
change_destination_time({{_YY, _MM, _DD}, {_HH, _Mi, _SS}} = DT, D) ->
    gen_server:call(?SERVER, {change_destination_time, DT, D}).

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
init([DT, D]) ->
    {ok, Handle} = i2c_interface:open_i2c_bus(?HW_ADDRESS),
    i2c_interface:write_i2c_smbus_byte(Handle, 4, 16#41),
    tweak_intensities(Handle),
    write_time(Handle, DT),
    countdown_clock:set_destination_time(DT, D),
    gpio:open_pin(?LED_PIN),
    gpio:set_output(?LED_PIN),
    gpio:write_pin(?LED_PIN, ?INITIAL_LED_VALUE),
    {ok, TRef} = timer:send_interval(500, toggle_leds),
    {ok, #state{i2c_handle=Handle, destination_time=DT,
		led_value=?INITIAL_LED_VALUE, timer_ref=TRef}}.

tweak_intensities(I2CHandle) ->
    time_circuit_lib:tweak_minute_intensity(I2CHandle, 15),
    time_circuit_lib:tweak_hour_intensity(I2CHandle, 15),
    time_circuit_lib:tweak_day_intensity(I2CHandle, 15),
    time_circuit_lib:tweak_month_intensity(I2CHandle, 0),
    time_circuit_lib:tweak_year_intensity(I2CHandle, 15),
    ok.

write_time(I2CHandle, {{Year, Month, Day},
		       {Hour, Minute, _Second}}) ->
    time_circuit_lib:write_day(I2CHandle, Day),
    time_circuit_lib:write_month(I2CHandle, Month),
    time_circuit_lib:write_year(I2CHandle, Year),
    time_circuit_lib:write_minute(I2CHandle, Minute),
    time_circuit_lib:write_hour(I2CHandle, Hour),
    ok.

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
handle_call({change_destination_time, DT, {distributed, _Dist} = D},
	    _From, State) ->
    write_time(State#state.i2c_handle, DT),
    countdown_clock:set_destination_time(DT, D),
    {reply, ok, State#state{destination_time=DT}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(toggle_leds, #state{led_value=CurrentValue}=State) ->
    NewValue = (CurrentValue + 1) rem 2,
    gpio:write_pin(?LED_PIN, NewValue),
    {noreply, State#state{led_value=NewValue}}.

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
    timer:cancel(TRef),
    i2c_interface:write_i2c_smbus_byte(Handle, 4, 0),
    i2c_interface:close_i2c_bus(Handle),
    gpio:close_pin(?LED_PIN),
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
