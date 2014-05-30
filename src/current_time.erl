%%%-------------------------------------------------------------------
%%% @author  <pi@datorbebis>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Apr 2014 by  <pi@datorbebis>
%%%-------------------------------------------------------------------
-module(current_time).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HW_ADDRESS, 16#60).
-define(LED_PIN, 24).
-define(INITIAL_LED_VALUE, 0).

-record(state, {current_time :: calendar:datetime(),
		i2c_handle   :: pos_integer(),
		led_value    :: 0..1,
		timer_ref    :: timer:tref()}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [erlang:localtime()], []).

stop() ->
    gen_server:call(?SERVER, stop).

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
init([{{_Year, _Month, _Day}, {_Hour, _Minute, _Second}} = CT]) ->
    {ok, Handle} = i2c_interface:open_i2c_bus(?HW_ADDRESS),
    i2c_interface:write_i2c_smbus_byte(Handle, 4, 16#41),
    tweak_intensities(Handle),
    write_time(Handle, CT),
    gpio:open_pin(?LED_PIN),
    gpio:set_output(?LED_PIN),
    gpio:write_pin(?LED_PIN, ?INITIAL_LED_VALUE),
    {ok, TRef} = timer:send_interval(500, check_time_and_toggle_leds),
    {ok, #state{i2c_handle=Handle, current_time=CT,
		led_value=?INITIAL_LED_VALUE, timer_ref=TRef}}.

tweak_intensities(I2CHandle) ->
    time_circuit_lib:tweak_minute_intensity(I2CHandle, 12),
    time_circuit_lib:tweak_hour_intensity(I2CHandle, 12),
    time_circuit_lib:tweak_day_intensity(I2CHandle, 12),
    time_circuit_lib:tweak_month_intensity(I2CHandle, 4),
    time_circuit_lib:tweak_year_intensity(I2CHandle, 12),
    ok.

write_time(I2CHandle, {{Year, Month, Day},
		       {Hour, Minute, _Second}} = _CT) ->
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
handle_info(check_time_and_toggle_leds,
	    #state{current_time={Date, {H, M, _S}},
		   i2c_handle=Handle, led_value = Value}=State) ->
    NewValue = (Value + 1) rem 2,
    gpio:write_pin(?LED_PIN, NewValue),
    NewCT = erlang:localtime(),
    case NewCT of
	{Date, {H, M, _S2}} ->
	    {noreply, State#state{led_value=NewValue}};
	{Date, {H, Minute, _S2}} ->
	    time_circuit_lib:write_minute(Handle, Minute),
	    {noreply, State#state{current_time=NewCT,
				  led_value=NewValue}};
	{Date, {Hour, Minute, _S2}} ->
	    time_circuit_lib:write_hour(Handle, Hour),	    
	    time_circuit_lib:write_minute(Handle, Minute),	    
	    {noreply, State#state{current_time=NewCT,
				  led_value=NewValue}}
    end;
handle_info(Msg, State) ->
    {stop, {unexpected_message_received, Msg}, State}.    

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
terminate(_Reason, #state{i2c_handle=Handle}=_State) ->
    gpio:close_pin(?LED_PIN),
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
