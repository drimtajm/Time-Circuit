-module(main).

-export([go/0, go/1]).

go() ->
    go([]).

go(Options) ->
    Duration = proplists:get_value(duration, Options, long),
    Distributed = proplists:get_value(distributed, Options, false),
    DestinationTime = {{2014, 6, 10}, {14, 55, 0}},
    start_servers(DestinationTime, Distributed),
    countdown_clock:set_destination_time(DestinationTime, {distributed, Distributed}),  
  case Duration of
	long ->
	    timer:sleep(30000),
	    change_destination_time(Distributed),
	    timer:sleep(200000);
	short ->
	    timer:sleep(10000)
    end,
    stop_servers(Distributed),
    ok.

start_servers(DestinationTime, Distributed) ->
    {ok, _CDServerPid} =
	countdown_clock:start_link({distributed, Distributed}),
%%    {ok, _DTServerPid} =
%%	destination_time:start_link(DestinationTime,
%%				    {distributed, Distributed}),
%%    {ok, _CTServerPid} = current_time:start_link(),
    ok.

stop_servers(Distributed) ->
    countdown_clock:stop({distributed, Distributed}),
%%    destination_time:stop(),
%%    current_time:stop().
ok.

change_destination_time(Distributed) ->
    %% set destination time to roughly 2 minutes from now
    {Date, {HH, MM, _SS}} = erlang:localtime(),
    {NewHH, NewMM} = if MM > 57 -> {HH+1, MM+2 rem 60};
			true    -> {HH, MM+2}
		     end,
%%    destination_time:change_destination_time
countdown_clock:set_destination_time({Date, {NewHH, NewMM, 0}},
					     {distributed, Distributed}),
    ok.
