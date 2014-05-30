%% This line tells emacs to use -*- erlang -*- mode for this file

{application, 'Time-Circuit',
 [{description, "Software for the Back-to-the-Future-style time circuit"},
  {vsn, "1.0"},
  {modules, [main, destination_time, current_time,
	     countdown_clock, time_circuit_lib]},
  {applications, [kernel, stdlib]}]}.
