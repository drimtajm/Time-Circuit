-module(time_circuit_lib).

-compile(export_all).
-define(display_addresses, [{{{slot, 1}, {pos, '1'}},  {address, 16#20}},
			    {{{slot, 1}, {pos, '1a'}}, {address, 16#28}},
			    {{{slot, 1}, {pos, '2'}},  {address, 16#21}},
			    {{{slot, 1}, {pos, '2a'}}, {address, 16#29}},
			    {{{slot, 2}, {pos, '1'}},  {address, 16#22}},
			    {{{slot, 2}, {pos, '1a'}}, {address, 16#2a}},
			    {{{slot, 2}, {pos, '2'}},  {address, 16#23}},
			    {{{slot, 2}, {pos, '2a'}}, {address, 16#2b}},
			    {{{slot, 3}, {pos, '1'}},  {address, 16#24}},
			    {{{slot, 3}, {pos, '1a'}}, {address, 16#2c}},
			    {{{slot, 3}, {pos, '2'}},  {address, 16#25}},
			    {{{slot, 3}, {pos, '2a'}}, {address, 16#2d}},
			    {{{slot, 4}, {pos, '1'}},  {address, 16#26}},
			    {{{slot, 4}, {pos, '1a'}}, {address, 16#2e}},
			    {{{slot, 4}, {pos, '2'}},  {address, 16#27}},
			    {{{slot, 4}, {pos, '2a'}}, {address, 16#2f}}]).

write_8segment_digit(Handle, Slot, Position, Value) ->
    {address, DisplayAddress} =
	proplists:get_value({{slot, Slot}, {pos, Position}},
			    ?display_addresses),
    i2c_interface:write_i2c_smbus_byte(Handle, DisplayAddress, Value).

write_8segment_digit_nodecode(Handle, Slot, Position, Value) ->
    write_8segment_digit_nodecode(Handle, Slot, Position, Value, false).
write_8segment_digit_nodecode(Handle, Slot, Position, Value, LightDC) ->
    DecodedValue0 = case Value of
			0 -> 2#01111110;
			1 -> 2#00110000;
			2 -> 2#01101101;
			3 -> 2#01111001;
			4 -> 2#00110011;
			5 -> 2#01011011;
			6 -> 2#01011111;
			7 -> 2#01110000;
			8 -> 2#01111111;
			9 -> 2#01111011
		    end,
    DecodedValue = case LightDC of
		       true  -> DecodedValue0 + 2#10000000;
		       false -> DecodedValue0
		   end,
    {address, DisplayAddress} =
	proplists:get_value({{slot, Slot}, {pos, Position}},
			    ?display_addresses),
    i2c_interface:write_i2c_smbus_byte(Handle, DisplayAddress, DecodedValue).
    
clear_8segment_digit_nodecode(Handle, Slot, Position) ->
    {address, DisplayAddress} =
	proplists:get_value({{slot, Slot}, {pos, Position}},
			    ?display_addresses),
    i2c_interface:write_i2c_smbus_byte(Handle, DisplayAddress, 0).

write_16segment_character(Handle, Slot, Position, Char) when Position == '1';
							     Position == '2' ->
    {address, DisplayAddress} =
	proplists:get_value({{slot, Slot}, {pos, Position}},
			    ?display_addresses),
    i2c_interface:write_i2c_smbus_byte(Handle, DisplayAddress, Char+16#41-$a).

write_years(Handle, Years) when Years >= 0 ->
    Digit1 = (Years rem 1000) div 100,
    Digit2 = (Years rem 100) div 10,
    Digit3 = Years rem 10,
    case Digit1 of
	0 -> clear_8segment_digit_nodecode(Handle, 4, '1');
	_ -> write_8segment_digit_nodecode(Handle, 4, '1', Digit1)
    end,
    case Digit2 of
	0 -> clear_8segment_digit_nodecode(Handle, 3, '1');
	_ -> write_8segment_digit_nodecode(Handle, 3, '1', Digit2)
    end,
    write_8segment_digit_nodecode(Handle, 3, '2', Digit3).    
    
write_days(Handle, Days) when Days >= 0 ->
    Digit1 = (Days rem 1000) div 100,
    Digit2 = (Days rem 100) div 10,
    Digit3 = Days rem 10,
    case Digit1 of
	0 -> clear_8segment_digit_nodecode(Handle, 3, '1a');
	_ -> write_8segment_digit_nodecode(Handle, 3, '1a', Digit1)
    end,
    case Digit2 of
	0 -> clear_8segment_digit_nodecode(Handle, 3, '2a');
	_ -> write_8segment_digit_nodecode(Handle, 3, '2a', Digit2)
    end,
    write_8segment_digit_nodecode(Handle, 4, '1a', Digit3).    

write_hours(Handle, Hours) when Hours >= 0,
				Hours < 24 ->
    Digit1 = Hours div 10,
    Digit2 = Hours rem 10,
    case Digit1 of
	0 -> clear_8segment_digit_nodecode(Handle, 1, '1');
	_ -> write_8segment_digit_nodecode(Handle, 1, '1', Digit1)
    end,
    write_8segment_digit_nodecode(Handle, 1, '2', Digit2).

write_minutes(Handle, Minutes) when Minutes >= 0,
				    Minutes < 60 ->
    Digit1 = Minutes div 10,
    Digit2 = Minutes rem 10,
    case Digit1 of
	0 -> clear_8segment_digit_nodecode(Handle, 1, '1a');
	_ -> write_8segment_digit_nodecode(Handle, 1, '1a', Digit1)
    end,
    write_8segment_digit_nodecode(Handle, 1, '2a', Digit2).

write_seconds(Handle, Seconds) when Seconds >= 0,
				    Seconds < 60 ->
    Digit1 = Seconds div 10,
    Digit2 = Seconds rem 10,
    case Digit1 of
	0     -> clear_8segment_digit_nodecode(Handle, 2, '1');
        _Else -> write_8segment_digit_nodecode(Handle, 2, '1', Digit1)
    end,
    write_8segment_digit_nodecode(Handle, 2, '2', Digit2).

write_seconds_extended(Handle, {10, Hundreds}) ->
    write_8segment_digit_nodecode(Handle, 2, '1', 1),
    write_8segment_digit_nodecode(Handle, 2, '2', 0, true),
    write_centiseconds(Handle, Hundreds);
write_seconds_extended(Handle, {Seconds, Hundreds}) ->
    clear_8segment_digit_nodecode(Handle, 2, '1'),
    write_8segment_digit_nodecode(Handle, 2, '2', Seconds, true),
    write_centiseconds(Handle, Hundreds).    

write_centiseconds(Handle, CentiSeconds) when CentiSeconds >= 0,
					      CentiSeconds < 100 ->
    Digit1 = CentiSeconds div 10,
    Digit2 = CentiSeconds rem 10,
    write_8segment_digit_nodecode(Handle, 2, '1a', Digit1),
    write_8segment_digit_nodecode(Handle, 2, '2a', Digit2).    

clear_centiseconds(Handle) ->
    clear_8segment_digit_nodecode(Handle, 2, '1a'),
    clear_8segment_digit_nodecode(Handle, 2, '2a').

write_hour(Handle, Hour) when Hour >= 0,
			      Hour =< 23 ->
    Digit1 = Hour div 10,
    Digit2 = Hour rem 10,
    write_8segment_digit(Handle, 1, '1', Digit1),
    write_8segment_digit(Handle, 1, '2', Digit2).

tweak_hour_intensity(Handle, Intensity) ->
    tweak_intensity(Handle, 1, '1a', Intensity),
    tweak_intensity(Handle, 1, '2',  Intensity).

write_minute(Handle, Minute) when Minute >= 0,
				  Minute =< 59 ->
    Digit1 = Minute div 10,
    Digit2 = Minute rem 10,
    write_8segment_digit(Handle, 1, '1a', Digit1),
    write_8segment_digit(Handle, 1, '2a', Digit2).

tweak_minute_intensity(Handle, Intensity) ->
    tweak_intensity(Handle, 1, '2a', Intensity),
    tweak_intensity(Handle, 1, '1',  Intensity).

write_day(Handle, Day) when Day >= 1,
			    Day =< 31 ->
    Digit1 = Day div 10,
    Digit2 = Day rem 10,
    write_8segment_digit(Handle, 3, '1a', Digit1),
    write_8segment_digit(Handle, 3, '1', Digit2).

tweak_day_intensity(Handle, Intensity) ->
    tweak_intensity(Handle, 3, '1a', Intensity),
    tweak_intensity(Handle, 3, '1',  Intensity).

write_month(Handle, Month) when Month >= 1,
				Month =< 12 ->
    [Char1, Char2, Char3] = case Month of
				1  -> "jan";
				2  -> "feb";
				3  -> "mar";
				4  -> "apr";
				5  -> "may";
				6  -> "jun";
				7  -> "jul";
				8  -> "aug";
				9  -> "sep";
				10 -> "oct";
				11 -> "nov";
				12 -> "dec"
			    end,
    write_16segment_character(Handle, 3, '2', Char1),
    write_16segment_character(Handle, 4, '1', Char2),
    write_16segment_character(Handle, 4, '2', Char3).

tweak_month_intensity(Handle, Intensity) ->
    tweak_intensity(Handle, 3, '2', Intensity),
    tweak_intensity(Handle, 4, '1', Intensity),
    tweak_intensity(Handle, 4, '2', Intensity).

write_year(Handle, Year) ->
    Digit1 = Year div 1000,
    Digit2 = (Year rem 1000) div 100,
    Digit3 = (Year rem 100) div 10,
    Digit4 = Year rem 10,
    write_8segment_digit(Handle, 2, '1',  Digit1),
    write_8segment_digit(Handle, 2, '2', Digit2),
    write_8segment_digit(Handle, 2, '1a',  Digit3),
    write_8segment_digit(Handle, 2, '2a', Digit4).

tweak_year_intensity(Handle, Intensity) ->
    tweak_intensity(Handle, 2, '1', Intensity),
    tweak_intensity(Handle, 2, '1a', Intensity),
    tweak_intensity(Handle, 2, '2', Intensity),
    tweak_intensity(Handle, 2, '2a',  Intensity).

tweak_intensity(Handle, Slot, Position, Intensity)
  when Intensity >= 0, Intensity =< 15 ->
    {IRegister, Half} =
	case {Slot, Position} of
	    {1, '1'}  -> {16#10, low};
	    {1, '1a'} -> {16#14, low};
	    {1, '2'}  -> {16#10, high};
	    {1, '2a'} -> {16#14, high};
	    {2, '1'}  -> {16#11, low};
	    {2, '1a'} -> {16#15, low};
	    {2, '2'}  -> {16#11, high};
	    {2, '2a'} -> {16#15, high};
	    {3, '1'}  -> {16#12, low};
	    {3, '1a'} -> {16#16, low};
	    {3, '2'}  -> {16#12, high};
	    {3, '2a'} -> {16#16, high};
	    {4, '1'}  -> {16#13, low};
	    {4, '1a'} -> {16#17, low};
	    {4, '2'}  -> {16#13, high};
	    {4, '2a'} -> {16#17, high}
	end,
    {ok, OldIntensity} =
	i2c_interface:read_i2c_smbus_byte(Handle, IRegister),
    NewIntensity = case Half of
		       low  -> Intensity band 16#0F;
		       high -> Intensity bsl 4
		   end,
    case NewIntensity of
	OldIntensity -> noop;
	_Else        -> i2c_interface:write_i2c_smbus_byte(
			  Handle, IRegister,
			  (OldIntensity bor NewIntensity))
    end.
		       

		
