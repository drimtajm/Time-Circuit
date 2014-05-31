Time-Circuit
============

An event-countdown clock with a retro design from Back To The Future (Note: HW required)

Note
------------
This software is in "hack" status. The code is not pretty nor well-designed or even complete.
However, the code works for target HW and there are some thoughts behind the design (three gen_server modules).
It is simply in need of some proper refactoring.

TODO
------------
* Add some benchmarking to check performance of I2C operations through Erlang.
* Factor out a driver for the MAXIM display driver so that it can be rewritten to support both I2C and SPI variants.
* Add support for setting destination time with a keypad.
* Solve the "absence of a hw-clock" issue - either add support for an RTC module or time through GPS
* Make countdown "reverse" at zero and count upwards as well (i.e. how long time has passed since "destination time"?)
* Add support for day, month and year stepping in countdown. Right now, the countdown just ticks down seconds, minutes and hours.
* Add better/more generic support for distribution, maybe control an array of countdown clocks.
* Maybe add start/stop functionality through pushbuttons (and start at boot) so there is no need to log in.
