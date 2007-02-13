#!/bin/bash

# we start by the independant and invert alpha
# iR * (1 - (alpha / 255))

set Color 0
set Alpha 0
set answer 0

set fd [open "alpha.inc" "w"]

puts $fd ""
puts $fd "static double HCD_IAlpha\[255\]\[255\] = \{"



while {$Color < 255} {
	set Alpha 0

	while {$Alpha < 255} {
		set answer [expr "[double $Color] * (1 - ([double $Alpha] / 255))"]
		
		if {$Color == 254 && $Alpha == 254} {
			puts $fd " $answer\}"
		} else {
			if {$Alpha == 0} {
				puts -nonewline $fd "		\{$answer,"
			} else {

				if {$Alpha == 254} {
					puts $fd " $answer\},"
				} else {
					puts -nonewline $fd " $answer,"
				}
			}
		}

		incr Alpha
	}

	incr Color
}

puts $fd "\};"


# now we do the dependant normal alpha
# aR * (alpha / 255)

puts $fd ""
puts $fd ""

set Color 0
set Alpha 0
set answer 0

puts $fd "static double HCD_Alpha\[255\]\[255\] = \{"


while {$Color < 255} {
	set Alpha 0

	while {$Alpha < 255} {
		set answer [expr "[double $Color] * ([double $Alpha] / 255)"]
		
		if {$Color == 254 && $Alpha == 254} {
			puts $fd " $answer\}"
		} else {
			if {$Alpha == 0} {
				puts -nonewline $fd "		\{$answer,"
			} else {

				if {$Alpha == 254} {
					puts $fd " $answer\},"
				} else {
					puts -nonewline $fd " $answer,"
				}
			}
		}

		incr Alpha
	}

	incr Color
}

puts $fd "\};"

close $fd
