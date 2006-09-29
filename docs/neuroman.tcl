# copyright neuroponic 2006
# author : Nicholas Niro
# by using this piece of software, you accept that it
# has no warranty at all, use it at your own risk.
#
# permission is hereby granted to modify and distribute 
# this script as long as the original authoring is kept.






# documentation section, the syntax of the configuration file
# and the syntax of the comments in the header files.

# configuration file syntax

# the configuration file requires 5 lines
# the first line is the name of the program/library
# the firs line is the section number 
#	here are all the sections with a description but you will 
#	generally use the section 3.
#	Section
#	1    User commands
#	2    System calls, that is, functions provided by the kernel.
#	3    Subroutines, that is, library functions.
#	4    Devices, that is, special files in the /dev directory.
#	5    File format descriptions, e.g. /etc/passwd.
#	6    Games, self-explanatory.
#	7    Miscellaneous, e.g. macro packages, conventions.
#	8    System administration tools that only root can execute.
#	9    Another
#	n    New documentation, that may be moved to a more appropriate section.
#	l    Local documentation referring to this particular system.
# the second line is the center footer
# the third line is the left footer
# and the last line is the center header

# here is an example configuration file
# 3
# 02 Sep 2006
# v0.44.0
# Libneuro Functions



# the syntax inside the header files

# first of all, the script will parse the comments that are 
# just above a function prototype which starts with the extern keyword
# and it will only parse those with an extra star ie /**
# so to make this script work good you need to have this minimum:
#/**
# *
# *
# */
#extern void somefunc(int foo, int bar);

# the actual stuff that will be put in the man page will need to be enclosed
# between the /** and the */ this is how the script will know the end of the
# things to parse.

# the script will also fetch data from the extern prototype too, it will 
# get the function name, return type, variable types and names it will parse
# until it goes to the ending ';' and it will ignore any ( and ) characters.

# inside the /** */ comments, a special "language" has to be used which uses
# commands or variables. commands/variables always start with the character '@'
# for example : @description
# the script will parse all the current data (the text) into the last "command"
# issued.
# heres the complete list of those commands/variables and what they do :
#
# take note that those commands can be entered in any order.
#
#
#
#
# @sdescri	-- this is the small description, the summary that should be about 1
# 		   sentence.
# @description	-- this is the exhaustive description, it can be of any length you want.
# @param	-- this command is special, you can not just write @param , you also have
#		   to put the param's "direction" (input output or both) in the form
#		   @param[...] (see below) You don't need to specify which function 
#		   argument the param goes to, it goes sequentially and remember to put a 
#		   description for each @param[...].
# @param[in]	-- the input type see @param
# @param[out]	-- the output type see @param
# @param[io]	-- both input and output see @param
# @related	-- text after this (which should be functions or such) will be put in the see also
#		   section.
# @examples	-- text after this is put in the code examples section.
# @returnval	-- text after this is put in the return value section.
# @errors	-- text after this is put in the error codes section.



# -- constants --
# put the version here
set version "0.7.3"

# -- global variables --

set individual 0
set use_config 0
set files ""
set config ""

proc showHelp { } {
	echo "Usage : neuroman \[OPTIONS\] ... \[FILES\]"
	echo "A very simple tcl header file parser to the man format."
	echo "The documentation on how to format the comments before"
	echo "the functions is inside this script."
	echo "   -c file	input the configuration file"
	echo "   -i		saves each functions in FILES into their own man page"
	echo "   -v		output the version of neuroman"
	echo "   -h		output this help message"
	echo
	echo "report bugs to neuroman-bugs@neuroponic.com"
}

proc showVersion { } {
	global version
	echo "neuroman version $version"
}

proc handleArgs { } {
	global argc
	global argv
	global individual
	global files
	global config
	global use_config
	set got_config 0
	if {$argc == 0} {
		showHelp
		return 0
	} else {

		#we check and handle the arguments
		foreach elem $argv {
			if {$elem == "-h"} {
				showHelp
				return 0
			} elseif {$elem == "-v"} {
				showVersion
				return 0
			} elseif {$elem == "-i"} {
				set individual 1
			} elseif {$elem == "-c"} {
				set got_config 1
			} elseif {$got_config == 1} {
				set got_config 0
				set config $elem
				set use_config 1
			} else {
				set files [linsert $files 1 "$elem"]
			}
		}	

		return 1
	}

	return 0
}

proc arrangeFunction {function} {
	set t 0
	set total [llength $function]


	while {$t < $total} {
		set str [lindex $function $t]
		set i 0
		set len [string length $str]

		while {$i < $len} {
			set c [string index $str $i]

			if {$c == "(" || $c == ")"} {

				set newstr [string replace $str $i $i " "]
				#echo $newstr
				set trail [lindex $newstr 1]
				set newstr [lindex $newstr 0]
				set function [lreplace $function $t $t $newstr]
				#echo $newstr [string length $newstr]
				if {[string length $trail] > 0} {
					set function [linsert $function [expr "$t + 1"] $trail]
					incr total
				}

				if {[string length $newstr] == 0} {
					set function [lrange $function 0 [expr "$total - 2"]]
					set total [expr "$total - 1"]
				}

				set str $newstr
			}

			incr i
		}

		incr t
	}

	return $function
}

proc pushData {storage toadd extra} {
	upvar $storage buffer
	upvar $extra xtra

	if {$storage == ""} {
		return
	}

	if {$xtra == ""} {
		set buffer $toadd
	} else {
		set buffer "$buffer $xtra {$toadd}"

		set xtra ""
	}
}

proc parseComment {comment sdescri ldescri options returnval example errors related} {
	upvar $sdescri small_d
	upvar $ldescri long_d
	upvar $options opt
	upvar $returnval retv
	upvar $example exmp
	upvar $errors err
	upvar $related rela

	set ctype ""
	set buffer ""
	set extra ""

	foreach word $comment {

		#puts $word

		#set word [string trim $word \"*\"]

		if {$word == "@sdescri"} {
			pushData $ctype $buffer extra

			set ctype small_d
	
			set buffer ""
		} elseif {$word == "@description"} {
			pushData $ctype $buffer extra

			set ctype long_d
	
			set buffer ""
		} elseif {$word == "@param\[in\]"} {
			pushData $ctype $buffer extra

			set ctype opt
			set extra 0
	
			set buffer ""
		} elseif {$word == "@param\[out\]"} {
			pushData $ctype $buffer extra

			set ctype opt
			set extra 1
	
			set buffer ""
		} elseif {$word == "@param\[io\]"} {
			pushData $ctype $buffer extra

			set ctype opt
			set extra 2
	
			set buffer ""
		} elseif {$word == "@related"} { 
			pushData $ctype $buffer extra

			set ctype rela
	
			set buffer ""
		} elseif {$word == "@examples"} {
			pushData $ctype $buffer extra

			set ctype exmp
	
			set buffer ""
		} elseif {$word == "@returnval"} {
			pushData $ctype $buffer extra

			set ctype retv
	
			set buffer ""
		} elseif {$word == "@errors"} {
			pushData $ctype $buffer extra

			set ctype err
	
			set buffer ""
		} else {
			set buffer "$buffer $word"
		}
	}

	pushData $ctype $buffer extra

}

proc parseOptions {comment options} {
	upvar $options opt


}

proc GetStringWord { str current} {
	#returns the word it fetched
	#current needs to be the variable itself, not the data
	upvar $current a
	set slen [string length $str]
	set sepchar " "
	set outword ""
	set initial $a

	while {$a < $slen} {
		set c [string index $str $a]

		if {$c != " "} {
			set outword "$outword$c"
			#echo "outword : $outword  initial $initial current $a char : $c"
		} else {
			break;
		}

		incr a
	}	

	incr a 
	return $outword
}

proc replace_Stars_By_Void {str} {
	upvar $str tochange
	set i 0
	
	#echo "string to change : $tochange"

	foreach word $tochange {
		#echo $word	

		if {$word == "*"} {
			lset tochange $i [format "\b"]
			#echo "matched $word"
		} elseif {[llength $word] > 1 } {
			#echo "calling itself"
			replace_Stars_By_Void word
			lset tochange $i $word
		}

		incr i
	}
	#echo "string changed(finished product) : $tochange"
}

proc replace_Stars_By_Space {str} {
	upvar $str tochange
	set i 0
	set a 0
	
	#echo "string to change : \"$tochange\""

	set len [string length $tochange]

	#while {$a < $len} {
	#	set word [GetStringWord $tochange a]

	#	echo "Replace_Stars_By_Space -> $word \[$a\]"
	#	if {$word == "*"} {
	#		set tochange [string replace $tochange $a $a ""]
	#		#echo "matched $word"
	#	}
	#}

	while {$i < $len} {
		set a $i
		set i [string first "*" $tochange $i]

		if {$i != $a && $i != -1} {
			set before [string index $tochange [expr $i - 1]]
			set after [string index $tochange [expr $i + 1]]

			#echo "BEFORE \"$before\" char \"[string index $tochange $i]\"  AFTER \"$after\" \[$i\]"

			if {$before == ""} {
				set before " "
			}

			if {$after == ""} {
				set after " "
			}

			if {$before == " " && $after == " "} {
				set tochange [string replace $tochange $i $i "\b"] 
			} else {
				incr i
			}
		} else {
			break
		}
	}

	#echo "string changed(finished product) : $tochange"
}

proc genMan {comment function config} {
	global individual
	set msmall_description ""
	set mdescription ""
	set moptions ""
	set mreturnv ""
	set mexamples ""
	set merrors ""
	set mseealso ""

	# by default we output to stdout (default screen)
	set fp stdout

	# debug test
	#set moptions {{2} {test}}

	#echo $comment
	#echo $function

	# arranges the function data so its easy to handle
	set function [arrangeFunction $function]

	# populate our variables
	parseComment $comment msmall_description mdescription moptions mreturnv mexamples merrors mseealso

	#echo $function

	#echo "config $config config"

	if {$individual == 1} {
		set fp [open "[string trim [lindex $function 1] \"*\"].[lindex $config 0]" w]
	}
		
	
	
	
	puts $fp ".TH [lindex $function 1] [lindex $config 0] \"[lindex $config 1]\" \"[lindex $config 2]\" \"[lindex $config 3]\""
	puts $fp ".SH NAME"

	if {[llength $msmall_description] > 0} {
		replace_Stars_By_Void msmall_description
		puts $fp ".TP"
		puts $fp "[string trim [lindex $function 1] \"*\"]"
		puts $fp "\- $msmall_description"
	} else {
		puts $fp "[string trim [lindex $function 1] \"*\"]"
	}


	puts $fp ".SH SYNOPSIS"
	puts $fp "[lindex $function 0] [lindex $function 1]([lrange $function 2 [llength $function]])"
	if {[llength $mdescription] > 0} {
		puts $fp ".SH DESCRIPTION"
		replace_Stars_By_Void mdescription
		puts $fp "$mdescription"
	}

	# The arguments... beats me why I initially called that
	# options...
	if {[llength $moptions] > 0} {
		set ototal [llength $moptions]
		set i 1
		set cfunc 3

		#echo "$moptions"

		replace_Stars_By_Void moptions

		puts $fp ".SH ARGUMENTS"

		while {$i < $ototal} {
			set ctype [lindex $moptions [expr "$i - 1"]]

			if {$ctype == 0} {
				set type "(input)"
			} elseif {$ctype == 1} {
				set type "(output)"
			} else {
				set type "(input and output)"
			}

			set nfunc [string trim [lindex $function $cfunc] "*"]
			set nfunc [string trim $nfunc ","]
			puts $fp ".TP"
			puts $fp ".BI \"$nfunc \" $type"
			puts $fp "[lindex $moptions $i]\n"

			incr cfunc 2

			incr i 2
		}
	}

	if {[llength $mreturnv] > 0} {
		puts $fp ".SH RETURN VALUES"
		puts $fp "$mreturnv"
	}

	if {[string length $mexamples] > 0} {
		puts $fp ".SH EXAMPLE(S)"

		replace_Stars_By_Space mexamples

		#puts $fp [subst -nocommands -nobackslashes -novariables $mexamples]
		puts $fp [subst $mexamples]
	}
	
	if {[llength $merrors] > 0} {
		puts $fp ".SH ERRORS"
		puts $fp "$merrors"
	}

	if {[llength $mseealso] > 0} {
		puts $fp ".SH SEE ALSO"
		replace_Stars_By_Void mseealso
		puts $fp "$mseealso"
	}


	
	
	
	if {$individual == 1} {
		close $fp
	}

}

proc parseFile {file config} {
	# buffers the valid comment
	set cbuffer ""
	# buffers the function
	set fbuffer ""
	set cbegin "/**"
	set cend "*/"
	set fbegin "extern "
	set fend ";"
	
	# current state 0 is nothing
	# 1 is we might have a comment that began
	# 2 we have a comment that is undergoing
	# 3 we have a comment, looking for a function
	# 4 is we might have a function that began
	# 5 we have a function that is undergoing
	# 6 we have a comment and function
	set state 0
	set blen 0

	# we continue until eof
	while {[eof $file] == 0} {
		set line [gets $file]
		set a 0

		# to avoid tcl errors
		#set line [string trim $line "\{"]
		#set line [string trim $line "\}"]

		# count the number of elements (strings) in the list
		#set telems [llength "$line"]
		
		# count the number of characters in the string/list
		set line_len [string length $line]

		#foreach str $line \{
		while {$a < $line_len} {
			
			set str [GetStringWord $line a]

			set i 0

			# undergoing add variable
			set ug_add 0

			set len [string length $str]

			while {$i < $len} {
				set char [string index $str $i]

				switch $state {
					0 {
						if { $char == [string index $cbegin $blen]} {
							# we have a starting comment type
							#set cbuffer [string index $line $i]
							set state 1
							incr blen

							set ug_add 1

						} 
					}

					1 {	
						if {$char == [string index $cbegin $blen] && $ug_add == 1} {
							#echo "$blen"
							incr blen

							if {$blen == [string length $cbegin]} {
								set state 2
								#echo "start an handle comment chunk"

								# we will remove the /** from the string
								if {$len > $blen} {
									set str [string range $str [string length $cbegin] $len]

								}

							}
						} else {
							set cbuffer ""
							set blen 0
							set state 0
						}
					}
					
					2 {
						set t 0
						set tlen 0

						if {$ug_add == 1} {
							break
						}
						
						while {$t < $len} {
							set tchar [string index $str $t]

							if {$tchar == [string index $cend $tlen]} {
								incr tlen

								if {$tlen == [string length $cend]} {
									#echo "end an handle comment chunk"
									set blen 0
									set state 3
								}
							}

							incr t
						}

						if {$state == 3} {
							# debug output

							# add the string to the buffer
							if {[string length $cend] < $len } {
								set cbuffer [linsert $cbuffer [llength $cbuffer] [string range $str 0 [expr "$len - [string length $cend] - 1"]]]
							}

							#echo $cbuffer

							#foreach st [lindex $cbuffer] {
							#	echo $st
							#}
						} else {
							# add the string to the buffer
							#echo "adding $str"
							set cbuffer [linsert $cbuffer [llength $cbuffer] $str]

							set ug_add 1
						}
					}

					3 {
						if {$char == [string index $fbegin $blen]} {
							# we have a starting function type
							#set fbuffer [string index $line $i]
							set state 4
							incr blen

							set ug_add 1
						}
					}

					4 {	
						#echo "len [string length $fbegin] $blen $char"

						if {$char == [string index $fbegin $blen] && $ug_add == 1} {
							#echo "$blen"
							incr blen

							set ug_add 1
							
							if {$blen == [string length $fbegin] } {
								set state 5
								#echo "start an handle function chunk"
							}
						} elseif {[string index $fbegin $blen] == " "  && $ug_add == 0} {
							# hack to recognise a space
							set state 5
							#echo "start an handle function chunk"
						} else {
							set fbuffer ""
							set blen 0
							set state 3
						}
					}

					5 {
						set t 0
						set tlen 0

						if {$ug_add == 1} {
							break
						}
						
						while {$t < $len} {
							set tchar [string index $str $t]

							if {$tchar == [string index $fend $tlen]} {
								incr tlen

								if {$tlen == [string length $fend]} {
									#echo "end an handle function chunk"
									set blen 0
									set state 6
								}
							}

							incr t
						}

						if {$state == 6} {
							# debug output

							# add the string to the buffer
							if {[string length $fend] < $len } {


								set fbuffer [linsert $fbuffer [llength $fbuffer] [string trimright $str $fend]]
							}

							#echo $fbuffer

							#foreach st [lindex $fbuffer] {
							#	echo $st
							#}
						} else {	
							set ug_add 1
							set fbuffer [linsert $fbuffer [llength $fbuffer] $str]
						}	

					}

					6 {

						genMan $cbuffer $fbuffer $config
						#echo "we have a comment type and a function type"

						set cbuffer ""
						set fbuffer ""
						set blen 0
						set state 0
					}
				}

				#echo $state
				
				incr i
			}
		}		
	}
}

proc parseConfig {file} {
	global use_config

	if { $use_config == 0} {
		# we output a default stock config
		set conf {{3} {"center footer"} {"left footer"} {"center header"}}
		return $conf
	} else {
		# we parse the configuration from the file
		set conf ""

		set fp [open $file r]

		while {[eof $fp] == 0} {
			set conf "$conf {[gets $fp]}"
		}

		close $fp

		return $conf
	}
}

proc handleFiles {} {
	global files
	global individual
	global use_config
	global config

	if {$use_config == 1} {
		# we first check if the configuration file exists
		set fp [open $config r]
		close $fp
	}

	# we check to see if the files exist or not
	foreach elem $files {
		set fp [open $elem r]
		
		close $fp
	}

	# now we need to open each of them and parse their content
	foreach elem $files {
		set fp [open $elem r]

		parseFile $fp [parseConfig $config]
		
		close $fp
	}
}

proc main { } {
	set err [handleArgs]


	if {$err == 0} {
		exit
	}

	handleFiles
}

main

