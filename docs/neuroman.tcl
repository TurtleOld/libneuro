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
# @name		-- the name the man page file name will be, necessary for all except
#		   the functions which automatically defaults to the function name.
#		   If this variable is present for a function prototype then it will
#		   override the function name with this name.
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
set version "2.0.7b"

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
	echo "   -i,--individual	saves each functions from FILES into their own man"
	echo "			pages instead of outputing to standard output."
	echo "   -v,--version		output the version of neuroman"
	echo "   -h,--help		output this help message"
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
			if {$elem == "-h" || $elem == "--help"} {
				showHelp
				return 0
			} elseif {$elem == "-v" || $elem == "--version"} {
				showVersion
				return 0
			} elseif {$elem == "-i" || $elem == "--individual"} {
				set individual 1
			} elseif {$elem == "-c"} {
				set got_config 1
			} elseif {$got_config == 1} {
				set got_config 0
				set config $elem
				set use_config 1
			} else {
				if {[string index $elem 0] == "-"} {
					echo "Invalid argument $elem"
					echo "use --help to see the list of valid arguments."
				} else {
					set files [linsert $files 1 "$elem"]
				}
			}
		}	

		return 1
	}

	return 0
}


# this function parses a C function prototype to remove its leading
# and ending brackets.
# it will not touch brackets that are not leading and ending.
proc arrangeFunction {function} {
	set t 0
	set i 0
	#set total [llength $function]
	#set found_start 0
	#set found_end 0
	#set coord_end 0

	# gets rid of all the commas
	#while {$t < [string length $function]} {
	#	set t [string first "," $function $t]
		
	#	if {$t == -1} {
	#		break;
	#	} else {
			#echo $t
	#		set function [string replace $function $t $t " "]
	#	}
	#	incr t
	#}

	# we find the first opening bracket
	set t [string first "(" $function]
	
	# if we found one, we remove it
	if {$t > 0} {
		set function [string replace $function $t $t " "]
	}

	# we find the last opening bracket
	set t [string last ")" $function]
	
	# if we found one, we remove it
	if {$t > 0} {
		set function [string replace $function $t $t " "]
	}


	# now we will kind of hack a way to assemble ()() function pointers
	# into an whole
	set t 0
	set found_begin 0
	set found_end 0
	set coord_begin 0
	set coord_end 0
	while {$t < [string length $function]} {
		set t [string first "(" $function $t]
		
		if {$t == -1} {
			break
		} elseif {$found_begin == 0} {
			#echo $t
			#set function [string replace $function [expr $t - 1] [expr $t - 1] "\{"]
			#set coord_begin $t
			set found_begin 1
			set found_end 0
		}


		set t [string first ")" $function $t]

		if {$t == -1} {
			break
		} elseif {$found_end == 1} {
			#echo $t
			#set function [string replace $function [expr $t + 1] [expr $t + 1] "\}"]
			set found_begin 0
		} else {
			incr found_end
		}

	}

	#puts $function

	# we get rid of the beginning extern if any
	if {[lindex $function 0] == "extern"} {
		set function [lrange $function 1 end]
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

proc parseComment {comment name sdescri ldescri options returnval example errors related} {
	upvar $name cname
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

		if {$word == "@name"} {
			pushData $ctype $buffer extra

			set ctype cname
	
			set buffer ""
		} elseif {$word == "@sdescri"} {
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

proc genMan_Unique {comment} {
	global individual
	global config
	set name ""
	set msmall_description ""
	set mdescription ""
	set moptions ""
	set mreturnv ""
	set mexamples ""
	set merrors ""
	set mseealso ""
	set config_i [parseConfig $config]


	# by default we output to stdout (default screen)
	set fp stdout

	# we get rid of the /* character in the string
	set comment [string trim $comment "/*"]

	# populate our variables
	parseComment $comment name msmall_description mdescription moptions mreturnv mexamples merrors mseealso


	if {$name == ""} {
		puts "Error : missing a @name element from a comment"
		return 
	}

	# we get rid of any stars in the name string
	set name [string trim $name "*"]
	set name [string trim $name " * "]
	set name [string trim $name "* "]
	set name [string trim $name " *"]

	if {$individual == 1} {
		set fp [open "[string trim $name \"*\"].[lindex $config_i 0]" w]
		#set fp [open "$name.[lindex $config 0]" w]
	}

	replace_Stars_By_Void name
	
	puts $fp ".TH $name [lindex $config_i 0] \"[lindex $config_i 1]\" \"[lindex $config_i 2]\" \"[lindex $config_i 3]\""
	puts $fp ".SH NAME"

	if {[llength $msmall_description] > 0} {
		replace_Stars_By_Void msmall_description
		puts $fp ".TP"
		puts $fp "[string trim $name \"*\"]"
		puts $fp "\- $msmall_description"
	} else {
		puts $fp "[string trim $name \"*\"]"
	}	

	if {[llength $mdescription] > 0} {
		puts $fp ".SH DESCRIPTION"
		
		#replace_Stars_By_Void mdescription
		
		replace_Stars_By_Space mdescription
		
		puts $fp [subst "$mdescription"]
	}	

	if {[llength $mreturnv] > 0 && [lindex $function 0] != ""} {

		replace_Stars_By_Void mreturnv
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

		replace_Stars_By_Void merrors

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

proc genMan {comment function} {
	global individual
	global config
	set name ""
	set msmall_description ""
	set mdescription ""
	set moptions ""
	set mreturnv ""
	set mexamples ""
	set merrors ""
	set mseealso ""
	set config_i [parseConfig $config]

	# we get rid of any { or } characters
	set function [eval "concat $function"]

	# by default we output to stdout (default screen)
	set fp stdout

	# arranges the function data so its easy to handle
	set function [arrangeFunction $function]

	# we get rid of the /* character in the string
	set comment [string trim $comment "/*"]

	# populate our variables
	parseComment $comment name msmall_description mdescription moptions mreturnv mexamples merrors mseealso


	if {$name == ""} {
		if {[lindex $function 1] == ""} {
			# an error
			return 
		}
		set name [lindex $function 1]

	}

	# we get rid of any stars in the name string
	set name [string trim $name "*"]
	set name [string trim $name " * "]
	set name [string trim $name "* "]
	set name [string trim $name " *"]

	if {$individual == 1} {
		set fp [open "[string trim $name \"*\"].[lindex $config_i 0]" w]
		#set fp [open "$name.[lindex $config 0]" w]
	}

	replace_Stars_By_Void name
	
	puts $fp ".TH $name [lindex $config_i 0] \"[lindex $config_i 1]\" \"[lindex $config_i 2]\" \"[lindex $config_i 3]\""
	puts $fp ".SH NAME"

	if {[llength $msmall_description] > 0} {
		replace_Stars_By_Space msmall_description
		puts $fp ".TP"
		puts $fp "[string trim $name \"*\"]"
		puts $fp "\- [subst $msmall_description]"
	} else {
		puts $fp "[string trim $name \"*\"]"
	}


	#echo $function
	if {[lindex $function 0] != ""} {
		puts $fp ".SH SYNOPSIS"
		puts $fp "[lindex $function 0] $name\([lrange $function 2 [llength $function]]\)"
	}

	if {[string length $mdescription] > 0} {
		puts $fp ".SH DESCRIPTION"
		#replace_Stars_By_Void mdescription

		replace_Stars_By_Space mdescription


		puts $fp [subst $mdescription]
	}

	# The arguments... beats me why I initially called that
	# options...
	if {[llength $moptions] > 0 && [lindex $function 0] != ""} {
		set ototal [llength $moptions]
		set i 1
		set cfunc 3

		replace_Stars_By_Void moptions

		#echo "$moptions"

		puts $fp ".SH ARGUMENTS"

		while {$i < $ototal} {
			set ctype [lindex $moptions [expr "$i - 1"]]
			set t 0
			set is_funcptr 0

			if {$ctype == 0} {
				set type "(input)"
			} elseif {$ctype == 1} {
				set type "(output)"
			} else {
				set type "(input and output)"
			}

			#set nfunc [string trim [lindex $function $cfunc] "*"]
			set nfunc [lindex $function $cfunc]

			# we check to see if the current argument is a pointer to
			# a function and if yes, we do something special to include
			# all its arguments
			if {[string index $nfunc 0] == "("} {
				set is_funcptr 1
			}

			if {$is_funcptr == 1} {
				set go_on 1
				set found_one 0

				set str $nfunc
				while { $go_on == 1 } {
					set cur [string last ")" $str]

					# this is a special case where the function pointer
					# has no arguments at all.
					# like : void (*callback)()
					if {$cur > 0} {
						if {[lindex $function [expr $cur - 1]] == "("} {
							break;
						} elseif {$found_one == 1} {
							break;
						} else {
							incr found_one
						}
					}
					
					incr cfunc
					set str [lindex $function $cfunc]

					set nfunc [linsert $nfunc [llength $nfunc] $str]
					#echo "SPECIAL $nfunc  -> $str"

				}
			}

			# we remove the last comma if we find one
			set t [string last "," $nfunc]

			#echo "cur $cfunc last [llength $function]"
			if {$t > 0 && [expr [llength $function] - 1] > $cfunc} {
				set nfunc [string replace $nfunc $t $t " "]
			}

			puts $fp ".TP"
			puts $fp ".BI \"$nfunc \" $type"
			puts $fp "[lindex $moptions $i]\n"

			incr cfunc 2

			incr i 2
		}
	}

	if {[llength $mreturnv] > 0 && [lindex $function 0] != ""} {

		replace_Stars_By_Void mreturnv
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

		replace_Stars_By_Void merrors

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


# "/**" gather  -> "**/" stop gather
# OR "*/" stop gather and look for anything except "/**"
# after a "/**" "*/" combination (next line)
# look for "extern" gather2   -> ";"  stop gather2 dump in function ...

# the part action can include :
#	none : none yet (put that when nothing is needed anymore)
#	gather : starts buffering inside the buffer part. And starts matching in the
#		 end_string_list list.
#	childonly : we got a match in the end_string_list so we process only in it
#	process_data : tells the algo that the current branch got a complete set of
#			matching patterns so it can now be processed. If this action
#			is choosen, the extra part is used to know which function to
#			call with what arguments.
#	gather_line : exactly like gather but it doesn't match anything, it stops after
#			a line change. It uses the extra part for the function to call.
#			it also uses the current_action element to the current line number.
#
# the end_string part is a list with a list of strings that can be matched.
# and the next element is the child elements exactly like the MATCH list.
#
# buffer is the current buffer which is filled only when the begin_string is
# matched. Filling it only ends when a match is found inside end_string_list
#
# begin_string_list action buffer current_action extra end_string_list

# take note that the general order is important for correct dependencies
# the more dependencies a match has, the more it is to the end.
proc MATCH_format {} {

	set MATCH_Handled_Test "{\"houba\" gather_line \"\" none \"genMan_Test\" {} }"

	set MATCH_Handled_Comment_End_Unique "{\"**/\" process_data \"\" none \"genMan_Unique\" {} }"

	set MATCH_Function_Prototype_End "{\";\" process_data \"\" none \"genMan\" {}}"

	set MATCH_Function_Prototype "{\"extern\" gather \"\" none \"\" {\
			$MATCH_Function_Prototype_End} }"

	set MATCH_Handled_Comment_End "{\"*/\" childonly \"\" none \"\" {\
			$MATCH_Function_Prototype} }"


	set MATCH_Handled_Comment_Start "{\"/**\" gather \"\" none \"\" {\
			$MATCH_Handled_Comment_End_Unique \
			$MATCH_Handled_Comment_End} }"


	set MATCH_list "$MATCH_Handled_Comment_Start $MATCH_Handled_Test"

	#set i 0
	#foreach parent $MATCH_list {
	#	MATCH_process "/**" MATCH_list "" $parent $i

		#puts "$parent"

	#	incr i
	#}

	#MATCH_process "/**" MATCH_list
	#MATCH_process "hello" MATCH_list
	#MATCH_process "*/" MATCH_list
	#MATCH_process "extern" MATCH_list
	#MATCH_process ";" MATCH_list

	#puts "---> $MATCH_list"

	#MATCH_resetProcess MATCH_list 0 0

	return $MATCH_list
}

proc MATCH_process {stringtm thelist} {
	upvar $thelist tlist
	set i 0
	set depth 0
	set num 0

	#puts "$tlist\n"
	#puts "fetched whole \"[Fetch_Cascade_Whole $tlist 5 {0} 0 1]\""
	#puts "fetched data \"[Fetch_Cascade_Data $tlist 5 0 {0 1} 1]\""
	
	#puts "\nChanging data"
	#set tlist [Set_Cascade_Data $tlist 5 3 "childonly" {0 1 0 0} 3]
	#puts "fetched modified data \"[Fetch_Cascade_Data $tlist 5 3 {0 1 0 0} 3]\""

	#puts "STRING -> $stringtm"

	MATCH_loopProcess $stringtm tlist 0 0
}

proc MATCH_resetProcess {thelist num depth} {
	upvar $thelist tlist

	#puts "will process num $num depth $depth"
	
	while {1 != 2} {
		set cData [Fetch_Cascade_Whole $tlist 5 $num $depth 1]

		#puts $cData

		if {$cData == -1 || $cData == ""} {
			break;
		}

		foreach elem $cData {
			set child_num 0

			# we reset the buffer elemenet
			set tlist [Set_Cascade_Data $tlist 5 2 "" $num $depth]
			
			# we reset the current_action element
			set tlist [Set_Cascade_Data $tlist 5 3 "none" $num $depth]


			
			set child_num $num

			lappend child_num 0


			#puts "child num : $child_num  -- depth [expr $depth + 1]"
				
			MATCH_resetProcess tlist $child_num [expr $depth + 1]

			lset num $depth [expr [lindex $num $depth] + 1]
		}
	}
}

proc MATCH_loopProcess {stringtm thelist num depth} {
	upvar $thelist tlist

	#puts "will process num $num depth $depth string $stringtm"
	
	while {1 != 2} {
		set cData [Fetch_Cascade_Whole $tlist 5 $num $depth 1]

		#puts $cData

		if {$cData == -1 || $cData == ""} {
			break;
		}

		foreach elem $cData {
			set child_num 0

			#puts [Fetch_Cascade_Data $tlist 5 0 $num $depth]
			set _err [MATCH_subprocess $stringtm tlist $num $depth]

			if {$_err == 0} {
				# we call a subsequent child

				#lset num $depth [expr [lindex $num $depth] + 1]
			
				set child_num $num

				lappend child_num 0


				#puts "child num : $child_num  -- depth [expr $depth + 1]"
				
				if {[MATCH_loopProcess $stringtm tlist $child_num [expr $depth + 1]] == 1} {
					return 1
				}

				#puts "finished child run"
				
				# we don't need to run on the next nodes
				return 0
			} elseif {$_err == 2} {
				return 1
			} else {
				lset num $depth [expr [lindex $num $depth] + 1]
			}
		}

		break
	}
}

# this function calls itself
# if the current node needs to call subsequent childs, it will return 0
# if not, it will return 1
proc MATCH_subprocess {stringtm thelist num depth} {
	upvar $thelist tlist
	set node [Fetch_Cascade_Whole $tlist 5 $num $depth]

	#puts "node [lindex $node 0]  string $stringtm"
	
	switch [lindex $node 3] {
		none {
			foreach string_match [lindex $node 0] {
				#puts -nonewline "trying to see if $stringtm is $string_match "
				
				if {[ParseString $string_match $stringtm] == 1} {
					set output $tlist

					#puts " <- MATCH \n"

					switch [lindex $node 1] {

						none {
							return 1
						}

						gather {
							set tlist [Set_Cascade_Data $tlist 5 3 "gather" $num $depth]

						}

						childonly {
							set tlist [Set_Cascade_Data $tlist 5 3 "childonly" $num $depth]
							
							set tlist [Set_Cascade_Data $tlist 5 3 "childonly" [lrange $num 0 [expr $depth - 1]] [expr $depth - 1]]

							return 2
						}

						process_data {
							set i 0
							set buf ""
							# this is the "last" command set for a list of matches, thats why we reset the list
							# need to call the extra element with the necessary arguments

							while {$i < $depth} {
								
								if {[Fetch_Cascade_Data $tlist 5 1 [lrange $num 0 $i] $i] == "gather"} {
									lappend buf [Fetch_Cascade_Data $tlist 5 2 [lrange $num 0 $i] $i]
								}

								incr i
							}
							#puts $tlist
							#puts $buf

							eval "[lindex $node 4] $buf"
							
							#puts "we reset the process"
							
							MATCH_resetProcess tlist 0 0
						
							return 2
						}

						gather_line {
							# don't know yet how to handle this one

							return 1
						}


					}

					#puts "initial gathering action [lindex $node 0] -> $stringtm"

					set buf [lindex $node 2]

					lappend buf $stringtm

					set tlist [Set_Cascade_Data $tlist 5 2 $buf $num $depth]

					# we only need one match
					return 2
				} else {
					#puts "\n"
				}
			}

			return 1
		}

		gather {
			#puts "gathering action [lindex $node 0] -> $stringtm"

			set buf [lindex $node 2]

			lappend buf $stringtm

			set tlist [Set_Cascade_Data $tlist 5 2 $buf $num $depth]


			return 0
		}

		childonly {
			#puts "child only state [lindex $node 0]"

			return 0
		}

		process_data {
			puts "process data state? this is an error buddy!"
		}

		gather_line {
			puts "gather line state? this is an error buddy!"
		}
	}

	return 1
}

# sets a single data in a list/sublist corresponding to depth
# and number pattern
proc Set_Cascade_Data {alist sublist_num elem data num_in depth} {
	set i 0
	set num $num_in
	set child ""

	set i [expr $depth + 1]

	while {$i > 0} {
		set i [expr $i - 1]

		set current [Fetch_Cascade_Whole $alist $sublist_num $num $i]
		set whole [Fetch_Cascade_Whole $alist $sublist_num $num $i 1]

		if {$i == $depth} {
			lset current $elem $data
		} elseif {$child != ""} {
			#puts "$sublist_num --> $current -- $child"
			set current [lreplace $current $sublist_num $sublist_num $child]
		}

		lset whole [lindex $num_in $i] $current


		#puts "$i -- $current"
		#puts "$whole"

		set child $whole

		#set tempo [lreplace [Fetch_Cascade_Whole $alist $sublist_num $num $i 1] [lindex $num 0] [lindex $num 0] $current]
		#puts "$whole TEST --_-_->>  [lindex $whole [lindex $num_in $i]]"

		set num [lrange $num 0 end-1]
		#puts "DEBUG -- $num"
	}
	return $whole
}

# in a cascading list/sublist variable, output a full list corresponding
# to a certain depth and a number pattern.
proc Fetch_Cascade_Whole {alist sublist_num num_in depth {relative 0}} {
	set i 0
	set num ""

	# we first reverse the num variable
	set i [llength $num_in]

	foreach item $num_in {
		set num [linsert $num 0 $item]
	}

	set i 0

	#puts "Whole output : number -- [lindex $num $depth]"

	foreach item $alist {
		#puts "THERES #[llength $alist] elements total! ?$relative?"
		#puts "$i - [lindex $num $depth]"

		if {$i == [lindex $num $depth]} {
			
			#puts "PROCESSING -- $item"
			
			
			if {$depth == 0} {
				if {$relative == 0} {
					return $item
				} else {
					return $alist
				}
			} else {
				return [Fetch_Cascade_Whole [lindex $item $sublist_num] $sublist_num $num_in [expr $depth - 1] $relative]
			}
		}

		incr i
	}

	return -1
}

# in a cascading list/sublist variable, output a single element
# corresponding to a certain depth and a number pattern.
proc Fetch_Cascade_Data {alist sublist_num elem num_in depth} {


	set output [Fetch_Cascade_Whole $alist $sublist_num $num_in $depth]

	if {$output != -1} {
		if {$elem >= 0 && $elem < [llength $output]} {
			return [lindex $output $elem]
		}
	}

	return -1
}

proc parseFile {file config} {
	# gather 1 is used to gather the comment
	# data
	set gather1 ""
	# gather 2 is used to gather principally
	# function prototypes and in the future,
	# more.
	set gather2 ""
	# contains the current line number for use
	# by the code to know when a line changed
	set current_line 0 
	#global MATCH_list

	#puts [subst $MATCH_list]

	set matchform [MATCH_format]


	# we loop line by line, string by string and character by character

	# we continue until eof to loop line by line
	while {[eof $file] == 0} {
		set line [gets $file]
		set a 0
		
		# count the number of characters in the line
		set line_len [string length $line]

		# loop string by string in the current line
		while {$a < $line_len} {		
			set str [GetStringWord $line a]

			MATCH_process $str matchform
		}

		# we increment the current number of lines
		incr current_line
	}
}

# returns 0 if theres no matching strings
# and 1 if there is
proc ParseString {string_indep string_depen} {
	# current matching characters
	set match 0
	set i 0
	set len [string length $string_depen]

	#puts "Checking String \"$string_indep\" over \"$string_depen\""

	# loop character by character in the current string
	while {$i < $len} {
		set char [string index $string_depen $i]
		set indep [string index $string_indep $match]

		#puts "depen $char indep $indep"

		if {$char == $indep} {
			incr match
		} else {
			
			# special case where we check the former character with this one to see
			# if it matches... if it matches, it means we might have a redundant
			# character which was primarily matched.
			#
			# example : our indep string is */ and our input string is **/
			# without this code, the first * is matched then it checks the
			# second * over with / but it won't match and it restarts the check.
			# |  |
			# */ **/  1
			#  |  |
			# */ **/  0  (so the algo restarts)
			# |    |
			# */ **/  0  (see, this is a bit false because it was
			# reset and thus the string wasn't matched.)
			#
			if {$match > 0 && $char == [string index $string_indep [expr $match - 1]]} {
				set match 1
			} else {
				set match 0
			}
		}

		if {$match == [string length $string_indep]} {
			return 1
		}
		
		incr i
	}

	return 0
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

