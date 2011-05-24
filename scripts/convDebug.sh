#! /bin/bash

# NEURO_WARN
# NEURO_TRACE
# NEURO_ERROR

indent=' *\t* *\n*'

# first strip the NEURO_ part
# second, strip any ', NULL'
# third, any '"%s", Neuro_s(...)' to 'Neuro_s(...)'
# fourth, any '"%d", a' to 'Neuro_s("%d", a)'
work="s/NEURO_\(.*\)/\1/; \
		s/(\(\".*\"\), *NULL/(\1/; \
		s/(\"\(.*\)%s\(.*\)\",$indent$indent\(Neuro_s$indent($indent\)\"\(.*\)\"\(.*);\)/(\3\"\1\4\2\"\5/; \
		/Neuro_s/! s/(\(\".*\",.*\));/(Neuro_s(\1));/"

# for multiline functions
Mwork=": gather; /;/! {N; b gather}; /;/ {$work}"

for i in $@; do
	sed -e "/\/\*.*\*\// b" \
		-e "/\/\*.*\*\//! {/\/\*/,/\*\// b}" \
		-e "/NEURO_\(WARN\|TRACE\|ERROR\).*;/ $work" \
		-e "/NEURO_\(WARN\|TRACE\|ERROR\).*;/! { \
			/NEURO_\(WARN\|TRACE\|ERROR\)/,/;/ {$Mwork} \
			}" -i $i
done
