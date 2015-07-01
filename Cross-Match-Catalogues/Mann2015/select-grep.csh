#!/bin/tcsh

grep -v "RESOURCE" $1 | grep -v "Name:" | grep -v "Title" | grep -v "INFO" | grep -v "MatchedRows" | grep -v "#Table" | grep -v "Gaiamag" | grep -v "^---" | grep -v "Rsun" | grep -v '#$' | grep -v '^$' > cleaned-$1

	
