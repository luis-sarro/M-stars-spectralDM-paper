#!/bin/tcsh

grep -v "RESOURCE" $1 | grep -v "Name:" | grep -v "Title" | grep -v "INFO" | grep -v "MatchedRows" | grep -v "#Table" | grep -v '^LSPM' | grep -v "^---" | grep -v "h:m:s" | grep -v '#$' | grep -v '^$' > cleaned-$1

	
