awk 'NR % 2 == 1 { o=$0 ; next } { print o "|" $0 }' cleaned-Newton2014IPAC.tsv > final-Newton2014-IPAC.tsv


