awk 'NR % 2 == 1 { o=$0 ; next } { print o "|" $0 }' Neves-t8-IRTF-cleaned.tsv  > Neves-t8-IRTF-final.tsv


