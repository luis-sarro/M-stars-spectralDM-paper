#!/bin/csh

awk -F ',' '{print $5}' struct.csv > sptp.txt

