#!/bin/env bash
# Stats 506, Fall 2020
# <1> Update the header with your information. 
# This script serves as a template for Part 1 of 
# the shell scripting activity for week 1. 
#
# Author(s): James Henderson
# Updated: September 13, 2020
# 79: -------------------------------------------------------------------------

# a - download data if not present
#<2> Uncomment the lines below and fill in the file name and url. 
file="recs2015_public_v4.csv" 
url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

## if the file doesn't exist
if [ ! -f $file ]; then
   wget $url
fi

# b - extract header row and output to a file with one name per line
new_file="recs_names.txt"

## delete new_file if it is already present
if [ -f $new_file ]; then
  rm $new_file
fi

# <4> Write your one liner below.  Consider testing in multiple steps

# head -n1 $file | tr , '\n' > $new_file

# c - get column numbers for DOEID and the BRR weights
# as a comma separated string to pass to `cut`
# <5> write your one liner below
# < $new_file  grep -n "DOEID\|BRR" | cut -f1 -d: | paste -s -d,


# <6> uncomment the next three lines and copy the one liner above
cols=$(
< $file  head -n1 | tr , '\n' | grep -n "DOEID\|BRR" | cut -f1 -d: | paste -s -d,
)

# Uncomment the line below for testing and development
echo $cols

# d - cut out the appropriate columns and save as recs_brrweights.csv

< $file cut -f$cols -d, > recs_brrweights.csv

# 79: -------------------------------------------------------------------------
