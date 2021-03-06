#!/bin/bash

if [[ $# != 1 ]]
then
  echo 'Usage : download_input.sh problem_number' >&2
  exit 1
fi

problem_number=$1
url='https://adventofcode.com/2019/day/'$problem_number'/input' 
output_filename=inputs/"$problem_number".txt
curl --silent "$url" -H 'Cookie: '"$ADVENT_SESSION_COOKIE" > "$output_filename"
