#!/usr/bin/bash
set -e

# Check if user gave args
if [ $# -eq 0 ]
  then
    echo "Provide A Payment Address."
    exit
fi

myString=$(./bech32 <<< ${1} | cut -c3-)

IFS='' read -r -a array <<< "$myString"

echo ${myString:0:56}