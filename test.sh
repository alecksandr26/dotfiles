#!/bin/env bash

set -x

# Prompt user for input
read -r -p "Enter some input: " input_variable

# Echo the input to demonstrate it was captured correctly
echo "Input entered: $input_variable"

export PASSWD="$input_variable"

echo $PASSWD

/bin/bash <<EOF

echo "-----"
echo ""\$PASSWD""
echo "-----"

EOF



