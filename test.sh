#!/bin/env bash


read -p "Put password: " passwd

export PASSWD="$passwd"

/bin/bash <<EOF

echo $PASSWD

EOF



