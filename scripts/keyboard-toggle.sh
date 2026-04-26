#!/bin/bash

layout=$(setxkbmap -query | grep layout | awk '{print $2}')

if [[ "$layout" == "us" ]]; then
    setxkbmap latam
else
    setxkbmap us
fi
