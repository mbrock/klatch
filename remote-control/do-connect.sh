#!/bin/bash
LINE="{\"contents\":[\"$1\",\"$2\",\"$3\"],\"tag\":\"Connect\"}"
echo $LINE | ./send-command.sh
