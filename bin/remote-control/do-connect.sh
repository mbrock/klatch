#!/bin/bash
LINE="{\"socket\":{\"Start\":{\"name\":\"$1\",\"host\":\"$2\",\"port\":$3}}}"
echo $LINE | bin/remote-control/send-command.sh
