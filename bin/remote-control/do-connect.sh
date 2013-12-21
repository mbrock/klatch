#!/bin/bash
LINE="{\"contents\":[\"$1\",\"$2\",\"$3\"],\"tag\":\"Connect\"}"
echo $LINE | bin/remote-control/send-command.sh
