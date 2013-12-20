#!/bin/bash
LINE1="{\"contents\":[\"$1\",\"USER $1 8 * :$2\\r\\n\"],\"tag\":\"Send\"}"
LINE2="{\"contents\":[\"$1\",\"NICK $2\\r\\n\"],\"tag\":\"Send\"}"
echo $LINE1 | ./send-command.sh
echo $LINE2 | ./send-command.sh
