#!/bin/bash
LINE="{\"contents\":[\"$1\",\"JOIN $2\\r\\n\"],\"tag\":\"Send\"}"
echo $LINE | ./send-command.sh
