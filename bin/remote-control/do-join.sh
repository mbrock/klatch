#!/bin/bash
LINE="{\"line\":{\"Send\":{\"name\":\"$1\",\"line\":\"JOIN $2\\r\\n\"}}}"
echo $LINE | bin/remote-control/send-command.sh
