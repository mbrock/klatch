#!/bin/bash
LINE1="{\"line\":{\"Send\":{\"name\":\"$1\",\"line\":\"USER $1 8 * :$2\\r\\n\"}}}"
LINE2="{\"line\":{\"Send\":{\"name\":\"$1\",\"line\":\"NICK $2\\r\\n\"}}}"
echo $LINE1 | bin/remote-control/send-command.sh
echo $LINE2 | bin/remote-control/send-command.sh
