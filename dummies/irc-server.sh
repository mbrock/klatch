#!/bin/bash
while true ; do
  ( while true ; do
     echo Hello! I am an IRC server, almost.
     sleep 1
     echo I keep saying the same thing.
     sleep 2
  done ) | nc -l $PORT
done
