#!/bin/bash
# Sets up some reasonable default parameters for Klatch.

export ENVOY_AMQP_EXCHANGE=klatch
export ENVOY_AMQP_IN_QUEUE=envoy-in
export ENVOY_AMQP_OUT_QUEUE=envoy-out
export EMBASSY_LOG=$HOME/.klatch-embassy

"$@"
