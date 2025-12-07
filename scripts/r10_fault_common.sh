#!/usr/bin/env bash
# Common helper functions for R10 fault injection scripts
# Source this file in other scripts: source scripts/r10_fault_common.sh

set -euo pipefail

: "${ROUTER_NODE:?ROUTER_NODE must be set, e.g. router@127.0.0.1}"
: "${ROUTER_COOKIE:?ROUTER_COOKIE must be set}"

erl_rpc() {
  local module="$1"
  local function="$2"
  local args="$3"
  
  erl -noshell \
      -name "r10ctl_$$@127.0.0.1" \
      -setcookie "${ROUTER_COOKIE}" \
      -eval "io:format(\"Calling ~p:~p(~s)~n\", ['${module}', '${function}', \"${args}\"]), Result = rpc:call('${ROUTER_NODE}', ${module}, ${function}, ${args}), io:format(\"Result: ~p~n\", [Result]), halt()."
}

