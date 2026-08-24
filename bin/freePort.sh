#!/usr/bin/env bash

set -u

usage() {
  echo "Usage: $(basename "$0") PORT" >&2
  exit 2
}

[ "$#" -eq 1 ] || usage

port=$1
case "$port" in
  ''|*[!0-9]*) usage ;;
esac

if [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
  echo "Port must be an integer from 1 to 65535." >&2
  exit 2
fi

if ! command -v lsof >/dev/null 2>&1; then
  echo "freePort.sh requires lsof." >&2
  exit 1
fi

pids=$(lsof -nP -t -i :"$port" 2>/dev/null | sort -u)

if [ -z "$pids" ]; then
  echo "Port $port is already free."
  exit 0
fi

if kill -KILL $pids; then
  echo "Freed port $port (killed PID(s): $(echo "$pids" | tr '\n' ' ' | sed 's/ $//'))."
else
  echo "Could not kill every process using port $port." >&2
  exit 1
fi
