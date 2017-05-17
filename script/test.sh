#!/usr/bin/env bash

set -o nounset -o pipefail -o errexit

echo here i am

do_this() {
    echo "now I'm doing this"
}

trap do_this INT TERM EXIT

for i in $(seq 1 10); do
    sleep 3
done

trap - INT TERM EXIT
