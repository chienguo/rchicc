#!/usr/bin/env bash
set -euo pipefail

BIN="${BIN:-./target/debug/rchicc}"

cargo build --quiet --bin rchicc

assert() {
    local input="$1"
    local expected="$2"
    local tmpdir

    tmpdir="$(mktemp -d)"

    "$BIN" "$input" >"${tmpdir}/tmp.s"
    gcc -o "${tmpdir}/tmp" "${tmpdir}/tmp.s"

    set +e
    "${tmpdir}/tmp"
    local actual=$?
    set -e

    if [[ "${actual}" -ne "${expected}" ]]; then
        echo "expected ${expected}, but got ${actual}" >&2
        rm -rf "${tmpdir}"
        exit 1
    fi

    echo "$input => $expected"
    rm -rf "${tmpdir}"
}

assert 0 0
assert 42 42
assert 255 255

echo "OK"
