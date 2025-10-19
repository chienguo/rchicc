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

    rm -rf "${tmpdir}"
}

assert 0 0
assert 42 42
assert 255 255
assert "1+2-1" 2
assert " 10 + 20 - 5 " 25
assert "50 - 20 + 3" 33
assert "2*3" 6
assert "18/3" 6
assert "1+2*3" 7
assert "(1+2)*3" 9
assert "2*(3+4)" 14
assert "50/(2*5)" 5
assert "(3+5)/2" 4
assert "5*(9-6)" 15
assert "5+6*7" 47
assert "-3 + 5" 2
assert "-(3+5) + 10" 2
assert "+(2*3)" 6
assert "-3*2 + 12" 6
assert "- - +10" 10
assert "- -10" 10
assert "-10+20" 10
assert "1==1" 1
assert "0==1" 0
assert "2!=3" 1
assert "4<5" 1
assert "5<5" 0
assert "4<=4" 1
assert "5>=3" 1
assert "3>=4" 0
assert "3>2" 1
assert "3>5" 0
assert "1+2==3" 1
assert "2*(3==3)" 2

echo "OK"
