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

assert "0;" 0
assert "42;" 42
assert "255;" 255
assert "1+2-1;" 2
assert " 10 + 20 - 5 ;" 25
assert "50 - 20 + 3;" 33
assert "2*3;" 6
assert "18/3;" 6
assert "1+2*3;" 7
assert "(1+2)*3;" 9
assert "2*(3+4);" 14
assert "50/(2*5);" 5
assert "(3+5)/2;" 4
assert "5*(9-6);" 15
assert "5+6*7;" 47
assert "-3 + 5;" 2
assert "-(3+5) + 10;" 2
assert "+(2*3);" 6
assert "-3*2 + 12;" 6
assert "- - +10;" 10
assert "- -10;" 10
assert "-10+20;" 10
assert "1==1;" 1
assert "0==1;" 0
assert "2!=3;" 1
assert "4<5;" 1
assert "5<5;" 0
assert "4<=4;" 1
assert "5>=3;" 1
assert "return 3>=4;" 0
assert "3>2;" 1
assert "return 3>5;" 0
assert "1+2==3;" 1
assert "2*(3==3);" 2
assert "1;2;" 2
assert "1;2;3;" 3
assert "1+2;3+4;" 7
assert "10;20;return 30+5;" 35
assert "a=3; a;" 3
assert "a=3;z=5;a+z;" 8
assert "foo=7; foo;" 7
assert "foo=1;bar=2;foo+bar;" 3
assert "foo123=3; bar=5; return foo123+bar;" 8
assert "return 3;" 3
assert "x=2; return x+4;" 6
assert "return 2; x = 10;" 2
assert "return 1; 2; 3;" 1
assert "1; return 3;" 3
assert "{ 1; 2; }" 2
assert "{ x=1; { y=2; x+y; }; }" 3
assert "{ x=1; { return x; } x=5; }" 1
assert "{ a=3; return a;}" 3
assert "{ a=b=3; return a+b; }" 6
assert "{ foo123=3; bar=5; return foo123+bar; }" 8
assert "{ 1; 2; return 3;}" 3
# assert "{ return fo; }" 12


echo "OK"
