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
        echo "${input}"
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
assert "int a=3; a;" 3
assert "int a=3; int z=5; a+z;" 8
assert "int foo=7; foo;" 7
assert "int foo=1; int bar=2; foo+bar;" 3
assert "int foo123=3; int bar=5; return foo123+bar;" 8
assert "return 3;" 3
assert "int x=2; return x+4;" 6
assert "int x; return 2; x = 10;" 2
assert "return 1; 2; 3;" 1
assert "1; return 3;" 3
assert "{ 1; 2; }" 2
assert "{ int x=1; { int y=2; x+y; }; }" 3
assert "{ int x=1; { return x; } x=5; }" 1
assert "{ int a=3; return a;}" 3
assert "{ int a; int b; a=b=3; return a+b; }" 6
assert "{ int foo123=3; int bar=5; return foo123+bar; }" 8
assert "{ 1; 2; return 3;}" 3
assert "{ ; ; return 3;}" 3
# assert "{ return fo; }" 12
assert "if (1) return 5; return 3;" 5
assert "if (0) return 5; return 3;" 3
assert "if (1) { return 7; } else { return 9; }" 7
assert "{ int x=0; int y; if (x) y=1; else y=2; y; }" 2
assert "{ if (1) { 1; 2; return 3; } else { return 4;} }" 3
assert "{ if (0) { 1; 2; return 3; } else { return 4;} }" 4
assert "{ if (1-1) return 2; return 3; }" 3
assert "{ if (2-1) return 2; return 3; }" 2
assert "int i=0; for (i=0; i<3; i=i+1) ; return i;" 3
assert "int sum=0; int i=0; for (i=0; i<=3; i=i+1) sum=sum+i; return sum;" 6
assert "for (;0;) return 1; return 2;" 2
assert "{ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j;}" 55
assert "{ for (;;) {return 3;} return 5;}" 3
assert "int i=0; while (i<3) i=i+1; return i;" 3
assert "int sum=0; int i=0; while (i<=3) { sum=sum+i; i=i+1; } return sum;" 6
assert "while (0) return 1; return 2;" 2
assert "{ int a=3; int *p=&a; return *p; }" 3
assert "{ int a=3; int *p=&a; *p=5; return a; }" 5
assert "{ int a=1; int b=2; int *p=&a; int *q=&b; *p=*q+5; return a; }" 7
assert "{ int i=0; while(i<10) { i=i+1; } return i; }" 10
assert "{ int x=3; return *&x; }" 3
assert "{ int x=3; int *y=&x; int **z=&y; return **z; }" 3
assert "{ int x=3; int *y=&x; int **z=&y; return **z; }" 3
assert "{ int x=3; int y=5; return *(&x+1); }" 5
assert "{ int x=3; int y=5; return *(&y-1); }" 3
assert "{ int x=3; int y=5; return *(&x-(-1)); }" 5
assert "{ int x=3; int *y=&x; *y=5; return x; }" 5
assert "{ int x=3; int y=5; *(&x+1)=7; return y; }" 7
assert "{ int x=3; int y=5; *(&y-1)=7; return x; }" 7
assert "{ int x=3; return (&x+4)-(&x); }" 4
assert "{ int x=3; int y=5; return (&y)-(&x); }" 1
assert "{ int x=3; int y=5; return (&x+2)-&x+3; }" 5


echo "OK"
