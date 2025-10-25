#!/usr/bin/env bash
set -euo pipefail

BIN="${BIN:-./target/debug/rchicc}"

cargo build --quiet --bin rchicc

COLOR_INPUT=$'\033[1;36m'
COLOR_EXPECTED=$'\033[1;32m'
COLOR_RESET=$'\033[0m'

assert() {
    local input="$1"
    local expected="$2"
    printf 'testing on "%b%s%b", expect %b%s%b\n' \
        "${COLOR_INPUT}" "${input}" "${COLOR_RESET}" \
        "${COLOR_EXPECTED}" "${expected}" "${COLOR_RESET}"
    local tmpdir

    tmpdir="$(mktemp -d)"

    "$BIN" "$input" >"${tmpdir}/tmp.s"
    {
        for value in $(seq 0 50); do
            printf 'int ret%d(void) { return %d; }\n' "${value}" "${value}"
        done
    } >"${tmpdir}/tmp2.c"
    cat <<'EOF' >>"${tmpdir}/tmp2.c"
int add2(int a, int b) { return a + b; }
int add3(int a, int b, int c) { return a + b + c; }
int add4(int a, int b, int c, int d) { return a + b + c + d; }
int add5(int a, int b, int c, int d, int e) { return a + b + c + d + e; }
int add6(int a, int b, int c, int d, int e, int f) { return a + b + c + d + e + f; }
int mix6(int a, int b, int c, int d, int e, int f) { return (a - b) + (c - d) + (e - f); }
EOF
    gcc -static -o "${tmpdir}/tmp" "${tmpdir}/tmp.s" "${tmpdir}/tmp2.c"

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

assert $'int main() {
  return 0;
}' 0
assert $'int main() {
  return 42;
}' 42
assert $'int main() {
  return 255;
}' 255
assert $'int main() {
  return 1 + 2 - 1;
}' 2
assert $'int main() {
  return 10 + 20 - 5;
}' 25
assert $'int main() {
  return 50 - 20 + 3;
}' 33
assert $'int main() {
  return 2 * 3;
}' 6
assert $'int main() {
  return 18 / 3;
}' 6
assert $'int main() {
  return 1 + 2 * 3;
}' 7
assert $'int main() {
  return (1 + 2) * 3;
}' 9
assert $'int main() {
  return 2 * (3 + 4);
}' 14
assert $'int main() {
  return 50 / (2 * 5);
}' 5
assert $'int main() {
  return (3 + 5) / 2;
}' 4
assert $'int main() {
  return 5 * (9 - 6);
}' 15
assert $'int main() {
  return 5 + 6 * 7;
}' 47
assert $'int main() {
  return -3 + 5;
}' 2
assert $'int main() {
  return -(3 + 5) + 10;
}' 2
assert $'int main() {
  return +(2 * 3);
}' 6
assert $'int main() {
  return -3 * 2 + 12;
}' 6
assert $'int main() {
  return - - +10;
}' 10
assert $'int main() {
  return - -10;
}' 10
assert $'int main() {
  return -10 + 20;
}' 10
assert $'int main() {
  return 1 == 1;
}' 1
assert $'int main() {
  return 0 == 1;
}' 0
assert $'int main() {
  return 2 != 3;
}' 1
assert $'int main() {
  return 4 < 5;
}' 1
assert $'int main() {
  return 5 < 5;
}' 0
assert $'int main() {
  return 4 <= 4;
}' 1
assert $'int main() {
  return 5 >= 3;
}' 1
assert $'int main() {
  return 3 >= 4;
}' 0
assert $'int main() {
  return 3 > 2;
}' 1
assert $'int main() {
  return 3 > 5;
}' 0
assert $'int main() {
  return 1 + 2 == 3;
}' 1
assert $'int main() {
  return 2 * (3 == 3);
}' 2
assert $'int main() {
  1;
  return 2;
}' 2
assert $'int main() {
  1;
  2;
  return 3;
}' 3
assert $'int main() {
  1 + 2;
  return 3 + 4;
}' 7
assert $'int main() {
  10;
  20;
  return 30 + 5;
}' 35
assert $'int main() {
  int a = 3;
  return a;
}' 3
assert $'int main() {
  int a = 3;
  int z = 5;
  return a + z;
}' 8
assert $'int main() {
  int foo = 7;
  return foo;
}' 7
assert $'int main() {
  int foo = 1;
  int bar = 2;
  return foo + bar;
}' 3
assert $'int main() {
  int foo123 = 3;
  int bar = 5;
  return foo123 + bar;
}' 8
assert $'int main() {
  return 3;
}' 3
assert $'int main() {
  int x = 2;
  return x + 4;
}' 6
assert $'int main() {
  int x;
  return 2;
  x = 10;
}' 2
assert $'int main() {
  return 1;
  2;
  3;
}' 1
assert $'int main() {
  1;
  return 3;
}' 3
assert $'int main() {
  { 1; 2; }
}' 2
assert $'int main() {
  int x = 1;
  { int y = 2; x + y; };
}' 3
assert $'int main() {
  int x = 1;
  { return x; }
  x = 5;
}' 1
assert $'int main() {
  int a = 3;
  return a;
}' 3
assert $'int main() {
  int a;
  int b;
  a = b = 3;
  return a + b;
}' 6
assert $'int main() {
  int foo123 = 3;
  int bar = 5;
  return foo123 + bar;
}' 8
assert $'int main() {
  1;
  2;
  return 3;
}' 3
assert $'int main() {
  ;
  ;
  return 3;
}' 3
assert $'int main() {
  if (1) return 5;
  return 3;
}' 5
assert $'int main() {
  if (0) return 5;
  return 3;
}' 3
assert $'int main() {
  if (1) {
    return 7;
  } else {
    return 9;
  }
}' 7
assert $'int main() {
  int x = 0;
  int y;
  if (x) y = 1; else y = 2;
  return y;
}' 2
assert $'int main() {
  if (1) {
    1;
    2;
    return 3;
  } else {
    return 4;
  }
}' 3
assert $'int main() {
  if (0) {
    1;
    2;
    return 3;
  } else {
    return 4;
  }
}' 4
assert $'int main() {
  if (1 - 1) return 2;
  return 3;
}' 3
assert $'int main() {
  if (2 - 1) return 2;
  return 3;
}' 2
assert $'int main() {
  int i = 0;
  for (i = 0; i < 3; i = i + 1)
    ;
  return i;
}' 3
assert $'int main() {
  int sum = 0;
  int i = 0;
  for (i = 0; i <= 3; i = i + 1)
    sum = sum + i;
  return sum;
}' 6
assert $'int main() {
  for (;0;)
    return 1;
  return 2;
}' 2
assert $'int main() {
  int i = 0;
  int j = 0;
  for (i = 0; i <= 10; i = i + 1)
    j = i + j;
  return j;
}' 55
assert $'int main() {
  for (;;) {
    return 3;
  }
  return 5;
}' 3
assert $'int main() {
  int i = 0;
  while (i < 3)
    i = i + 1;
  return i;
}' 3
assert $'int main() {
  int sum = 0;
  int i = 0;
  while (i <= 3) {
    sum = sum + i;
    i = i + 1;
  }
  return sum;
}' 6
assert $'int main() {
  while (0) return 1;
  return 2;
}' 2
assert $'int main() {
  int a = 3;
  int *p = &a;
  return *p;
}' 3
assert $'int main() {
  int a = 3;
  int *p = &a;
  *p = 5;
  return a;
}' 5
assert $'int main() {
  int a = 1;
  int b = 2;
  int *p = &a;
  int *q = &b;
  *p = *q + 5;
  return a;
}' 7
assert $'int main() {
  int i = 0;
  while (i < 10) {
    i = i + 1;
  }
  return i;
}' 10
assert $'int main() {
  int x = 3;
  return *&x;
}' 3
assert $'int main() {
  int x = 3;
  int *y = &x;
  int **z = &y;
  return **z;
}' 3
assert $'int main() {
  int x = 3;
  int y = 5;
  return *(&x + 1);
}' 5
assert $'int main() {
  int x = 3;
  int y = 5;
  return *(&y - 1);
}' 3
assert $'int main() {
  int x = 3;
  int y = 5;
  return *(&x - (-1));
}' 5
assert $'int main() {
  int x = 3;
  int *y = &x;
  *y = 5;
  return x;
}' 5
assert $'int main() {
  int x = 3;
  int y = 5;
  *(&x + 1) = 7;
  return y;
}' 7
assert $'int main() {
  int x = 3;
  int y = 5;
  *(&y - 1) = 7;
  return x;
}' 7
assert $'int main() {
  int x = 3;
  return (&x + 4) - (&x);
}' 4
assert $'int main() {
  int x = 3;
  int y = 5;
  return (&y) - (&x);
}' 1
assert $'int main() {
  int x = 3;
  int y = 5;
  return (&x + 2) - &x + 3;
}' 5
assert $'int three() {
  return 3;
}

int main() {
  return three();
}' 3
assert $'int ret5_local() {
  return 5;
}

int main() {
  int x = ret5_local();
  return x;
}' 5
assert $'int make_sum() {
  int a = ret3();
  int b = ret5();
  return a + b;
}

int main() {
  int a = make_sum();
  return ret8() - a;
}' 0
assert $'int choose() {
  return ret10();
}

int main() {
  int x = choose();
  x = ret42();
  return x;
}' 42
assert $'int main() {
  return add2(3, 4);
}' 7
assert $'int main() {
  return add3(ret1(), 2, 3);
}' 6
assert $'int main() {
  int x = add5(1, 2, 3, 4, 5);
  return x;
}' 15
assert $'int main() {
  return add6(1, 2, 3, 4, 5, 6);
}' 21
assert $'int main() {
  return add2(add2(1, 2), add2(3, 4));
}' 10
assert $'int main() {
  return mix6(ret5(), ret3(), ret4(), ret2(), ret1(), ret0());
}' 5
assert $'int add_local(int a, int b) {
  return a + b;
}

int main() {
  return add_local(7, 3);
}' 10
assert $'int sum6_local(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

int main() {
  return sum6_local(1, 2, 3, 4, 5, 6);
}' 21
assert $'int load(int *p) {
  return *p;
}

int main() {
  int x = 9;
  return load(&x);
}' 9
assert $'int set(int *p) {
  *p = 7;
  return *p;
}

int main() {
  int x = 4;
  int y = set(&x);
  return x + y;
}' 14
assert $'int main() {
  int arr[3];
  *arr = 1;
  *(arr + 1) = 2;
  *(arr + 2) = *arr + *(arr + 1);
  return *(arr + 2);
}' 3
assert $'int sum(int xs[3]) {
  return *xs + *(xs + 1) + *(xs + 2);
}

int main() {
  int arr[3];
  *arr = 1;
  *(arr + 1) = 4;
  *(arr + 2) = 7;
  return sum(arr);
}' 12
assert $'int main() {
  int arr[4];
  *(arr + 3) = 7;
  int *p = arr;
  return *(p + 3);
}' 7


echo "OK"
