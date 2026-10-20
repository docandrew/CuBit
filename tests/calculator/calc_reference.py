#!/usr/bin/env python3
"""Reference answers for the calculator core's differential tests.

Writes one case per line, tab-separated:
  expression  fraction  places  decimal  form
or, for an expected failure:
  expression  !failure  column

The fractions come from Python's exact integers (fractions.Fraction); the
decimal text from an independent implementation of the display rules in
docs/calculator.md ("Display"); approximations (sqrt, pi, e) from the
decimal module at high precision (pi by Gauss-Legendre, not Machin as in
the core).

usage: calc_reference.py OUT COUNT [SEED]
"""
import decimal
import random
import sys
from fractions import Fraction
from math import floor, isqrt

PLACES = 20
PRECISION = 50  # the tests' Limits.Precision


class Failure(Exception):
    def __init__(self, kind):
        self.kind = kind


# --- display (independent of the Ada code) --------------------------------

def round_half_away(x: Fraction) -> int:
    n = abs(x)
    q, r = divmod(n.numerator, n.denominator)
    if 2 * r >= n.denominator:
        q += 1
    return q if x >= 0 else -q


def strip_zeros(digits: str) -> str:
    stripped = digits.rstrip("0")
    return stripped if stripped else "0"


def scientific(mantissa_digits: str, exponent: int) -> str:
    mantissa_digits = mantissa_digits.rstrip("0") or "0"
    head, tail = mantissa_digits[0], mantissa_digits[1:]
    return head + ("." + tail if tail else "") + "e" + str(exponent)


def decimal_text(x: Fraction, places: int):
    sign = "-" if x < 0 else ""
    a = abs(x)
    whole, rest = divmod(a.numerator, a.denominator)
    den = a.denominator
    if rest == 0:
        return sign + str(whole), "Integer_Form"
    odd, twos, fives = den, 0, 0
    while odd % 2 == 0:
        odd //= 2
        twos += 1
    while odd % 5 == 0:
        odd //= 5
        fives += 1
    pre = max(twos, fives)
    if odd == 1 and pre <= places:
        digits = str(rest * 10 ** pre // den).rjust(pre, "0")
        return f"{sign}{whole}.{digits}", "Terminating"
    if odd == 1 and whole == 0:
        n = rest * 10 ** pre // den
        s = len(str(n))
        if s <= places:
            return sign + scientific(str(n), s - 1 - pre), "Scientific_Exact"
    if odd != 1 and pre < places:
        period = None
        power = 1
        for k in range(1, places - pre + 1):
            power = power * 10 % odd
            if power == 1:
                period = k
                break
        if period is not None:
            digits = str(rest * 10 ** (pre + period) // den).rjust(pre + period, "0")
            return f"{sign}{whole}.{digits[:pre]}({digits[pre:]})", "Repeating"
    if whole == 0:
        zeros = 0
        while rest * 10 ** (zeros + 1) < den:
            zeros += 1
        if zeros > places // 2:
            n = round_half_away(Fraction(rest * 10 ** (zeros + places), den))
            s = len(str(n))
            return sign + scientific(str(n), s - 1 - zeros - places), "Scientific_Rounded"
    n = round_half_away(a * 10 ** places)
    if n == 0:
        sign = ""
    text = str(n).rjust(places + 1, "0")
    int_part, frac = text[:-places], text[-places:]
    return f"{sign}{int_part}.{strip_zeros(frac)}", "Rounded"


def fraction_text(x: Fraction) -> str:
    if x.denominator == 1:
        return str(x.numerator)
    return f"{x.numerator}/{x.denominator}"


# --- approximations -------------------------------------------------------

def approx_floor(value: decimal.Decimal, digits: int) -> Fraction:
    scaled = value.scaleb(digits).to_integral_value(rounding=decimal.ROUND_FLOOR)
    return Fraction(int(scaled), 10 ** digits)


def pi_value(digits: int) -> Fraction:
    with decimal.localcontext() as ctx:
        ctx.prec = digits + 30
        a, b = decimal.Decimal(1), decimal.Decimal(1) / decimal.Decimal(2).sqrt()
        t, p = decimal.Decimal(1) / 4, decimal.Decimal(1)
        for _ in range(40):
            an = (a + b) / 2
            b = (a * b).sqrt()
            t -= p * (a - an) ** 2
            a = an
            p *= 2
        return approx_floor((a + b) ** 2 / (4 * t), digits)


def e_value(digits: int) -> Fraction:
    with decimal.localcontext() as ctx:
        ctx.prec = digits + 30
        return approx_floor(decimal.Decimal(1).exp(), digits)


def sqrt_value(x: Fraction, digits: int):
    if x < 0:
        raise Failure("Outside_Domain")
    p, q = x.numerator, x.denominator
    if isqrt(p) ** 2 == p and isqrt(q) ** 2 == q:
        return Fraction(isqrt(p), isqrt(q)), False
    return Fraction(isqrt(p * q * 10 ** (2 * digits)), q * 10 ** digits), True


# --- random expressions ---------------------------------------------------

def literal(rng):
    kind = rng.random()
    if kind < 0.5:
        text = str(rng.randint(0, 10 ** rng.randint(1, 30)))
    elif kind < 0.8:
        text = f"{rng.randint(0, 999)}.{rng.randint(0, 10 ** rng.randint(1, 8))}"
    else:
        text = f"{rng.randint(1, 99)}e{rng.choice(['', '-', '+'])}{rng.randint(0, 12)}"
    return text, parse_literal(text)


def parse_literal(text):
    mantissa, _, exponent = text.partition("e")
    whole, _, frac = mantissa.partition(".")
    value = Fraction(int(whole or "0") * 10 ** len(frac) + int(frac or "0"), 10 ** len(frac))
    if exponent:
        value *= Fraction(10) ** int(exponent)
    return value


def expression(rng, depth):
    """(text, Fraction or Failure, approximate?) for a random expression."""
    if depth <= 0 or rng.random() < 0.3:
        return literal(rng) + (False,)
    choice = rng.random()
    if choice < 0.55:
        op = rng.choice("+-*/")
        lt, lv, la = expression(rng, depth - 1)
        rt, rv, ra = expression(rng, depth - 1)
        text = f"({lt}) {op} ({rt})"
        if op == "+":
            return text, lv + rv, la or ra
        if op == "-":
            return text, lv - rv, la or ra
        if op == "*":
            return text, lv * rv, la or ra
        if rv == 0:
            raise Failure("Division_By_Zero")
        return text, lv / rv, la or ra
    if choice < 0.65:
        bt, bv, ba = expression(rng, depth - 1)
        e = rng.randint(-6, 6)
        if bv == 0 and e < 0:
            raise Failure("Division_By_Zero")
        return f"({bt})^{e}" if e >= 0 else f"({bt})^({e})", bv ** e, ba
    if choice < 0.75:
        t, v, a = expression(rng, depth - 1)
        return f"-({t})", -v, a
    if choice < 0.85:
        fn = rng.choice(["abs", "floor", "ceil", "round", "trunc", "num", "den"])
        t, v, a = expression(rng, depth - 1)
        result = {
            "abs": abs(v), "floor": Fraction(floor(v)), "ceil": -Fraction(floor(-v)),
            "round": Fraction(round_half_away(v)), "trunc": Fraction(int(v)),
            "num": Fraction(v.numerator), "den": Fraction(v.denominator),
        }[fn]
        return f"{fn}({t})", result, a
    if choice < 0.92:
        lt, lv, la = expression(rng, depth - 1)
        rt, rv, ra = expression(rng, depth - 1)
        if rv == 0:
            raise Failure("Division_By_Zero")
        return f"({lt}) mod ({rt})", lv - rv * floor(lv / rv), la or ra
    n = rng.randint(0, 30)
    return f"{n}!", Fraction(__import__("math").factorial(n)), False


def fixed_cases():
    """Hand-picked cases: exactness, display forms, errors with columns."""
    cases = []

    def ok(text, value, places=PLACES):
        cases.append((text, value, places))

    def fail(text, kind, column):
        cases.append((text, Failure(kind), column))

    ok("2/3 + 1/6", Fraction(5, 6))
    ok("1/3", Fraction(1, 3))
    ok("1/7", Fraction(1, 7))
    ok("1/97", Fraction(1, 97))
    ok("1/8", Fraction(1, 8))
    ok("0.1 + 0.2", Fraction(3, 10))
    ok("1e-30", Fraction(1, 10 ** 30))
    ok("1.5e-30", Fraction(15, 10 ** 31))
    ok("1/3e30", Fraction(1, 3 * 10 ** 30))
    ok("2^100", Fraction(2 ** 100))
    ok("2^-3", Fraction(1, 8))
    ok("-2^2", Fraction(-4))
    ok("(-2)^3", Fraction(-8))
    ok("2^3^2", Fraction(2 ** 9))
    ok("100!", Fraction(__import__("math").factorial(100)))
    ok("2^1000 / 3", Fraction(2 ** 1000, 3))
    ok("7 mod 3", Fraction(1))
    ok("-7 mod 3", Fraction(2))
    ok("7/2 mod 1", Fraction(1, 2))
    ok("gcd(12, 18)", Fraction(6))
    ok("lcm(4, 6)", Fraction(12))
    ok("round(2.5)", Fraction(3))
    ok("round(-2.5)", Fraction(-3))
    ok("round(2/3, 4)", Fraction(6667, 10000))
    ok("floor(-1/2)", Fraction(-1))
    ok("ceil(-1/2)", Fraction(0))
    ok("trunc(-7/2)", Fraction(-3))
    ok("sqrt(16/9)", Fraction(4, 3))
    ok("num(-6/4)", Fraction(-3))
    ok("den(6/4)", Fraction(2))
    ok("0^0", Fraction(1))
    ok("1^-99999999999999999999", Fraction(1))
    ok("(-1)^99999999999999999999", Fraction(-1))
    ok("0.999999999999999999999", Fraction(999999999999999999999, 10 ** 21))
    ok("-1/3", Fraction(-1, 3))
    ok("22/7", Fraction(22, 7))
    ok("1/3", Fraction(1, 3), 1)
    ok("1/6", Fraction(1, 6), 1)
    ok("123456789/1000", Fraction(123456789, 1000), 2)
    ok("  3 *( 4 + 5 )  ", Fraction(27))
    fail("1/0", "Division_By_Zero", 2)
    fail("(1+2)/(3-3)", "Division_By_Zero", 6)
    fail("0^-1", "Division_By_Zero", 2)
    fail("5 mod 0", "Division_By_Zero", 3)
    fail("", "Empty_Expression", 1)
    fail("   ", "Empty_Expression", 1)
    fail("2 3", "Expected_Operator", 3)
    fail("2(3)", "Expected_Operator", 2)
    fail("2 +", "Expected_Operand", 3)
    fail("(2", "Unclosed_Parenthesis", 1)
    fail("2)", "Unopened_Parenthesis", 2)
    fail("1, 2", "Misplaced_Comma", 2)
    fail("foo(2)", "Unknown_Name", 1)
    fail("sqrt(1, 2)", "Wrong_Argument_Count", 1)
    fail("gcd(4)", "Wrong_Argument_Count", 1)
    fail("1.2.3", "Malformed_Number", 4)
    fail(".", "Malformed_Number", 1)
    fail("2 # 3", "Unexpected_Character", 3)
    fail("2^(1/2)", "Fractional_Exponent", 2)
    fail("sqrt(-4)", "Outside_Domain", 1)
    fail("(-3)!", "Outside_Domain", 5)
    fail("(1/2)!", "Not_An_Integer", 6)
    fail("gcd(1/2, 3)", "Not_An_Integer", 1)
    fail("2^99999999999", "Too_Large", 2)
    fail("10^(10^9)", "Too_Large", 3)
    fail("1e999999999999", "Too_Large", 1)
    fail("ans", "No_Previous_Result", 1)
    return cases


def write_case(out, text, value, places):
    if isinstance(value, Failure):
        out.write(f"{text}\t!{value.kind}\t{places}\n")
        return
    decimal_form, form = decimal_text(value, places)
    out.write(f"{text}\t{fraction_text(value)}\t{places}\t{decimal_form}\t{form}\n")


def main():
    out_path, count = sys.argv[1], int(sys.argv[2])
    rng = random.Random(int(sys.argv[3]) if len(sys.argv) > 3 else 2026)
    with open(out_path, "w") as out:
        for text, value, places in fixed_cases():
            write_case(out, text, value, places)
        # Approximations at PRECISION digits (marked approximate).
        for digits_case in ["sqrt(2)", "sqrt(1/3)", "pi", "e", "2*pi", "sqrt(2)^2"]:
            if digits_case == "sqrt(2)":
                value, _ = sqrt_value(Fraction(2), PRECISION)
            elif digits_case == "sqrt(1/3)":
                value, _ = sqrt_value(Fraction(1, 3), PRECISION)
            elif digits_case == "pi":
                value = pi_value(PRECISION)
            elif digits_case == "e":
                value = e_value(PRECISION)
            elif digits_case == "2*pi":
                value = 2 * pi_value(PRECISION)
            else:
                value = sqrt_value(Fraction(2), PRECISION)[0] ** 2
            out.write(f"~{digits_case}\t{fraction_text(value)}\t{PLACES}\t"
                      f"{decimal_text(value, PLACES)[0]}\t{decimal_text(value, PLACES)[1]}\n")
        written = 0
        while written < count:
            try:
                text, value, _ = expression(rng, rng.randint(1, 5))
            except Failure as failure:
                continue  # random failures are not located; fixed cases cover them
            except (OverflowError, ZeroDivisionError):
                continue
            if max(abs(value.numerator), value.denominator).bit_length() > 20000:
                continue
            write_case(out, text, value, rng.choice([PLACES, 1, 5, 50]))
            written += 1


if __name__ == "__main__":
    main()
