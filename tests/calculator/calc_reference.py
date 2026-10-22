#!/usr/bin/env python3
"""Reference answers for the calculator core's differential tests.

Writes one case per line, tab-separated, the first field its kind and the
second the evaluation mode ("std:radians", "std:degrees", "std:gradians",
"prog:<word>:<signed|unsigned>" with word unbounded, 8, 16, 32 or 64):

  E  mode  expression  fraction  places  decimal  form     an exact result
  F  mode  expression  failure  column                     a typed failure
  A  mode  expression  reference  places                   an approximation
  P  mode  expression  decimal  hex  octal  binary  wrapped   programmer mode

Exact results come from Python's integers (fractions.Fraction) and an
independent implementation of the display rules in docs/calculator.md
("Display"). Approximations are checked against mpmath at 150 digits: the
core's ball must contain the reference and its shown digits must be within
the bound it states. Programmer results come from Python's integers with
the word's wrap after every step.

usage: calc_reference.py OUT COUNT [SEED]
"""
import random
import sys
from fractions import Fraction
from math import floor, factorial

import mpmath

PLACES = 20
mpmath.mp.dps = 150


class Failure(Exception):
    def __init__(self, kind):
        self.kind = kind


# --- exact display (independent of the Ada code) --------------------------

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


# --- standard mode: exact expressions --------------------------------------

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


def exact_expression(rng, depth):
    if depth <= 0 or rng.random() < 0.3:
        return literal(rng)
    choice = rng.random()
    if choice < 0.55:
        op = rng.choice("+-*/")
        lt, lv = exact_expression(rng, depth - 1)
        rt, rv = exact_expression(rng, depth - 1)
        text = f"({lt}) {op} ({rt})"
        if op == "+":
            return text, lv + rv
        if op == "-":
            return text, lv - rv
        if op == "*":
            return text, lv * rv
        if rv == 0:
            raise Failure("Division_By_Zero")
        return text, lv / rv
    if choice < 0.65:
        bt, bv = exact_expression(rng, depth - 1)
        e = rng.randint(-6, 6)
        if bv == 0 and e < 0:
            raise Failure("Division_By_Zero")
        return f"({bt})^{e}" if e >= 0 else f"({bt})^({e})", bv ** e
    if choice < 0.75:
        t, v = exact_expression(rng, depth - 1)
        return f"-({t})", -v
    if choice < 0.85:
        fn = rng.choice(["abs", "floor", "ceil", "round", "trunc", "num", "den"])
        t, v = exact_expression(rng, depth - 1)
        result = {
            "abs": abs(v), "floor": Fraction(floor(v)), "ceil": -Fraction(floor(-v)),
            "round": Fraction(round_half_away(v)), "trunc": Fraction(int(v)),
            "num": Fraction(v.numerator), "den": Fraction(v.denominator),
        }[fn]
        return f"{fn}({t})", result
    if choice < 0.92:
        lt, lv = exact_expression(rng, depth - 1)
        rt, rv = exact_expression(rng, depth - 1)
        if rv == 0:
            raise Failure("Division_By_Zero")
        return f"({lt}) mod ({rt})", lv - rv * floor(lv / rv)
    n = rng.randint(0, 30)
    return f"{n}!", Fraction(factorial(n))


# --- approximations: mpmath at 150 digits ----------------------------------

UNITS = {"radians": mpmath.mpf(1), "degrees": mpmath.pi / 180, "gradians": mpmath.pi / 200}


def real(q: Fraction):
    return mpmath.mpf(q.numerator) / q.denominator


def small_rational(rng, low, high):
    den = rng.choice([1, 2, 3, 4, 7, 10, 100, 1000, 12345])
    num = rng.randint(int(low * den), int(high * den))
    return Fraction(num, den)


def text_of(q: Fraction) -> str:
    return str(q.numerator) if q.denominator == 1 and q >= 0 else f"({q.numerator}/{q.denominator})"


def approximate_case(rng):
    """(mode, text, reference) for a random elementary-function expression."""
    unit = rng.choice(["radians", "degrees", "gradians"])
    fn = rng.choice(["sin", "cos", "tan", "asin", "acos", "atan", "ln", "log", "exp", "sqrt", "pow", "root",
                     "pythagoras", "pi", "e", "mixed"])
    mode = f"std:{unit}"
    if fn in ("sin", "cos", "tan"):
        x = small_rational(rng, -400, 400)
        f = {"sin": mpmath.sin, "cos": mpmath.cos, "tan": mpmath.tan}[fn]
        return mode, f"{fn}({text_of(x)})", f(real(x) * UNITS[unit])
    if fn in ("asin", "acos"):
        x = small_rational(rng, -0.999, 0.999)
        f = {"asin": mpmath.asin, "acos": mpmath.acos}[fn]
        return mode, f"{fn}({text_of(x)})", f(real(x)) / UNITS[unit]
    if fn == "atan":
        x = small_rational(rng, -1000, 1000)
        return mode, f"atan({text_of(x)})", mpmath.atan(real(x)) / UNITS[unit]
    if fn in ("ln", "log"):
        x = small_rational(rng, 0.001, 10 ** 6)
        if x <= 0:
            x = Fraction(1, 7)
        f = {"ln": mpmath.ln, "log": mpmath.log10}[fn]
        return mode, f"{fn}({text_of(x)})", f(real(x))
    if fn == "exp":
        x = small_rational(rng, -60, 60)
        return mode, f"exp({text_of(x)})", mpmath.exp(real(x))
    if fn == "sqrt":
        x = small_rational(rng, 0.001, 10 ** 9)
        if x <= 0:
            x = Fraction(2)
        return mode, f"sqrt({text_of(x)})", mpmath.sqrt(real(x))
    if fn == "pow":
        x = small_rational(rng, 0.01, 100)
        if x <= 0:
            x = Fraction(3, 2)
        y = small_rational(rng, -5, 5)
        return mode, f"{text_of(x)}^{text_of(y)}", mpmath.power(real(x), real(y))
    if fn == "root":
        x = small_rational(rng, -1000, 1000)
        n = rng.choice([3, 5, 7])
        r = mpmath.root(abs(real(x)), n)
        return mode, f"root({text_of(x)}, {n})", (-r if x < 0 else r)
    if fn == "pythagoras":
        x = small_rational(rng, -10, 10)
        return mode, f"sin({text_of(x)})^2 + cos({text_of(x)})^2", mpmath.mpf(1)
    if fn == "pi":
        return mode, "pi", +mpmath.pi
    if fn == "e":
        return mode, "e", +mpmath.e
    x = small_rational(rng, 0.1, 50)
    if x <= 0:
        x = Fraction(1, 2)
    return mode, f"exp(ln({text_of(x)})) + sqrt(2)*pi", mpmath.exp(mpmath.ln(real(x))) + mpmath.sqrt(2) * mpmath.pi


# --- programmer mode: words -------------------------------------------------

def wrap(value, word, signed):
    if word == 0:
        return value
    value %= 1 << word
    if signed and value >= 1 << (word - 1):
        value -= 1 << word
    return value


def pattern(value, word):
    return value % (1 << word) if word else value


def based(value, base, word):
    digits = "0123456789ABCDEF"
    v = pattern(value, word) if (word and base != 10) else value
    sign = "-" if v < 0 else ""
    v = abs(v)
    out = ""
    while True:
        out = digits[v % base] + out
        v //= base
        if v == 0:
            break
    if word and base != 10:
        per = {2: 1, 8: 3, 16: 4}[base]
        out = out.rjust((word + per - 1) // per, "0")
    return sign + out


def program_expression(rng, depth, word, signed, state):
    """(text, value) with every step wrapped; state['wrapped'] notes a change."""
    def note(v):
        w = wrap(v, word, signed)
        if w != v:
            state["wrapped"] = True
        return w

    if depth <= 0 or rng.random() < 0.3:
        v = rng.randint(0, (1 << (word or 40)) - 1)
        style = rng.choice(["dec", "hex", "bin", "oct"])
        text = {"dec": str(v), "hex": f"0x{v:X}", "bin": f"0b{v:b}", "oct": f"0o{v:o}"}[style]
        return text, note(v)
    op = rng.choice(["+", "-", "*", "/", "mod", "&", "|", "xor", "~", "<<", ">>", "rotl", "rotr", "neg"])
    if op in ("rotl", "rotr") and not word:
        op = "&"
    if op == "~":
        t, v = program_expression(rng, depth - 1, word, signed, state)
        return f"~({t})", note(-v - 1)
    if op == "neg":
        t, v = program_expression(rng, depth - 1, word, signed, state)
        return f"-({t})", note(-v)
    lt, lv = program_expression(rng, depth - 1, word, signed, state)
    if op in ("<<", ">>", "rotl", "rotr"):
        n = rng.randint(0, (word or 40) + 3)
        rt, rv = str(n), n
    else:
        rt, rv = program_expression(rng, depth - 1, word, signed, state)
    if op in ("/", "mod") and rv == 0:
        raise Failure("Division_By_Zero")
    if op == "+":
        v = lv + rv
    elif op == "-":
        v = lv - rv
    elif op == "*":
        v = lv * rv
    elif op == "/":
        q = abs(lv) // abs(rv)
        v = q if (lv >= 0) == (rv >= 0) else -q
    elif op == "mod":
        v = lv % rv
    elif op == "&":
        v = lv & rv
    elif op == "|":
        v = lv | rv
    elif op == "xor":
        v = lv ^ rv
    elif op == "<<":
        v = lv << rv
    elif op == ">>":
        v = lv >> rv
    else:
        u = pattern(lv, word)
        n = rv % word
        if op == "rotr":
            n = (word - n) % word
        v = ((u << n) | (u >> (word - n))) % (1 << word)
        return f"{op}({lt}, {rt})", wrap(v, word, signed)
    return f"({lt}) {op} ({rt})", note(v)


def mode_of(word, signed):
    return f"prog:{word or 'unbounded'}:{'signed' if signed else 'unsigned'}"


def program_line(text, value, word, signed, wrapped):
    return (f"P\t{mode_of(word, signed)}\t{text}\t{value}\t{based(value, 16, word)}\t{based(value, 8, word)}\t"
            f"{based(value, 2, word)}\t{'wrapped' if wrapped else 'exact'}\n")


# --- fixed cases ------------------------------------------------------------

def fixed_cases(out):
    def ok(text, value, places=PLACES, mode="std:radians"):
        decimal_form, form = decimal_text(value, places)
        out.write(f"E\t{mode}\t{text}\t{fraction_text(value)}\t{places}\t{decimal_form}\t{form}\n")

    def fail(text, kind, column, mode="std:radians"):
        out.write(f"F\t{mode}\t{text}\t{kind}\t{column}\n")

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
    ok("100!", Fraction(factorial(100)))
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
    ok("-1/3", Fraction(-1, 3))
    ok("22/7", Fraction(22, 7))
    ok("1/3", Fraction(1, 3), 1)
    ok("1/6", Fraction(1, 6), 1)
    ok("123456789/1000", Fraction(123456789, 1000), 2)
    ok("  3 *( 4 + 5 )  ", Fraction(27))
    # Exact where exactness allows.
    ok("8^(1/3)", Fraction(2))
    ok("(4/9)^(3/2)", Fraction(8, 27))
    ok("(-8)^(1/3)", Fraction(-2))
    ok("root(-32, 5)", Fraction(-2))
    ok("sin(0)", Fraction(0))
    ok("cos(0)", Fraction(1))
    ok("exp(0)", Fraction(1))
    ok("ln(1)", Fraction(0))
    ok("log(1000)", Fraction(3))
    ok("log(1/100)", Fraction(-2))
    ok("atan(0)", Fraction(0))
    ok("sin(30)", Fraction(1, 2), mode="std:degrees")
    ok("sin(-210)", Fraction(1, 2), mode="std:degrees")
    ok("cos(60)", Fraction(1, 2), mode="std:degrees")
    ok("cos(180)", Fraction(-1), mode="std:degrees")
    ok("tan(45)", Fraction(1), mode="std:degrees")
    ok("tan(135)", Fraction(-1), mode="std:degrees")
    ok("asin(1/2)", Fraction(30), mode="std:degrees")
    ok("acos(-1/2)", Fraction(120), mode="std:degrees")
    ok("atan(1)", Fraction(45), mode="std:degrees")
    ok("atan(-1)", Fraction(-50), mode="std:gradians")
    ok("sin(100)", Fraction(1), mode="std:gradians")
    ok("asin(1)", Fraction(100), mode="std:gradians")
    ok("round(pi, 4)", Fraction(31416, 10000))
    ok("floor(1000 * e)", Fraction(2718))
    # Bitwise on integers (infinite two's complement in standard mode).
    ok("0xFF & 0x0F", Fraction(15))
    ok("0b1010 | 0b0101", Fraction(15))
    ok("0o17 xor 5", Fraction(15 ^ 5))
    ok("~5", Fraction(-6))
    ok("-6 & 7", Fraction(-6 & 7))
    ok("-6 | -3", Fraction(-6 | -3))
    ok("-6 xor 9", Fraction(-6 ^ 9))
    ok("1 << 100", Fraction(1 << 100))
    ok("-1 >> 3", Fraction(-1))
    ok("-17 >> 2", Fraction(-17 >> 2))
    ok("1 + 2 << 3", Fraction((1 + 2) << 3))
    ok("6 & 3 | 8", Fraction((6 & 3) | 8))
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
    fail("0b102", "Malformed_Number", 5)
    fail("2 # 3", "Unexpected_Character", 3)
    fail("2 < 3", "Unexpected_Character", 3)
    fail("sqrt(-4)", "Outside_Domain", 1)
    fail("(-4)^(1/2)", "Outside_Domain", 5)
    fail("(-3)!", "Outside_Domain", 5)
    fail("(1/2)!", "Not_An_Integer", 6)
    fail("gcd(1/2, 3)", "Not_An_Integer", 1)
    fail("1.5 & 1", "Not_An_Integer", 5)
    fail("1 << -1", "Outside_Domain", 3)
    fail("pi & 1", "Not_An_Integer", 4)
    fail("ln(0)", "Outside_Domain", 1)
    fail("ln(-1)", "Outside_Domain", 1)
    fail("asin(2)", "Outside_Domain", 1)
    fail("tan(90)", "Outside_Domain", 1, mode="std:degrees")
    fail("tan(-270)", "Outside_Domain", 1, mode="std:degrees")
    fail("floor(sqrt(2)^2)", "Indeterminate", 1)
    fail("rotl(1, 1)", "Needs_Word_Size", 1)
    fail("2^99999999999", "Too_Large", 2)
    fail("10^(10^9)", "Too_Large", 3)
    fail("1e999999999999", "Too_Large", 1)
    fail("exp(10^12)", "Too_Large", 1)
    fail("ans", "No_Previous_Result", 1)
    # Programmer mode.
    fail("1/2 + 0.5", "Not_An_Integer", 7, mode="prog:32:signed")
    fail("sqrt(2)", "Not_An_Integer", 1, mode="prog:32:signed")
    fail("pi", "Not_An_Integer", 1, mode="prog:64:signed")
    fail("7 / 0", "Division_By_Zero", 3, mode="prog:8:unsigned")

    for text, value, word, signed, wrapped in [
            ("0xFF", -1, 8, True, True),
            ("0xFF", 255, 8, False, False),
            ("127 + 1", -128, 8, True, True),
            ("255 + 1", 0, 8, False, True),
            ("-1", 255, 8, False, True),
            ("7 / 2", 3, 32, True, False),
            ("-7 / 2", -3, 32, True, False),
            ("~0", -1, 64, True, False),
            ("~0", 2 ** 64 - 1, 64, False, True),
            ("rotl(0x81, 1)", 0x03, 8, False, False),
            ("rotr(1, 1)", -2 ** 15, 16, True, False),
            ("1 << 31", -2 ** 31, 32, True, True),
            ("0x7FFFFFFF * 2", -2, 32, True, True),
            ("2^64", 2 ** 64, 0, True, False),
            ("-6 & 7", 2, 0, True, False)]:
        out.write(program_line(text, value, word, signed, wrapped))


def main():
    out_path, count = sys.argv[1], int(sys.argv[2])
    rng = random.Random(int(sys.argv[3]) if len(sys.argv) > 3 else 2026)
    with open(out_path, "w") as out:
        fixed_cases(out)
        written = 0
        while written < count:
            try:
                text, value = exact_expression(rng, rng.randint(1, 5))
            except Failure:
                continue
            if max(abs(value.numerator), value.denominator).bit_length() > 20000:
                continue
            places = rng.choice([PLACES, 1, 5, 50])
            decimal_form, form = decimal_text(value, places)
            out.write(f"E\tstd:radians\t{text}\t{fraction_text(value)}\t{places}\t{decimal_form}\t{form}\n")
            written += 1
        written = 0
        while written < count // 4:
            mode, text, reference = approximate_case(rng)
            out.write(f"A\t{mode}\t{text}\t{mpmath.nstr(reference, 140, min_fixed=-10**9, max_fixed=10**9)}\t"
                      f"{rng.choice([PLACES, 10, 40])}\n")
            written += 1
        written = 0
        while written < count // 2:
            word = rng.choice([0, 8, 16, 32, 64])
            signed = rng.random() < 0.5
            state = {"wrapped": False}
            try:
                text, value = program_expression(rng, rng.randint(1, 4), word, signed, state)
            except Failure:
                continue
            if word == 0 and abs(value).bit_length() > 4000:
                continue
            out.write(program_line(text, value, word, signed, state["wrapped"]))
            written += 1


if __name__ == "__main__":
    main()
