"""Compare an Ada table dump against the pinned Linux v6.16 MOCS source.

Usage: python3 check-mocs-reference.py /path/to/intel_mocs.c /path/to/adln_mocs_tests
No downloads or C execution: evaluate only the tiny integer-expression grammar
used by the reference macros. Unexpected syntax fails closed.
"""
import ast
import hashlib
import re
import subprocess
import sys
from pathlib import Path

source_bytes = Path(sys.argv[1]).read_bytes()
assert hashlib.sha256(source_bytes).hexdigest() == (
    "8f789f79594c08dcc63f61dd20cd6e18d75d3c0baa5ae87d59b37d95a41427cc"
)
source = source_bytes.decode()
definitions = {}
for match in re.finditer(r"^#define\s+(\w+)(\(value\))?\s+([^\n]+)", source, re.M):
    name, parameter, expression = match.groups()
    if name.startswith(("LE_", "L3_", "_LE_", "_L3_")):
        definitions[name] = (bool(parameter), expression)


def evaluate(expression, argument=None):
    def visit(node):
        if isinstance(node, ast.Constant) and type(node.value) is int:
            return node.value
        if isinstance(node, ast.Name):
            if node.id == "value" and argument is not None:
                return argument
            parameter, body = definitions[node.id]
            assert not parameter
            return evaluate(body)
        if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
            parameter, body = definitions[node.func.id]
            assert parameter and len(node.args) == 1 and not node.keywords
            return evaluate(body, visit(node.args[0]))
        if isinstance(node, ast.BinOp):
            left, right = visit(node.left), visit(node.right)
            if isinstance(node.op, ast.BitOr):
                return left | right
            if isinstance(node.op, ast.LShift):
                return left << right
        raise ValueError(ast.dump(node))
    return visit(ast.parse(expression.strip(), mode="eval").body)


common = source.split("#define GEN11_MOCS_ENTRIES", 1)[1].split(
    "static const struct drm_i915_mocs_entry tgl_mocs_table", 1
)[0]
specific = source.split("static const struct drm_i915_mocs_entry gen12_mocs_table[] = {", 1)[1].split("};", 1)[0]
entries = {}
for block in (common, specific):
    block = re.sub(r"/\*.*?\*/", "", block, flags=re.S).replace("\\", "")
    for start in re.finditer(r"MOCS_ENTRY\(", block):
        depth, parts, first = 0, [], start.end()
        for index in range(first, len(block)):
            char = block[index]
            if char == "(":
                depth += 1
            elif char == ")":
                if depth == 0:
                    parts.append(block[first:index])
                    break
                depth -= 1
            elif char == "," and depth == 0:
                parts.append(block[first:index])
                first = index + 1
        assert len(parts) == 3
        key = int(parts[0])
        assert key not in entries
        entries[key] = tuple(evaluate(item) for item in parts[1:])

expected = [entries.get(index, entries[2]) for index in range(64)]
dump = subprocess.check_output([sys.argv[2], "dump"], text=True)
actual = [tuple(map(int, line.split())) for line in dump.splitlines()]
assert len(actual) == 64
for index, (wanted, got) in enumerate(zip(expected, actual)):
    assert wanted == got, (index, wanted, got)
print("PASS: all 64 ADL-N control/L3 entries match pinned Linux v6.16")
