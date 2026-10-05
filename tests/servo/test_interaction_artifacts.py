import ast
import contextlib
import io
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
source = ast.parse(Path(__file__).with_name('run-interaction.py').read_text())
cleanup = [s for s in source.body if isinstance(s, ast.Try)][-1].finalbody
for primary in (False, True):
    for artifact_failure in (False, True):
        called = []
        def report(*args):
            called.append('report')
            if artifact_failure:
                raise ValueError('artifact failure')
        with tempfile.TemporaryDirectory() as directory:
            body = ast.parse("raise LookupError('original failure')" if primary else 'pass').body
            unit = ast.fix_missing_locations(ast.Module(body=[ast.Try(body=body, handlers=[], orelse=[], finalbody=cleanup)], type_ignores=[]))
            scope = dict(stop=lambda: called.append('stop'), server=SimpleNamespace(shutdown=lambda: called.append('shutdown')), d=Path(directory), serial=None, memory_report=report, sys=sys)
            error = None
            try:
                with contextlib.redirect_stdout(io.StringIO()):
                    exec(compile(unit, '<actual runner cleanup>', 'exec'), scope)
            except Exception as caught:
                error = caught
            assert called == ['stop', 'shutdown', 'report'], called
            expected = LookupError if primary else RuntimeError if artifact_failure else None
            assert (type(error) if error else None) is expected, (primary, artifact_failure, error)
print('PASS actual runner cleanup: success, original failure, artifact failure, simultaneous failures')
