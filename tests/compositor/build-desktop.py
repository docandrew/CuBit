"""Compatibility entry point for compositor fault/native test builds."""
from pathlib import Path
import runpy
import sys
helper=Path(__file__).resolve().parents[2]/'tools/build_mesa_desktop.py'
sys.path.insert(0,str(helper.parent))
runpy.run_path(str(helper),run_name='__main__')
