#!/usr/bin/env python3
"""Run a build with the pinned static media libraries, without shell evaluation."""
import argparse
import json
import os
from pathlib import Path
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--repo', type=Path, required=True)
parser.add_argument('--metadata', type=Path,
                    help='Use an already built environment JSON instead of invoking Nix')
parser.add_argument('command', nargs=argparse.REMAINDER)
args = parser.parse_args()
command = args.command[1:] if args.command[:1] == ['--'] else args.command
if not command:
    parser.error('a build command is required after --')
if os.environ.get('CARGO_ENCODED_RUSTFLAGS'):
    parser.error('CARGO_ENCODED_RUSTFLAGS would override the static media link flags')
metadata = args.metadata
if metadata is None:
    result = subprocess.check_output([
        'nix', 'build', '--impure', '--no-link', '--json', '--file',
        str(Path(__file__).with_name('pinned-environment.nix')),
        '--argstr', 'repo', str(args.repo.resolve()),
    ], text=True)
    metadata = Path(json.loads(result)[0]['outputs']['out'])
config = json.loads(metadata.read_text())
if config.get('schema') != 1:
    parser.error('unsupported media environment schema')
for archive in config['archives']:
    if not Path(archive).is_file():
        parser.error(f'missing static archive: {archive}')
env = dict(os.environ)
env.update(PKG_CONFIG_PATH=config['pkg_config_path'], PKG_CONFIG_LIBDIR='',
           PKG_CONFIG_ALLOW_CROSS='1', PKG_CONFIG_ALL_STATIC='1',
           FREETYPE2_NO_PKG_CONFIG='1')
env['RUSTFLAGS'] = ' '.join(filter(None, [env.get('RUSTFLAGS'), config['rustflags']]))
print(f'Penny media environment: {metadata}', flush=True)
os.execvpe(command[0], command, env)
