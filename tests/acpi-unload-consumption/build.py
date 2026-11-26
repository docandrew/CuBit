#!/usr/bin/env python3
"""Build a pinned Linux-hosted Unload regression profile in private outputs."""
from pathlib import Path, PurePosixPath
import argparse
import hashlib
import json
import shutil
import subprocess

HERE = Path(__file__).resolve().parent


def sha(body):
    return hashlib.sha256(body).hexdigest()


def build_profile(root, output, mode):
    catalog_bytes = (HERE/'sources.json').read_bytes()
    catalog = json.loads(catalog_bytes)
    inputs = {}
    for name, expected in catalog['inputs'].items():
        path = PurePosixPath(name)
        if path.is_absolute() or '..' in path.parts:
            raise ValueError('Nonrelative input: '+name)
        body = (root/name).read_bytes()
        if sha(body) != expected:
            raise ValueError('Source drift: '+name)
        inputs[name] = body
    tools = {}
    for name in ['gprbuild', 'gcc', 'timeout']:
        tool = Path(shutil.which(name) or '/missing').resolve()
        if not str(tool).startswith('/nix/store/'):
            raise ValueError('Use the pinned CuBit Nix development shell')
        tools[str(tool)] = sha(tool.read_bytes())
    flags = catalog['profiles'][mode]
    key = sha(json.dumps(dict(catalog=sha(catalog_bytes), tools=tools, profile=mode, flags=flags), sort_keys=True).encode())
    folder = output/key
    receipt = folder/'BUILD.json'
    binary = folder/'bin/unload_method_probe'
    if folder.exists():
        if receipt.exists():
            old = json.loads(receipt.read_text())
            if old['exit'] == 0 and binary.exists() and sha(binary.read_bytes()) == old['binary_sha256']:
                return old
        raise ValueError('Prior incomplete/failed build retained: '+str(folder))
    source = folder/'source'
    source.mkdir(parents=True)
    names = set()
    for name in catalog['ada_sources']:
        basename = Path(name).name
        if basename in names:
            raise ValueError('Duplicate Ada basename: '+basename)
        names.add(basename)
        (source/basename).write_bytes(inputs[name])
    project = folder/'unload.gpr'
    project.write_text('project Unload is\nfor Source_Dirs use ('+json.dumps(str(source))+');\n'
                       'for Object_Dir use "obj";for Exec_Dir use "bin";for Main use ("unload_method_probe.adb");\n'
                       'package Compiler is for Default_Switches("Ada") use ('+
                       ','.join(json.dumps(flag)for flag in flags)+');end Compiler;end Unload;\n')
    argv = ['timeout', '--kill-after=3s', '270s', 'gprbuild', '-q', '-p', '-j2', '-P', str(project)]
    log = folder/'build.log'
    with log.open('w') as stream:
        process = subprocess.run(argv, stdout=stream, stderr=subprocess.STDOUT, timeout=275)
    result = dict(profile=mode, argv=argv, exit=process.returncode, flags=flags, tools=tools,
                  catalog_sha256=sha(catalog_bytes), input_hashes=catalog['inputs'],
                  binary=str(binary), log=str(log), log_sha256=sha(log.read_bytes()), native=False)
    if process.returncode == 0:
        result['binary_sha256'] = sha(binary.read_bytes())
    receipt.write_text(json.dumps(result, indent=2)+'\n')
    if process.returncode:
        raise RuntimeError('Build failed; exact receipt retained: '+str(receipt))
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--root', type=Path, default=HERE.parents[1])
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--profile', choices=['checked', 'release'], required=True)
    args = parser.parse_args()
    print(build_profile(args.root.resolve(), args.output.resolve(), args.profile)['binary'])
