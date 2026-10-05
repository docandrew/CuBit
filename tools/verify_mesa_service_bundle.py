#!/usr/bin/env python3
"""Verify a local native Mesa bundle before linking (not an authenticity check).

Run under build.lock through final linking to prevent later input mutation.
Bundle manifests are trusted local build metadata, not untrusted packages.
"""
import argparse
import hashlib
import json
from pathlib import Path


def verify(directory):
    directory = Path(directory).resolve()
    data = json.loads((directory / 'inputs.json').read_text())
    if data.get('status') != 'LINK_PASS' or data.get('executed') is not False:
        raise ValueError('Not a completed native link bundle')
    inventory = dict(data['inputs_sha256'])
    inventory.update(data['objects_sha256'])
    inventory[str(directory / 'link-check.app')] = data['link_check_sha256']
    inventory[str(directory / 'link-args.json')] = data['link_args_sha256']
    for name, expected in inventory.items():
        with Path(name).open('rb') as stream:
            actual = hashlib.file_digest(stream, 'sha256').hexdigest()
        if actual != expected:
            raise ValueError('Changed bundle input/output: ' + name)
    return data['link_prefix'], json.loads((directory / 'link-args.json').read_text())


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bundle', type=Path)
    args = parser.parse_args()
    prefix, flags = verify(args.bundle)
    print(json.dumps({'link_prefix': prefix, 'link_args': flags}, indent=2))
