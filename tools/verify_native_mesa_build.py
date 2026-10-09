"""Verify local clean-build provenance and engine configuration, not execution."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
from verify_mesa_service_bundle import verify
from native_mesa_targets import archives as select_archives


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def load_record(directory, continued=False):
    original_path = directory / 'build.json'
    original = json.loads(original_path.read_text())
    record = original
    if continued:
        record = json.loads((directory / 'continuation.json').read_text())
        if original.get('status') != 'FAILED':
            raise ValueError('continuation requires a retained failed build record')
        if record.get('prior_failed_build_sha256') != digest(original_path):
            raise ValueError('changed prior failed build record')
        if record.get('inputs_sha256') != original.get('inputs_sha256'):
            raise ValueError('continuation changed original input identities')
    if record.get('status') != 'LINK_PASS':
        raise ValueError('native build is not complete')
    return record


def check(directory, bundle, combined, continued=False):
    directory, bundle = directory.resolve(), bundle.resolve()
    record = load_record(directory, continued)
    for name, expected in record['inputs_sha256'].items():
        if digest(Path(name)) != expected:
            raise ValueError('changed clean-build input: ' + name)
    native, source = directory / 'native', directory / 'source'
    info = json.loads((native / 'meson-info/meson-info.json').read_text())
    if Path(info['directories']['source']).resolve() != source or Path(info['directories']['build']).resolve() != native:
        raise ValueError('mismatched source/build directories')
    options = {item['name']: item['value'] for item in json.loads(
        (native / 'meson-info/intro-buildoptions.json').read_text())}
    expected = {'vulkan-drivers': ['intel'], 'gallium-drivers': ['softpipe'] if combined else [],
                'llvm': 'disabled', 'default_library': 'static'}
    if combined:
        expected['draw-use-llvm'] = False
    for key, value in expected.items():
        if options.get(key) != value:
            raise ValueError('unexpected native build option: ' + key)
    commands = json.loads((native / 'compile_commands.json').read_text())
    if combined and sum(item['file'].endswith('/st_manager.c') for item in commands) != 1:
        raise ValueError('missing unique software frontend compile recipe')
    verify(bundle)
    inventory = json.loads((bundle / 'inputs.json').read_text())['inputs_sha256']
    targets = json.loads((native / 'meson-info/intro-targets.json').read_text())
    archives = {Path(name).resolve() for target in targets if target['type'] == 'static library'
                for name in target['filename']}
    if combined:
        archives = set(select_archives(targets, native))
        selected = sorted(str(p.relative_to(native)) for p in archives)
        if sorted(record.get('selected_archives', [])) != selected:
            raise ValueError('selected archive record does not match native engine policy')
    members = set()
    for archive in archives:
        if not archive.is_relative_to(native) or str(archive) not in inventory:
            raise ValueError('native archive missing from bundle inventory')
        with archive.open('rb') as stream:
            thin = stream.read(8) == b'!<thin>\n'
        if thin:
            for name in subprocess.check_output(['ar', 't', str(archive)], text=True).splitlines():
                member = Path(name)
                member = (member if member.is_absolute() else archive.parent / member).resolve()
                if str(member) not in inventory:
                    raise ValueError('thin member missing from bundle inventory: ' + str(member))
                members.add(member)
    symbols = {line.split()[-1] for line in subprocess.check_output(
        ['nm', '--defined-only', str(bundle / 'link-check.app')], text=True).splitlines() if line.split()}
    required = {'cubit_mesa_service_start', 'cubit_mesa_service_close'}
    if combined:
        required |= {'softpipe_create_screen', 'util_make_vertex_passthrough_shader',
                     'util_make_fragment_tex_shader'}
    if not required <= symbols:
        raise ValueError('link check omitted required engine entrypoints')
    if subprocess.check_output(['nm', '-u', str(bundle / 'link-check.app')], text=True).strip():
        raise ValueError('unresolved native symbols')
    return {'status': 'VERIFIED_LINK_ONLY', 'combined': combined, 'continued': continued,
            'archives': len(archives), 'external_members': len(members),
            'executed': False, 'hardware_validated': False}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('bundle', type=Path)
    parser.add_argument('--combined', action='store_true')
    parser.add_argument('--continued', action='store_true',
                        help='verify continuation.json chained to unchanged failed build.json')
    args = parser.parse_args()
    print(json.dumps(check(args.build, args.bundle, args.combined, args.continued), indent=2))
