#!/usr/bin/env python3
"""Mutation check for the persistent client-source tests.

Builds the source churn and glyph residency tests in a scratch copy once
unmodified (must PASS) and once per mutation that reintroduces a churn or
eviction defect (each must FAIL). Run inside the Nix shell; never writes to the checkout.
"""
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DIRS = ['userspace/lib/compositor', 'userspace/lib/display', 'userspace/runtime/gnat',
        'userspace/lib/image', 'userspace/lib/theme', 'userspace/services/desktop', 'tests/compositor']
SKIP = shutil.ignore_patterns('build*', 'obj', 'gnatprove', '*.o', '*.ali', 'proof*', '__pycache__')

CHURN = ('desktop_source_churn.gpr', 'build/desktop-source-churn/desktop_source_churn_tests', [],
         'PASS desktop source churn')
GLYPH = ('desktop_glyph_residency.gpr', 'build/desktop-glyph-residency/desktop_glyph_residency_tests', ['0'],
         'PASS 128 resident/pinned glyphs')
STALL = ('stall_watch.gpr', 'build/stall-watch/stall_watch_tests', [], 'PASS stall watch')
SCENE = ('desktop_scene_sources.gpr', 'build/desktop-scene-sources/desktop_scene_sources_tests', [],
         'PASS desktop scene sources')
BACKDROP = ('desktop_backdrop.gpr', 'build/desktop-backdrop/desktop_gpu_scene-backdrop-tests', [],
            'PASS wallpaper capture')
TESTS = (CHURN, GLYPH, STALL, SCENE, BACKDROP)
MUTATIONS = {
    'reallocate on every new version': (CHURN,
        'userspace/services/desktop/desktop_image_source.adb',
        'if Image.Width /= S.Width or else Image.Height /= S.Height then',
        'if True then'),
    'ignore damage notes (copy every row)': (CHURN,
        'userspace/services/desktop/desktop_image_registry.adb',
        'Book.Note (S.Keys, Key, C.Band (Rows.First, Rows.Last));',
        'null;'),
    'evict most recently used': (CHURN,
        'userspace/lib/compositor/compositor_source_residency.adb',
        'S.Entries (I).Last_Use < S.Entries (Best).Last_Use',
        'S.Entries (I).Last_Use > S.Entries (Best).Last_Use'),
    'free the image when its client buffer returns': (CHURN,
        'userspace/services/desktop/desktop_image_registry.adb',
        '         I.Detach (S.Owners (N), Pixels, Detached);',
        '         I.Detach (S.Owners (N), Pixels, Detached);\n'
        '         if Detached and then S.Keys.Entries (N).Key /= C.No_Key then\n'
        '            Book.Unbind (S.Keys, N);\n'
        '         end if;'),
    'free the glyph cell on eviction': (GLYPH,
        'userspace/services/desktop/desktop_glyph_residency.adb',
        'Retire (S, C.At_Slot (S.Cache, Found), True, OK);',
        'Retire (S, C.At_Slot (S.Cache, Found), False, OK);'),
    'glyph cells not prepared at renderer start': (GLYPH,
        'userspace/services/desktop/desktop_glyph_residency.adb',
        "      if S.Stopping or else S.Stage /= Idle or else not D.Can_Retire_Readers then return; end if;\n      for Position in C.Slot loop",
        "      if True then return; end if;\n      for Position in C.Slot loop"),
    'stall ignores upload progress': (STALL,
        'userspace/lib/compositor/compositor_stall_watch.ads',
        'not S.Armed or else Uploads /= S.Uploads or else Now < S.Last',
        'not S.Armed or else Now < S.Last'),
    'stall counted from idle time': (STALL,
        'userspace/lib/compositor/compositor_stall_watch.ads',
        ' or else\n      Now - S.Last >= Deadline);',
        ');'),
    'wallpaper inherits the previous clip': (BACKDROP,
        'userspace/services/desktop/desktop_gpu_scene-backdrop.adb',
        '      Set_Clip (S, Damage, Accepted);\n      if not Accepted then return; end if;\n',
        ''),
    'scene budget back to 512 layers': (SCENE,
        'userspace/lib/compositor/vulkan_submission.ads',
        'Maximum_Draws : constant := 16384;',
        'Maximum_Draws : constant := 4096;'),
}


def run(tree: Path, test) -> bool:
    project, exe, args, marker = test
    build = subprocess.run(['gprbuild', '-p', '-q', '-P', project],
                           cwd=tree / 'tests/compositor', capture_output=True, text=True)
    if build.returncode != 0:
        print(build.stdout[-2000:], build.stderr[-2000:])
        raise SystemExit('build failed')
    result = subprocess.run(['./' + exe, *args], cwd=tree / 'tests/compositor',
                            capture_output=True, text=True)
    return result.returncode == 0 and marker in result.stdout


def main() -> int:
    with tempfile.TemporaryDirectory(prefix='source-churn-mutation-') as scratch:
        tree = Path(scratch)
        for d in DIRS:
            shutil.copytree(ROOT / d, tree / d, ignore=SKIP, symlinks=True)
        runtime = ROOT / 'userspace/runtime'
        for item in runtime.iterdir():
            if item.name != 'gnat' and not (tree / 'userspace/runtime' / item.name).exists():
                (tree / 'userspace/runtime' / item.name).symlink_to(item)
        if not all(run(tree, test) for test in TESTS):
            print('FAIL unmodified sources do not pass')
            return 1
        print('baseline PASS')
        killed = 0
        for name, (test, path, old, new) in MUTATIONS.items():
            target = tree / path
            original = target.read_text()
            if original.count(old) != 1:
                print(f'FAIL mutation anchor missing: {name}')
                return 1
            target.write_text(original.replace(old, new))
            survived = run(tree, test)
            target.write_text(original)
            print(('SURVIVED ' if survived else 'killed   ') + name)
            killed += not survived
        if killed != len(MUTATIONS):
            print(f'FAIL {len(MUTATIONS) - killed} mutation(s) survived')
            return 1
        print(f'PASS source churn mutation check: {killed}/{len(MUTATIONS)} mutations killed')
        return 0


if __name__ == '__main__':
    sys.exit(main())
