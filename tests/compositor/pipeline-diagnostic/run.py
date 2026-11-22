"""Hosted scalar diagnostic tests; requires Nix and explicit frozen inputs."""
import argparse, os, subprocess, tempfile, json
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--source-root',type=Path,required=True)
p.add_argument('--generated',type=Path,required=True)
p.add_argument('--vulkan-include',type=Path,required=True)
a=p.parse_args()
assert os.environ.get('IN_NIX_SHELL'), 'Run in Nix'
s=a.source_root.resolve()/'userspace/lib/compositor'
w=Path(tempfile.mkdtemp(prefix='cubit-pipeline-diag-test-'))
subprocess.run(['cc','-std=c11','-Wall','-Wextra','-Werror','-I',str(s),'-I',str(a.generated.resolve()),'-I',str(a.vulkan_include.resolve()),str(Path(__file__).with_name('check.c')), *[str(s/n) for n in ('vulkan_affine.c','vulkan_checker.c','vulkan_sources.c')],'-o',str(w/'check')],check=True)
for n in range(16):subprocess.run([str(w/'check'),str(n)],check=True)
subprocess.run([str(w/'check'),'6','1000297000'],check=True)
for stage,names in json.loads(Path(__file__).with_name('lookups.json').read_text()).items():
 for index,name in enumerate(names,1):subprocess.run([str(w/'check'),'0','vk'+name,stage,str(index)],check=True)
print('PASS',w)
