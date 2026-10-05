#!/usr/bin/env python3
"""Build and run the audio sink failure suite in a disposable native CuBit VM."""
import argparse,hashlib,json,os,shlex,shutil,subprocess,tempfile,time
from pathlib import Path
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--suite',choices=['sink','hub','player'],default='sink')
parser.add_argument('--kernel',type=Path)
parser.add_argument('--metadata',type=Path)
a=parser.parse_args()
root=Path(__file__).resolve().parents[3];source=Path(__file__).resolve().parent
os.chdir(root)
if not os.environ.get('IN_NIX_SHELL'):parser.error('run inside nix develop')
kernel=(a.kernel or root/'kernel/cubit_kernel').resolve()
if not kernel.is_file():parser.error('build the CuBit kernel first')
metadata=a.metadata
if metadata is None:
 result=json.loads(subprocess.check_output(['nix','build','--impure','--no-link','--json','--file',str(root/'userspace/servo/media/pinned-environment.nix'),'--argstr','repo',str(root)],text=True))
 metadata=Path(result[0]['outputs']['out'])
config=json.loads(metadata.read_text());assert config['schema']==1
for archive in config['archives']:assert Path(archive).is_file(),archive
env=dict(os.environ,PKG_CONFIG_PATH=config['pkg_config_path'],PKG_CONFIG_LIBDIR='',PKG_CONFIG_ALLOW_CROSS='1')
flags=shlex.split(subprocess.check_output(['pkg-config','--cflags','gstreamer-base-1.0','gstreamer-app-1.0'],env=env,text=True))
(source/'build').mkdir(exist_ok=True)
p=Path(tempfile.mkdtemp(prefix='native-',dir=source/'build'));print('ARTIFACTS:',p,flush=True)
(p/'cpio').mkdir();(p/'iso/boot/grub').mkdir(parents=True)
media=root/'userspace/servo/media'
test_source=source/{'sink':'main.c','hub':'hub.c','player':'player.c'}[a.suite]
extra_sources=[str(media/'penny-audio-hub.c')] if a.suite in ('hub','player') else []
if a.suite=='player':extra_sources.append(str(media/'penny-audio-player.c'))
args=[str(root/'userspace/libc/cubit-cc'),'-O2','-Wall','-Wextra','-Werror','-Wl,--gc-sections','-I'+str(media),*flags,str(media/'penny-audio-sink.c'),str(test_source),*extra_sources,'-Wl,--start-group',*config['archives'],'-Wl,--end-group','-lm','-o',str(p/'audio-test')]
(p/'link.json').write_text(json.dumps(args,indent=2)+'\n');subprocess.run(args,check=True)
(p/'child.S').write_text('.section .rodata\n.balign 16\n.global media_start,media_end\nmedia_start:\n.incbin "'+str(p/'audio-test')+'"\nmedia_end:\n.section .note.GNU-stack,"",@progbits\n')
subprocess.run(['gcc','-O2','-Wall','-Wextra','-Werror','-ffreestanding','-fno-stack-protector','-fno-pic','-mno-red-zone','-nostdlib','-static','-no-pie','-Wl,-z,stack-size=8388608','-Wl,-T,userspace/c/link.ld',str(source/'start.S'),str(source/'supervisor.c'),str(p/'child.S'),'-o',str(p/'cpio/devmgr.svc')],check=True)
shutil.copyfile(kernel,p/'iso/boot/cubit_kernel');shutil.copyfile(root/'tests/intel-gpu/native/demand-grub.cfg',p/'iso/boot/grub/grub.cfg')
with (p/'iso/boot/initrd.img').open('wb') as out:subprocess.run(['cpio','-o','-H','newc'],input=b'devmgr.svc\n',cwd=p/'cpio',stdout=out,check=True)
with (p/'package.log').open('w') as out:subprocess.run(['grub-mkrescue','-o',str(p/'test.iso'),str(p/'iso')],stdout=out,stderr=subprocess.STDOUT,check=True)
inputs=[p/'audio-test',kernel,media/'penny-audio-sink.c',media/'penny-audio-sink.h',test_source]+([media/'penny-audio-hub.c',media/'penny-audio-hub.h'] if a.suite in ('hub','player') else [])
if a.suite=='player':inputs += [media/'penny-audio-player.c',media/'penny-audio-player.h']
(p/'inputs.json').write_text(json.dumps({str(f):hashlib.sha256(f.read_bytes()).hexdigest() for f in inputs},indent=2)+'\n')
serial=p/'serial.log'
prefix={'sink':'GSTREAMER-OUTPUT','hub':'GSTREAMER-HUB','player':'GSTREAMER-PLAYER-OUTPUT'}[a.suite]
with (p/'qemu.log').open('w') as log:
 vm=subprocess.Popen(['qemu-system-x86_64','-machine','q35','-accel','tcg,thread=multi','-cpu','Broadwell','-smp','4','-m','256','-display','none','-monitor','none','-serial',f'file:{serial}','-no-reboot','-cdrom',str(p/'test.iso')],stdout=log,stderr=log)
 try:
  deadline=time.monotonic()+90
  while True:
   text=serial.read_text(errors='replace') if serial.exists() else ''
   assert not any(x in text for x in ['GSTREAMER-SMOKE: FAIL',prefix+': FAIL','USER-MEMORY-FAULT','PANIC','EXCEPTION','Last chance']),text[-3000:]
   if 'MEDIA-SUPERVISOR: ordinary child resumed' in text and prefix+': PASS' in text:
    if a.suite=='sink':
     assert 'GSTREAMER-OUTPUT: audio registry PASS' in text
     for mode in range(8):assert f'GSTREAMER-OUTPUT: mode={mode} PASS' in text
    elif a.suite=='hub':
     for marker in ['nine sources exact','three late joins exact','blocked producer cancelled','device stall reported','independent drain exact sample boundaries PASS']:assert marker in text
    else:
     for marker in ['idle session never opens device PASS','missing device fails playback and releases session PASS','seek replay, paused preroll and scheduled-buffer cancellation PASS','factory lifetime and unbound rejection PASS']:assert marker in text
    (p/'result.json').write_text(json.dumps({'result':'PASS','scope':'Native CuBit sink with injected transport; no real HDA','suite':a.suite})+'\n')
    print('PASS native audio suite:',serial,flush=True);break
   assert vm.poll() is None,'QEMU exited before completion'
   assert time.monotonic()<deadline,'Native audio test timeout: '+str(serial)
   time.sleep(.2)
 finally:
  if vm.poll() is None:
   vm.terminate()
   try:vm.wait(timeout=5)
   except subprocess.TimeoutExpired:vm.kill();vm.wait()
