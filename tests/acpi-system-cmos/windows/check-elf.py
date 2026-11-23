from pathlib import Path
import json,struct,subprocess,hashlib,sys
w=Path(sys.argv[1]);root=w/'elf-path';raw=(root/'manifest.o').read_bytes();shoff=struct.unpack_from('<Q',raw,40)[0];shnum=struct.unpack_from('<H',raw,60)[0];sections={}
for i in range(shnum):
 off=struct.unpack_from('<Q',raw,shoff+i*64+24)[0];size=struct.unpack_from('<Q',raw,shoff+i*64+32)[0]
 if size>=4 and off+size<=len(raw):sections[raw[off:off+4]]=off
assert b'CBST'in sections and b'CBRS'in sections and b'CBDV'in sections
samples=[('normal','allow',raw)]
x=bytearray(raw);off=sections[b'CBST']+16+4*80;x[off+2]=1;struct.pack_into('<Q',x,off+8,1);samples.append(('granted-startup','bad-startup',x))
x=bytearray(raw);x[sections[b'CBRS']]=0;samples.append(('missing-resource','deny',x))
x=bytearray(raw);x[sections[b'CBDV']+12]=1;samples.append(('wrong-platform','deny',x))
x=bytearray(raw);x[sections[b'CBRS']+6]=2;samples.append(('resource-count','deny',x))
x=bytearray(raw);struct.pack_into('<Q',x,40,len(x)+64);samples.append(('outside-header','bad-elf',x))
rows=[]
for name,mode,data in samples:
 p=root/(name+'.o');p.write_bytes(data);argv=[str(root/'candidate/bin/procmgr_manifest_path_tests'),str(p),mode];r=subprocess.run(argv,capture_output=True,text=True,timeout=10);(root/(name+'.log')).write_text(r.stdout+r.stderr);assert r.returncode==0,(name,r.stdout,r.stderr);rows.append(dict(name=name,mode=mode,argv=argv,returncode=r.returncode,elf_sha256=hashlib.sha256(data).hexdigest()))
v=dict(passed=len(rows),total=len(rows),percent=100,scope='Actual CCL metadata inside hosted ELF, exact native Main findSection/readManifest/zero-map branch; capability minter callback modeled. No native/hardware execution.',cases=rows);(w/'ELF-PATH-RESULT.json').write_text(json.dumps(v,indent=2)+'\n');print('ACTUAL_ELF_PATH '+str(len(rows))+'/'+str(len(rows))+' 100%')
