from pathlib import Path
import json,subprocess,struct,hashlib
import argparse
parser=argparse.ArgumentParser(description="Validate the native capture fixture and reader fault handling")
parser.add_argument('--reader',type=Path,required=True)
parser.add_argument('--output',type=Path,required=True)
args=parser.parse_args()
source=Path(__file__).parent/'fixtures/native201.cubittrace'
out=args.output;out.mkdir(parents=True,exist_ok=True)
reader=args.reader.resolve();data=source.read_bytes()
assert hashlib.sha256(data).hexdigest()=="250097b02a1aea0de9f402297f0fd3b8ecf4aa17507bac48066c3bcdd8115f9e"
r=subprocess.run([str(reader),str(source)],capture_output=True,text=True,timeout=30)
assert r.returncode==0 and 'ARCHIVE: COMPLETE events= 78' in r.stdout
(out/'valid.log').write_text(r.stdout+r.stderr)
cases={'missing-footer':data[:-256],'short-footer':data[:-1],'extra-byte':data+b'x','replayed-event':data[:512]+data[256:512]+data[512:],'reordered-events':data[:256]+data[512:768]+data[256:512]+data[768:]}
bad=bytearray(data);bad[300]^=1;cases['payload-corruption']=bytes(bad)
bad=bytearray(data);bad[-17]^=1;cases['footer-corruption']=bytes(bad)
reports={}
for name,contents in cases.items():
 p=out/(name+'.cubittrace');p.write_bytes(contents);r=subprocess.run([str(reader),str(p)],capture_output=True,text=True,timeout=30);assert r.returncode!=0,name;assert 'ARCHIVE: COMPLETE' not in r.stdout,name;(out/(name+'.log')).write_text(r.stdout+r.stderr);reports[name]={'exit':r.returncode,'bytes':len(contents),'sha256':hashlib.sha256(contents).hexdigest()}
# Independent byte-level verification of every chunk, including its checksum.
for offset in range(0,len(data),256):
 block=data[offset:offset+256];h=0xcbf29ce484222325
 for v in block[:248]:h=((h^v)*0x100000001b3)&((1<<64)-1)
 assert struct.unpack_from('<Q',block,248)[0]==h,offset
(out/'result.json').write_text(json.dumps({'status':'PASS','scope':'Actual native file, independent byte-level checksums, reader rejects seven corrupted/truncated/reordered variants; not filesystem fault injection','original_sha256':hashlib.sha256(data).hexdigest(),'chunks':len(data)//256,'cases':reports},indent=2)+'\n')
print('PASS native archive 80 chunk checksums and seven reader negative controls')
