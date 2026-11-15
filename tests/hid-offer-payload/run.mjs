import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const d=import.meta.dirname,root=path.resolve(d,'../..'),out=fs.mkdtempSync(process.env.TMPDIR+'/hid-offer-codec-'),pins={},runs=[],profiles=[],sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
fs.mkdirSync(out+'/source');for(const p of [d+'/run.mjs',d+'/WIRE.md',root+'/shared/platform/hid_offer_payload.ads',root+'/shared/platform/hid_offer_payload.adb',root+'/userspace/runtime/gnat/cubit-hid_offer_exchange.ads',root+'/userspace/runtime/gnat/cubit.ads',d+'/fixtures/codec_tests.adb']){pins[p]=sha(p);if(/\.ad[bs]$/.test(p)){const q=out+'/source/'+path.basename(p);fs.copyFileSync(p,q);pins[q]=sha(q);}}

function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('input drift '+p);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,profiles,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:'New pure HID offer codec/exchange declarations; independent JS golden vectors versus Ada encoder, full roundtrip, malformed fields, every truncation, unknown PCI presence and high array bounds in checked/release. No native producer/transport/devmgr launch/hardware or new proof.'},null,2)+'\n');}
function run(label,args,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576}),text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal});save(false);if(r.status!==0||r.error||r.signal)throw Error(label+' '+out);return text;}
// Independent spelling of WIRE.md's numeric layout. Do not parse Ada constants.
function golden(kind){const wide=kind==='wide',partial=kind==='partial',max=(1n<<64n)-1n;
 const spans=wide?[Buffer.from(Array.from({length:256},(_,i)=>(i+1)&255)),Buffer.alloc(256,0x55),Buffer.alloc(256,0xaa),Buffer.alloc(256,0xff)]:[Buffer.from('\\_SB.PC00.I2C1\0','ascii'),Buffer.alloc(0),Buffer.from('\\_SB.GPI0\0','ascii'),Buffer.alloc(0)];
 const b=Buffer.alloc(552+spans.reduce((n,p)=>n+p.length,0)),put=(o,w,v)=>{if(w===8)b.writeBigUInt64LE(BigInt(v),o);else if(w===4)b.writeUInt32LE(Number(v),o);else if(w===2)b.writeUInt16LE(Number(v),o);else b.writeUInt8(Number(v),o);};
 put(0,4,1);put(4,4,b.length);put(8,8,wide?max:0x100000007n);put(16,8,wide?max:17);put(24,8,wide?max:29);
 put(32,8,wide?65535:0);put(40,8,wide?255:0);put(48,8,wide?31*65536+7:0x150000);
 put(56,4,wide?0xffffffff:400000);put(60,2,wide?1023:0x15);put(62,2,wide?65535:0x20);put(64,2,wide?65535:0x4c);
 put(66,1,2);put(67,1,wide?255:1);put(68,1,wide?255:0);put(69,1,partial?5:7);put(70,2,wide?65535:0);
 put(74,1,wide?1:0);put(75,1,wide?1:0);put(76,1,wide?1:0);put(77,1,wide?2:1);put(78,1,1);put(79,1,wide?1:0);
 put(81,1,wide?255:0);put(82,2,wide?65535:0);put(84,2,wide?65535:0);put(86,1,wide?2:1);
 put(87,1,wide?32:4);put(88,1,wide?32:3);put(89,1,wide?32:2);for(let i=0;i<4;i++)put(90+2*i,2,spans[i].length);
 put(100,8,wide?max:0);b.write('INTC1057',108,'ascii');b.write(wide?'SSDT':'DSDT',116,'ascii');b.write(wide?'WIDECASE':'LENOVO  ',120,'ascii');
 put(128,8,wide?max:0x1122334455667788n);put(136,4,wide?0xffffffff:6);put(140,1,3);
 put(144,8,wide?max:0x9988776655443322n);put(152,8,wide?max:0xfeddccbbaa998877n);b.write('PNP0C50\0',160,'ascii');
 const paths=wide?[Array(32).fill('ABCD'),Array(32).fill('ABCD'),Array(32).fill('ABCD')]:[['_SB_','PC00','I2C1','TPD0'],['_SB_','PC00','I2C1'],['_SB_','GPI0']];
 for(let p=0;p<3;p++)paths[p].forEach((s,i)=>b.write(s,168+p*128+i*4,'ascii'));let at=552;for(const s of spans){s.copy(b,at);at+=s.length;}return b;
}
for(const kind of ['basic','wide','partial']){const p=out+'/'+kind+'.bin';fs.writeFileSync(p,golden(kind));pins[p]=sha(p);}
for(const mode of ['checked','release']){const p=out+'/'+mode+'.gpr';fs.writeFileSync(p,`project Test is for Source_Dirs use("source");for Object_Dir use"${mode}/obj";for Exec_Dir use"${mode}/bin";for Main use("codec_tests.adb");package Compiler is for Default_Switches("Ada") use("-O1","-gnat2022","-gnatwa","-gnatwe",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;end Test;`);pins[p]=sha(p);run(mode+'-build',['gprbuild','-q','-p','-j1','-P',p],180);const cases=[];for(const kind of ['basic','wide','partial']){const s=run(mode+'-'+kind,[out+'/'+mode+'/bin/codec_tests',out+'/'+kind+'.bin',kind],30),m=/^HID offer codec checks\s+(\d+)\n$/.exec(s);if(!m)throw Error('verdict');cases.push({kind,checks:Number(m[1])});}profiles.push({mode,cases});}
verify();save(true);console.log(JSON.stringify({completed:true,out,profiles}));
