import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
const aml=process.argv[2],tool='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec';
if(!aml||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Expected compiled fixture and TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/ec-startup-oracle-');
const pins={[aml]:sha(aml),[tool]:sha(tool),[new URL(import.meta.url).pathname]:sha(new URL(import.meta.url).pathname)};
const args=[tool,'-l','-di','-dr','-fv','0','-b',
 'execute \\_INI; evaluate STUP; execute EC00._REG 3 1; evaluate STUP; evaluate EC00._STA; execute EC00._INI; evaluate STUP; evaluate RVAL; execute EC00._Q01; evaluate EVCT; evaluate EVST; evaluate LID0._LID',aml];
const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5',...args],{encoding:'utf8',maxBuffer:1048576,timeout:8000});
const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/acpica.log',log.slice(0,1048576));
for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift');
const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));
const notify=/Received a Device Notify on \[LID0\].*Value 0x80/.test(log);
const accepted=r.status===0&&!r.error&&!r.signal&&Buffer.byteLength(log)<=1048576&&notify
 &&!/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log)
 &&JSON.stringify(integers)===JSON.stringify([16,1,15,2,0,1,0,0]);
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,args,status:r.status,signal:r.signal,error:r.error?.message??null,integers,lid_notify:notify,inputs:pins,scope:'Explicit root/region/device initialization and handler semantics with simulated zero-filled EC region; no physical protocol,automatic startup ordering or native IPC claim.'},null,2)+'\n');
console.log(JSON.stringify({out,accepted,integers}));if(!accepted)process.exit(1);
