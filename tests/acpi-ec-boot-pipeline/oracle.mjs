import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
const aml=process.argv[2],tool='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec';
if(!aml||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Expected compiled fixture and TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/ec-pipeline-oracle-');
const pins={[aml]:sha(aml),[tool]:sha(tool),[new URL(import.meta.url).pathname]:sha(new URL(import.meta.url).pathname)};
const args=[tool,'-l','-di','-dr','-b',
 'evaluate EC00._UID; evaluate ECOA._UID; evaluate EC01._UID; evaluate EC02._UID; evaluate BUS0.EC00._UID; evaluate EC06._UID; evaluate EC07._UID; evaluate EC03._UID; evaluate EC04._UID; evaluate CNT0; evaluate EC00._CRS; evaluate RCNT',aml];
const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5',...args],{encoding:'utf8',maxBuffer:1048576,timeout:8000});
const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/acpica.log',log.slice(0,1048576));
for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift');
const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));
const buffer=/\[Buffer\] Length 02 =\s+0000: 01 02\s+\/\//.test(log);
const string=/\[String\] Length 01 = "2"/.test(log);
const section=log.split('Evaluating \\EC00._CRS')[1]??'',bytes=[];
for(const row of section.matchAll(/(?:^|\n|=)\s*([0-9A-F]{4}): ([^\n]+)/g)){
 if(Number.parseInt(row[1],16)!==bytes.length)throw Error('Resource offset');
 const parts=row[2].split('//')[0].trim().split(/\s+/);
 if(parts.some(x=>!(/^[0-9A-F]{2}$/.test(x))))throw Error('Resource byte');
 bytes.push(...parts.map(x=>Number.parseInt(x,16)));
}
const resource=Buffer.from(bytes).toString('hex').toUpperCase();
const accepted=resource==='470162006200000147016600660000017900'&&r.status===0&&!r.error&&!r.signal&&Buffer.byteLength(log)<=1048576&&buffer&&string
 &&!/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log)
 &&JSON.stringify(integers)===JSON.stringify([0,0,1,2,7,0xFFFFFFFF,0x100000000,2,1]);
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,args,status:r.status,signal:r.signal,error:r.error?.message??null,integers,string,buffer,resource,inputs:pins,scope:'Explicit UID and CRS fixture semantics with repair disabled; not the composed discovery algorithm,physical EC or native acceptance.'},null,2)+'\n');
console.log(JSON.stringify({out,accepted,integers,string,buffer}));if(!accepted)process.exit(1);
