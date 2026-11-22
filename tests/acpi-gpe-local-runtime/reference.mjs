import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const pins={},runs=[],hostOut=process.argv[2];
if(!hostOut?.startsWith(process.env.TMPDIR+'/acpi-gpe-local-runtime-'))throw Error('expected hosted fixture directory');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const out=fs.mkdtempSync(process.env.TMPDIR+'/gpe-same-core-reference-');
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA comparison of the same normal GPE AML fixtures: package result and device Notify. Does not validate CuBit ownership, masking, acknowledgement or hardware.',newProof:false,nativeExecuted:false},null,2)+'\n');}
for(const width of [1,2]){
 const source=hostOut+'/gpe'+width+'.aml',aml=out+'/gpe'+width+'.aml';
 fs.copyFileSync(source,aml);pins[source]=sha(source);pins[aml]=sha(aml);
 const p=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152','acpiexec','-di','-dr','-b','evaluate _GPE._L09',aml],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576});
 const output=(p.stdout??'')+(p.stderr??'');fs.writeFileSync(out+'/reference-'+width+'.log',output);runs.push({width,status:p.status,error:p.error?.message??null,signal:p.signal,output});save(false);
 if(p.status!==0||p.error||p.signal||/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(output))throw Error('reference execution');
 const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
 if(JSON.stringify(integers)!=='["1","2"]'||! /\[Package\] Contains 2 Elements/.test(output)||! /Received a Device Notify on \[TPD0\].*Value 0x80/.test(output))throw Error('typed result/Notify '+out);
 runs.at(-1).typedValues={integers,notify:128};
}
for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('input drift');save(true);console.log('GPE ACPICA references passed; '+out);
