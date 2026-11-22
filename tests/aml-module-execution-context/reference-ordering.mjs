import fs from 'node:fs';import path from 'node:path';import {fileURLToPath} from 'node:url';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const w=path.dirname(fileURLToPath(import.meta.url));
if(!process.env.TMPDIR||!path.isAbsolute(process.env.TMPDIR))throw Error('Set absolute TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/aml-loadtable-ordering-');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const bin='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/';
const inputs={[w+'/reference-ordering.mjs']:sha(w+'/reference-ordering.mjs')},commands=[],rows=[];
for(const[n,h]of Object.entries({iasl:'1c6a21c90ff6f846e27a98dbb3106362c706f67db8ceca21ccd81ab76a7d9dcb',acpiexec:'a161f8b3a0b94b4301c1d0f2ccca6c0f808d2998cd4c9e037a9d7b218c88dcfe'})){
 if(sha(bin+n)!==h)throw Error('Tool drift '+n);inputs[bin+n]=h;
}
function run(label,args){
 for(const[p,h]of Object.entries(inputs))if(sha(p)!==h)throw Error('Input drift '+p);
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:8000});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 fs.writeFileSync(out+'/inputs.json',JSON.stringify(inputs,null,2)+'\n');
 if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+' '+out);return log;
}
console.log(out);
for(const revision of [1,2])for(const mode of ['omitted','existing','relative','absolute','parent','missing']){
 const slash=String.fromCharCode(92), parameter=mode==='omitted'?'':mode==='existing'?'PRE0':mode==='absolute'?slash+'DST0.NEWV':mode==='parent'?'^DST0.NEWV':mode==='missing'?'MISS':'NEWV';
 const loader=`DefinitionBlock ("", "DSDT", ${revision}, "CUBIT", "LATEPATH", 1) {
 Name (CNT0, Zero) Device (DST0) { Name (_HID, "CUB0001") Name (PRE0, Zero) }
 Method (RUN0, 0) { Return (LoadTable ("OEM1", "CUBIT", "LATEPATH", ${JSON.stringify(slash+'DST0')}, ${JSON.stringify(parameter)}, 0x99)) }
 }`;
 const module=`DefinitionBlock ("", "OEM1", ${revision}, "CUBIT", "LATEPATH", 1) {
 External (${slash}CNT0, IntObj) Increment (${slash}CNT0) Name (NEWV, 0x2A)
 }`;
 const prefix=out+'/'+mode+'-r'+revision;
 for(const[name,text]of Object.entries({loader,module})){
  fs.writeFileSync(prefix+'-'+name+'.asl',text);inputs[prefix+'-'+name+'.asl']=sha(prefix+'-'+name+'.asl');
  run(mode+'-r'+revision+'-'+name,[bin+'iasl','-oa','-r',String(revision),'-p',prefix+'-'+name,prefix+'-'+name+'.asl']);
  inputs[prefix+'-'+name+'.aml']=sha(prefix+'-'+name+'.aml');
 }
 const log=run(mode+'-r'+revision+'-execute',[bin+'acpiexec','-dr','-b','evaluate RUN0; evaluate CNT0; evaluate DST0.NEWV; evaluate DST0.PRE0',prefix+'-loader.aml',prefix+'-module.aml']);
 const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>'0x'+BigInt('0x'+m[1]).toString(16));
 const failures=log.split('\n').filter(l=>l.startsWith('Evaluation of ')&&l.includes(' failed '));
 const diagnostics=log.split('\n').filter(l=>/ACPI (Error|Warning)/.test(l));
 rows.push({revision,mode,integers,failures,diagnostics});
 fs.writeFileSync(out+'/OBSERVATIONS.json',JSON.stringify({rows,scope:'Independent ACPICA observations; comparison verdict is separate'},null,2)+'\n');
}
for(const row of rows){
 const good=['omitted','existing'].includes(row.mode);
 const expected=good?[row.revision===1?'0xffffffff':'0xffffffffffffffff','0x1','0x2a',row.mode==='existing'?'0x99':'0x0']:['0x0','0x0'];
 if(JSON.stringify(row.integers)!==JSON.stringify(expected)||row.failures.length!==(good?0:2)||(!good&&(!row.failures.some(s=>s.includes('RUN0 failed with status AE_NOT_FOUND'))||!row.failures.some(s=>s.includes('DST0.NEWV failed with status AE_NOT_FOUND')))))throw Error('Reference ordering changed '+row.mode+' '+out);
}
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({rows,commands:commands.length,hosted:true,new_proof:false,native_executed:false},null,2)+'\n');
console.log(JSON.stringify(rows));
