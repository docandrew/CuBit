import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
const here=path.dirname(fileURLToPath(import.meta.url)),from=process.argv[2];
if(process.argv.length!==3||!from||!path.isAbsolute(from)) throw Error('Pass the absolute output directory printed by run.sh');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp') throw Error('Set TMPDIR before Nix');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const exe='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec';
if(sha(exe)!=='a161f8b3a0b94b4301c1d0f2ccca6c0f808d2998cd4c9e037a9d7b218c88dcfe') throw Error('Normal ACPICA tool drift');
const out=fs.mkdtempSync(process.env.TMPDIR+'/aml-loadtable-coercion-reference-'),inputs={},commands=[],observations=[];
for(const p of [exe,here+'/reference.mjs',here+'/cases.json',from+'/COMPLETE.json',from+'/inputs.json']) inputs[p]=sha(p);
const generated=JSON.parse(fs.readFileSync(from+'/inputs.json'));
const cases=JSON.parse(fs.readFileSync(here+'/cases.json'));
function verify() { for(const[p,h]of Object.entries(inputs)) if(sha(p)!==h) throw Error('Reference input drift '+p); }
console.log(out);
for(const revision of [1,2]) for(const name of cases) {
 const files=['main','child'].map(part=>from+'/'+name+'-'+part+'-r'+revision+'.aml');
 for(const p of files) { if(sha(p)!==generated[p]) throw Error('Generated fixture drift'); inputs[p]=sha(p); }
 const label=name+'-r'+revision,args=[exe,'-dr','-b','evaluate GO00; evaluate MARK',...files];
 verify();
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','30',...args],
  {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:33000});
 const log=(r.stdout??'')+(r.stderr??''); fs.writeFileSync(out+'/'+label+'.log',log);
 commands.push({label,args,status:r.status,error:r.error?.message??null,signal:r.signal});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n'); verify();
 if(r.status!==0||r.error||r.signal) throw Error('Reference process failed '+label);
 const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>'0x'+BigInt('0x'+m[1]).toString(16));
 const diagnostics=log.split('\n').filter(l=>/AE_|failed with status/.test(l));
 const shortIdentifier=name==='buffer-oem'||name==='string-oem';
 const expectedError=name==='integer-oem'||name==='package-table'||(name==='integer-table'&&revision===2);
 const ones=revision===1?'0xffffffff':'0xffffffffffffffff';
 if(shortIdentifier) {
  const loaded=JSON.stringify(integers)===JSON.stringify([ones,'0x5b']);
  const absent=JSON.stringify(integers)==='["0x0"]'&&diagnostics.some(l=>l.includes('MARK')&&l.includes('AE_NOT_FOUND'));
  if(!loaded&&!absent) throw Error('Unexpected short-identifier observation '+label);
  observations.push({revision,name,integers,diagnostics,conformance_assertion:false,
   candidate_loads:true,reference_loads:loaded,selection_difference:!loaded,
   scope:'Short OEM identifier matching is recorded separately; not conversion conformance evidence'});
 } else {
  if(expectedError) {
   if(integers.length!==0||!diagnostics.some(l=>l.includes(name==='package-table'?'AE_AML_OPERAND_TYPE':'AE_AML_STRING_LIMIT')))
    throw Error('Reference refusal changed '+label);
  } else if(JSON.stringify(integers)!==JSON.stringify([ones,'0x5b'])||diagnostics.length)
   throw Error('Reference common effects changed '+label);
  observations.push({revision,name,integers,diagnostics,conformance_assertion:true,
   common_admission_values_effects:true,exact_error_status_equivalence:false});
 }
}
verify();
fs.writeFileSync(out+'/inputs.json',JSON.stringify(inputs,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({observations,commands:commands.length,
 common_controls:observations.filter(o=>o.conformance_assertion).length,
 short_identifier_observations:observations.filter(o=>!o.conformance_assertion).length,
 selection_differences:observations.filter(o=>o.selection_difference).length,
 new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log({common_controls:16,short_identifier_observations:4,
 selection_differences:observations.filter(o=>o.selection_difference).length});
