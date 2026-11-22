import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const dir=path.dirname(import.meta.filename),sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/module-datum-oracle-');console.log(out);const bin='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/',pins={},commands=[],results=[];
function run(label,args){const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','10',...args],{cwd:out,encoding:'utf8',timeout:13000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});if(r.status!==0||r.signal||r.error)throw Error(label);return log;}
for(const[name,expected]of [['wide',[0xCD,0xAB,0xABCD]],['partial',[0xB5,0xEA,0xAB]]]){
 const source=dir+'/fixtures/'+name+'.asl';pins[source]=sha(source);fs.copyFileSync(source,out+'/'+name+'.asl');pins[bin+'iasl']=sha(bin+'iasl');pins[bin+'acpiexec']=sha(bin+'acpiexec');
 run('iasl-'+name,[bin+'iasl','-p',name,name+'.asl']);
 const text=run('acpica-'+name,[bin+'acpiexec','-l','-di','-dr','-fv','0','-b','execute _INI; evaluate FLD1; evaluate FLD2; evaluate OUT0',out+'/'+name+'.aml']);
 const values=[...text.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));if(JSON.stringify(values)!==JSON.stringify(expected)||/Evaluation of .* failed|Aborting method/.test(text))throw Error('scalar comparison '+name+' '+values);
 results.push({name,values,cache_cleanup_diagnostic:text.includes('Outstanding cache allocations')});
}
for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('source drift');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({results,commands,inputs:pins,scope:'Independent simulated-EC scalar/partial-preserve values after explicit initializer: provider initial bytes5/E0 correspond to initialization stores. Does not compare automatic startup ordering, native protocol or hardware.',native_executed:false,new_proof:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify(results));
