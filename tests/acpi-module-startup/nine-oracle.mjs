import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const w=path.dirname(import.meta.filename),bin='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/',sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/module-nine-comparison-'),base=fs.readFileSync(w+'/fixtures/nine.asl','utf8'),inputs={},commands=[],observations=[];console.log(out);
function run(label,args){const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','10',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:13000});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});if(r.status!==0||r.signal||r.error)throw Error(label);return log;}
for(const[kind,text]of [['ec-tight',base],['memory-tight',base.replace('EmbeddedControl','SystemMemory')],['ec-padded',base.replace('EmbeddedControl, Zero, 9','EmbeddedControl, Zero, 16')]])for(const revision of [1,2]){
 const name=kind+'-r'+revision,source=out+'/'+name+'.asl';fs.writeFileSync(source,text);inputs[source]=sha(source);for(const tool of ['iasl','acpiexec'])inputs[bin+tool]=sha(bin+tool);
 run(name+'-iasl',[bin+'iasl','-r',String(revision),'-p',out+'/'+name,source]);
 const log=run(name+'-acpica',[bin+'acpiexec','-l','-di','-dr','-fv','0','-b','execute _INI; evaluate C000; evaluate C008; evaluate OUT0',out+'/'+name+'.aml']);
 const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16)),m=/\[Buffer\] Length 08 =\s+0000: ((?:[0-9A-F]{2} ){7}[0-9A-F]{2})/.exec(log),buffer=m?.[1];
 const region_limit=/Evaluation of .* failed with status AE_AML_REGION_LIMIT/.test(log);
 if(kind==='ec-tight'){
  if(!region_limit||JSON.stringify(integers)!==JSON.stringify([0x85,0xA5])||buffer!=='00 00 00 00 00 00 00 00')throw Error('tight EC reference discrepancy changed');
 }else if(region_limit||/Aborting method|Evaluation of .* failed/.test(log)||JSON.stringify(integers)!==JSON.stringify([0x85,0xA1])||buffer!=='88 77 66 55 44 33 22 11')throw Error('ordinary scalar/buffer comparison '+name+' '+integers+' '+buffer);
 observations.push({kind,revision,integers,buffer,region_limit,cache_cleanup_diagnostic:log.includes('Outstanding cache allocations')});
}
for(const[p,h]of Object.entries(inputs))if(sha(p)!==h)throw Error('input drift');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({observations,commands,inputs,scope:'Tight9-byte EC reference has a recorded region-limit discrepancy, not a passing CuBit equivalence claim. Ordinary byte-access SystemMemory and padded16-byte EC fixtures compare final first/last bytes and full8-byte buffer at both AML widths. No protocol/access-count, startup-order, hardware or proof claim.',native_executed:false,new_proof:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify(observations));
