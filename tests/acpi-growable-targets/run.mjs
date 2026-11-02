import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const test=import.meta.dirname,root=path.resolve(test,'../..'),pins={},commands=[];
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR=/home/doc/cubit-build-tmp before Nix and inside the command');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-growable-targets-');fs.mkdirSync(out+'/source');
const catalog=JSON.parse(fs.readFileSync(test+'/sources.json')),names=new Set();
for(const[rel,h]of Object.entries(catalog.inputs)){
 const p=root+'/'+rel;if(!fs.lstatSync(p).isFile()||fs.realpathSync(p)!==p||hash(p)!==h)throw Error('input mismatch '+rel);pins[p]=h;
 if(/\.ad[bs]$/.test(rel)){const name=path.basename(rel);if(names.has(name))throw Error('duplicate source '+name);names.add(name);fs.copyFileSync(p,out+'/source/'+name);pins[out+'/source/'+name]=hash(out+'/source/'+name);}
}
for(const n of['run.sh','run.mjs','sources.json'])pins[test+'/'+n]=hash(test+'/'+n);
const fixture=root+'/'+catalog.fixture;fs.copyFileSync(fixture,out+'/capacity.asl');pins[out+'/capacity.asl']=hash(out+'/capacity.asl');
const report={out,pins,commands,scope:catalog.scope,state:'running',new_proof:false,native:false,goal_complete:false};
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift '+p);}
function run(label,args,seconds){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message});fs.writeFileSync(out+'/REPORT.json',JSON.stringify(report,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error(label+' failed; see '+out);console.log(label+' '+log.slice(-700));return log;}
console.log(out);
try{
run('iasl',['iasl','-tc','capacity.asl'],30);pins[out+'/capacity.aml']=hash(out+'/capacity.aml');const ref=run('ACPICA',['acpiexec','-b','evaluate BOOT','capacity.aml'],30);if(!/\[Integer\]\s*=\s*0000000000000000/.test(ref))throw Error('ACPICA fixture result');report.reference_diagnostics=ref.split('\n').filter(l=>/error|warning|allocation/i.test(l));
for(const profile of['checked','release']){
 const gp=out+'/'+profile+'.gpr';fs.writeFileSync(gp,`project ${profile} is
 for Source_Dirs use ("source");
 for Excluded_Source_Files use ("acpi_target_heap.adb");
 for Main use ("storage_tests.adb","failure_tests.adb","capacity_format_tests.adb","provider_tests.adb","growable_owner_tests.adb","field_capacity_format_tests.adb");
 for Object_Dir use "${profile}/obj";for Exec_Dir use "${profile}";
 package Naming is for Body ("ACPI_Target_Heap") use "acpi_target_heap_host.adb";end Naming;
 package Compiler is for Default_Switches ("Ada") use (${profile==='checked'?'"-O0"':'"-O1"'},"-gnat2022","-gnatwa","-gnatwe",${profile==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;
end ${profile};
`);pins[gp]=hash(gp);run(profile+'-build',['gprbuild','-q','-p','-j1','-P',gp],180);
 for(const stem of['storage_tests','capacity_format_tests','field_capacity_format_tests','provider_tests']){const log=run(profile+'-'+stem,[out+'/'+profile+'/'+stem],30);if(!log.includes('PASS '))throw Error('test did not report PASS');}
 const oom=run(profile+'-allocator-refusal',['bash','-c','ulimit -v 131072; exec "$1"','--',out+'/'+profile+'/failure_tests'],30);if(!oom.includes('PASS allocation refusal'))throw Error('refusal untested');
 run(profile+'-owner-growth',[out+'/'+profile+'/growable_owner_tests','13',out+'/capacity.aml'],30);
 const refused=run(profile+'-owner-capacity',[out+'/'+profile+'/growable_owner_tests','14',out+'/capacity.aml'],30);for(const token of['aml-field fail CAPACITY name=F032 region=SANV','demand= 33 capacity= 32','PASS growable owner'])if(!refused.includes(token))throw Error('missing '+token);
}
verify();report.state='passed';
}catch(e){report.state='failed';report.error=e.message;process.exitCode=1;}
fs.writeFileSync(out+'/REPORT.json',JSON.stringify(report,null,2)+'\n');console.log(JSON.stringify({state:report.state,error:report.error,out}));
