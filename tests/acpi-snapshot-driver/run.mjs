import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
const suite=path.dirname(fileURLToPath(import.meta.url));
const root=path.resolve(suite,'../..');
const mode=process.argv[2]??'checked';
if(!['checked','release','--validate-only'].includes(mode)||process.argv.length>3)throw Error('usage: run.sh [checked|release|--validate-only]');
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const files=fs.readFileSync(path.join(suite,'source-files.txt'),'utf8').trim().split('\n');
const pins=new Map(fs.readFileSync(path.join(suite,'inputs.sha256'),'utf8').trim().split('\n').map(line=>{
 const m=/^([0-9a-f]{64})  (.+)$/.exec(line);if(!m)throw Error('invalid input hash');return [m[2],m[1]];
}));
if(new Set(files).size!==files.length||files.some(rel=>!pins.has(rel)))throw Error('duplicate/missing source pin');
function verify(){for(const [rel,expected] of pins){
 if(path.isAbsolute(rel)||rel.split('/').includes('..')||sha(fs.readFileSync(path.join(root,rel)))!==expected)throw Error('source changed: '+rel);
}}
verify();
if(mode==='--validate-only'){console.log('Controlled ACPI snapshot inputs verified; no compile.');process.exit(0);}
fs.mkdirSync(path.join(suite,'results'),{recursive:true});
const work=fs.mkdtempSync(path.join(suite,'results',mode+'-'));
fs.mkdirSync(path.join(work,'source'));fs.mkdirSync(path.join(work,'obj'));fs.mkdirSync(path.join(work,'bin'));
const names=new Set();
for(const rel of files){
 const name=path.basename(rel);if(!/\.(ads|adb)$/.test(name))continue;
 if(names.has(name))throw Error('source name collision: '+name);names.add(name);
 fs.copyFileSync(path.join(root,rel),path.join(work,'source',name));
}
const commands=[];
function run(label,args,seconds){
 const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:work,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
 fs.writeFileSync(path.join(work,label+'.log'),(r.stdout??'')+(r.stderr??''));
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
 fs.writeFileSync(path.join(work,'COMMANDS.json'),JSON.stringify(commands,null,2)+'\n');
 if(r.status!==0||r.signal||r.error)throw Error(label+' failed; see '+work);
 return r.stdout;
}
const mains=['controlled_tests','snapshot_startup_tests','local_diagnostic_tests','startup_diagnostic_format_tests'];
const switches=['-O1','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];
fs.writeFileSync(path.join(work,'host.gpr'),`project Host is
 for Source_Dirs use ("source");
 for Object_Dir use "obj";
 for Exec_Dir use "bin";
 for Main use (${mains.map(m=>JSON.stringify(m+'.adb')).join(', ')});
 package Compiler is
  for Default_Switches ("Ada") use (${switches.map(s=>JSON.stringify(s)).join(', ')});
 end Compiler;
end Host;
`);
for(const main of mains)run('build-'+main,['gprbuild','-p','-j1','-P','host.gpr',main+'.adb'],180);
for(const name of ['fixture','fatal']){
 fs.copyFileSync(path.join(suite,'models',name+'.asl'),path.join(work,name+'.asl'));
 run('iasl-'+name,['iasl','-p',name,name+'.asl'],30);
}
const oracle=run('acpica',['acpiexec','-b','evaluate MARK','fixture.aml'],30);
if(!/\[Integer\] = 000000000000005A/.test(oracle))throw Error('ACPICA initializer value mismatch');
const cases=['default','normal','unavailable','evaluation-exception','barrier-exception','before-deadline','after-deadline','source-exception','module-fatal'];
const observations=[];
for(const name of cases){
 const stdout=run(name,['bin/controlled_tests',name==='module-fatal'?'fatal.aml':'fixture.aml',name],30);
 const m=/controlled snapshot checks\s+(\d+)/.exec(stdout);if(!m)throw Error('missing controlled verdict');
 observations.push({name,checks:Number(m[1])});
}
for(const name of mains.slice(1)){
 const stdout=run(name,['bin/'+name],30),m=/checks\s+(\d+)/i.exec(stdout);if(!m)throw Error('missing baseline verdict');
 observations.push({name,checks:Number(m[1])});
}
verify();
const checks=observations.reduce((n,o)=>n+o.checks,0);
if(checks!==1258)throw Error('unexpected check total: '+checks);
fs.writeFileSync(path.join(work,'RESULT.json'),JSON.stringify({mode,checks,processes:observations.length,observations,source_files:files.length,ACPICA_initializer_control:true,new_proof:false,native_execution:false,hardware_validated:false,scope:'Actual copied-table loader and controlled device initialization; modeled source/provider callbacks. Module hardware access and native provider provisioning are not implemented by this change.'},null,2)+'\n');
console.log(JSON.stringify({mode,checks,processes:observations.length,result:path.join(work,'RESULT.json')}));
