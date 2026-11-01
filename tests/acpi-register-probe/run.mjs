import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const w=import.meta.dirname,root=path.resolve(w,'../..'),sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),pins={};
const manifest=w+'/inputs.sha256';pins[manifest]=sha(manifest);
for(const line of fs.readFileSync(manifest,'utf8').trim().split('\n')){const m=line.match(/^([0-9a-f]{64})\s+(.+)$/);if(!m||path.posix.normalize(m[2])!==m[2]||m[2].startsWith('/')||m[2].split('/').includes('..'))throw Error('Invalid test input');const p=root+'/'+m[2];if(fs.realpathSync(p)!==p||sha(p)!==m[1])throw Error('Input mismatch '+m[2]);pins[p]=m[1];}
if(process.env.TMPDIR!='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-timer-probe-');fs.mkdirSync(out+'/source');
const add=p=>{const dest=out+'/source/'+path.basename(p);if(fs.existsSync(dest))throw Error('duplicate '+p);pins[p]=sha(p);fs.copyFileSync(p,dest);pins[dest]=sha(dest);};
for(const d of ['userspace/services/acpi/lib','shared/firmware','shared/acpi'])for(const f of fs.readdirSync(root+'/'+d))if(/\.(ads|adb)$/.test(f))add(root+'/'+d+'/'+f);
add(root+'/userspace/services/acpi/backend/acpi_backend_register_ids.ads');
for(const f of fs.readdirSync(w+'/support'))if(/\.(ads|adb)$/.test(f)&&f!=='acpi_backend_register_ids.ads')add(w+'/support/'+f);
add(w+'/probe_tests.adb');
const verify=()=>{for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Drift '+p);};
const commands=[];
for(const profile of ['checked','release']){const d=out+'/'+profile;fs.mkdirSync(d);fs.writeFileSync(d+'/probe.gpr',`project Probe is
 for Source_Dirs use ("../source");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("probe_tests.adb");
 package Compiler is
 for Default_Switches ("Ada") use ("-gnat2022", "-gnatwa", "-gnatwe", ${profile==='checked'?'"-gnata", "-gnato"':'"-gnatp"'});
 end Compiler;
end Probe;\n`);
for(const [cmd,args,limit]of [['gprbuild',['-q','-j1','-p','-P',d+'/probe.gpr'],180],[d+'/probe_tests',[],30]]){verify();const r=spawnSync('timeout',['--kill-after=2s',String(limit),cmd,...args],{encoding:'utf8',timeout:(limit+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(d+'/'+(cmd==='gprbuild'?'build':'runtime')+'.log',log);commands.push({cmd,args,status:r.status,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2));console.log(profile,cmd,r.status,log.slice(-2500));verify();if(r.status!==0||r.signal||r.error)throw Error('Failure '+out);}}
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({inputs:pins,commands,native_executed:false,hardware_validated:false,proved:false},null,2));fs.writeFileSync(w+'/HOSTED-RESULT.json',JSON.stringify({out},null,2));console.log(out);
