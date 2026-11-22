import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const suite=import.meta.dirname,repo=path.resolve(suite,'../..');
if(!process.env.TMPDIR)throw Error('Export TMPDIR before entering Nix');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-pci-fields-')),pins={},copies={},commands=[],references=[];
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
let inventory;
function save(accepted=false,error=null){fs.writeFileSync(out+'/REPORT.json',JSON.stringify({accepted,state:accepted?'passed':error?'failed':'running',error,out,pins,copies,commands,references,scope:inventory?.scope,native:false,hardware:false,new_proof:false,goal_complete:false},null,2)+'\n');}
function verify(){for(const[p,h]of Object.entries({...pins,...copies}))if(!fs.existsSync(p)||hash(p)!==h)throw Error('Input changed: '+p);}
function run(name,args,seconds){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+name+'.log',log);commands.push({name,status:r.status,log});save();verify();console.log(name,log.trim().slice(-1000));if(r.status!==0||r.error)throw Error(name+' failed');return log;}
try{
 const catalog=suite+'/sources.json';pins[catalog]=hash(catalog);inventory=JSON.parse(fs.readFileSync(catalog));if(inventory.version!==2)throw Error('Unsupported source inventory');
 fs.mkdirSync(out+'/source');const names=new Set();
 for(const[rel,h]of Object.entries(inventory.inputs)){if(path.isAbsolute(rel)||rel.split('/').some(p=>p==='..'||p==='.'||!p))throw Error('Invalid input path');const p=repo+'/'+rel;if(!fs.existsSync(p)||hash(p)!==h)throw Error('Source mismatch: '+rel);pins[p]=h;if(/\.ad[bs]$/.test(rel)){const n=path.basename(rel);if(names.has(n))throw Error('Duplicate source '+n);names.add(n);fs.copyFileSync(p,out+'/source/'+n);copies[out+'/source/'+n]=h;}}
 for(const p of[suite+'/run.mjs',suite+'/run.sh'])pins[p]=hash(p);
 const pure=['provider_tests','combined_tests','protocol_tests','dispatch_tests','deadline_tests','alias_tests','geometry_tests','policy_tests','frontend_buses_tests'];
 function project(mode,mains,core=false){const flags=core?['-O2','-gnat2022','-gnatp']:['-O2','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])],name=core?'core':mode,gp=out+'/'+name+'.gpr';fs.writeFileSync(gp,`project ${name} is for Source_Dirs use("source");for Object_Dir use "${name}-obj";for Exec_Dir use "${name}-bin";for Main use(${mains.map(n=>JSON.stringify(n+'.adb')).join(',')});package Naming is for Body("ACPI_Target_Heap")use"acpi_target_heap_host.adb";end Naming;package Compiler is for Default_Switches("Ada")use(${flags.map(JSON.stringify).join(',')});end Compiler;end ${name};`);copies[gp]=hash(gp);run(name+'-build',['gprbuild','-q','-p','-j1','-P',gp],180);return out+'/'+name+'-bin/';}
 for(const mode of['checked','release']){const bin=project(mode,pure);for(const n of['protocol_tests','deadline_tests','alias_tests','geometry_tests','policy_tests','frontend_buses_tests'])run(mode+'-'+n,[bin+n],30);for(const k of[...Array.from({length:24},(_,i)=>i+1),26,27,28,29,30])run(mode+'-provider-'+k,[bin+'provider_tests',String(k)],30);for(const k of[1,2,3,4,5,6,7,8,14])run(mode+'-combined-'+k,[bin+'combined_tests',String(k)],30);for(let k=1;k<=20;k++)run(mode+'-dispatch-'+k,[bin+'dispatch_tests',String(k)],30);}
 // Whole Core code generation is functional release only. Pure components above
 // retain assertion/overflow checks; no assertion-enabled Core build is hidden.
 const bin=project('release',['startup_tests'],true);
 for(const[name,file,mode]of[['bus0','alias.asl',15],['bus1','nonzero-bus.asl',16],['no-bus-zero','no-bus-zero.asl',17],['alias-first','alias-first.asl',18]]){run(name+'-iasl',['iasl','-p',out+'/'+name,suite+'/'+file],30);copies[out+'/'+name+'.aml']=hash(out+'/'+name+'.aml');run(name+'-startup',[bin+'startup_tests',String(mode),out+'/'+name+'.aml'],30);const log=run(name+'-ACPICA',['acpiexec','-di','-dr','-fv','224','-b','evaluate OUT0; evaluate ALV0',out+'/'+name+'.aml'],30),values=[...log.matchAll(/\[Integer\] = ([0-9A-F]{16})/g)].map(m=>Number(BigInt('0x'+m[1])));references.push({name,values,diagnostics:log.split('\n').filter(l=>/error|warning|allocation/i.test(l))});if(values.join(',')!=='56,56')throw Error('ACPICA mismatch '+name);}
 verify();save(true);console.log(JSON.stringify({accepted:true,receipt:out+'/REPORT.json'}));
}catch(e){save(false,String(e.message??e));console.error(String(e.message??e));console.error(JSON.stringify({accepted:false,receipt:out+'/REPORT.json'}));process.exitCode=1;}
