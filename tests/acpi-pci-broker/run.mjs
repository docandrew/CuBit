import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const suite=import.meta.dirname,repo=path.resolve(suite,'../..');
if(!process.env.TMPDIR)throw Error('Export TMPDIR before entering the Nix development environment');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-pci-broker-'));
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const commands=[],pins={},copied={},names=new Set();
let manifest;
function save(accepted=false,error=null){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,error,out,commands,pins,copied,scope:manifest?.scope??'Input validation incomplete',native:false,hardware:false,new_proof:false},null,2)+'\n');}
function checkedPath(base,rel){if(typeof rel!=='string'||!rel||path.isAbsolute(rel)||rel.split('/').some(x=>x==='..'||x==='.'||!x))throw Error('Invalid relative source path');return path.join(base,rel);}
function install(base,rel,expected){const source=checkedPath(base,rel);if(!/^[a-f0-9]{64}$/.test(expected)||!fs.existsSync(source)||hash(source)!==expected)throw Error('Source mismatch: '+rel);const n=path.basename(rel);if(names.has(n))throw Error('Duplicate Ada source name: '+n);names.add(n);pins[source]=expected;const target=out+'/source/'+n;fs.copyFileSync(source,target);copied[target]=expected;}
function verify(){for(const[p,h]of Object.entries({...pins,...copied}))if(!fs.existsSync(p)||hash(p)!==h)throw Error('Source changed: '+p);}
function run(mode,name,args,seconds){verify();const start=Date.now(),r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+mode+'-'+name+'.log',log);commands.push({mode,name,status:r.status,milliseconds:Date.now()-start,log});save();verify();if(r.status!==0||r.error)throw Error(mode+' '+name+' failed');console.log(mode,name,log.trim());}
try{
 const catalog=suite+'/sources.json';pins[catalog]=hash(catalog);manifest=JSON.parse(fs.readFileSync(catalog));if(manifest.version!==1||!manifest.inputs||!manifest.fixtures)throw Error('Unsupported source manifest');
 fs.mkdirSync(out+'/source');for(const[rel,h]of Object.entries(manifest.inputs))install(repo,rel,h);for(const[rel,h]of Object.entries(manifest.fixtures))install(suite,rel,h);
 for(const mode of['checked','release']){const gp=out+'/'+mode+'.gpr',flags=['-O2','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];fs.writeFileSync(gp,`project ${mode} is for Source_Dirs use("source");for Object_Dir use "${mode}-obj";for Exec_Dir use "${mode}-bin";for Main use("broker_tests.adb");package Compiler is for Default_Switches("Ada")use(${flags.map(JSON.stringify).join(',')});end Compiler;end ${mode};`);run(mode,'build',['gprbuild','-q','-p','-j1','-P',gp],180);run(mode,'broker',[out+'/'+mode+'-bin/broker_tests'],30);}
 verify();save(true);console.log(JSON.stringify({accepted:true,receipt:out+'/RESULT.json'}));
}catch(e){save(false,String(e.message??e));console.error(String(e.message??e));console.error(JSON.stringify({accepted:false,receipt:out+'/RESULT.json'}));process.exitCode=1;}
