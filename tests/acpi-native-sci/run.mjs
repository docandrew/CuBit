import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const suite=import.meta.dirname,tree=path.resolve(suite,'../..');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
let mode='release',validate=false;
for(let i=2;i<process.argv.length;i++){
 if(process.argv[i]==='--validate-only')validate=true;
 else if(process.argv[i]==='--mode'&&['checked','release'].includes(process.argv[++i]))mode=process.argv[i];
 else throw Error('Usage: run.sh [--mode checked|release] [--validate-only]');
}
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json')),pins={};
for(const name of ['sources.json','run.mjs','run.sh'])pins[suite+'/'+name]=sha(suite+'/'+name);
function regular(base,rel){
 if(path.isAbsolute(rel)||rel.split(/[\\/]/).some(p=>p==='..'||p===''))throw Error('Invalid source path');
 let p=base;for(const part of rel.split('/')){p+='/'+part;if(fs.lstatSync(p).isSymbolicLink())throw Error('Linked input '+p);}
 if(!fs.lstatSync(p).isFile())throw Error('Nonregular input '+p);return p;
}
for(const [base,rows] of [[tree,catalog.inputs],[suite,catalog.helpers]])for(const row of rows){
 const p=regular(base,row.path);if(!/^[0-9a-f]{64}$/.test(row.sha256)||sha(p)!==row.sha256)throw Error('Input mismatch '+p);pins[p]=row.sha256;
}
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,inputs:catalog.inputs.length,helpers:catalog.helpers.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
function tool(name,nix=true){
 const p=process.env.PATH.split(path.delimiter).map(d=>d+'/'+name).find(p=>fs.existsSync(p));if(!p)throw Error('Missing '+name);
 const real=fs.realpathSync(p);if(nix&&!real.startsWith('/nix/store/'))throw Error('Use Nix '+name);pins[p]=sha(p);pins[real]=sha(real);return p;
}
const compiler=tool('gprbuild'),timeout=tool('timeout'),nice=tool('nice'),limit=tool('prlimit',false);
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-native-sci-'),frozen=out+'/source',commands=[],results=[];
fs.mkdirSync(frozen);const names=new Set();
for(const[base,rows]of [[tree,catalog.inputs],[suite,catalog.helpers]])for(const row of rows){
 const name=path.basename(row.path);if(names.has(name))throw Error('Duplicate basename '+name);names.add(name);
 fs.copyFileSync(base+'/'+row.path,frozen+'/'+name);pins[frozen+'/'+name]=sha(frozen+'/'+name);
}
function save(complete){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({complete,out,mode,pins,commands,results,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:catalog.scope},null,2)+'\n');}
function run(label,args,seconds){
 verify();const r=spawnSync(nice,['-n','19',timeout,'--kill-after=2s',String(seconds),limit,'--as=25769803776','--stack=67108864','--core=0',...args],{cwd:out,encoding:'utf8',timeout:(seconds+4)*1000,maxBuffer:1048576});
 const log=out+'/'+label+'.log';fs.writeFileSync(log,(r.stdout??'')+(r.stderr??''));commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null,log,sha256:sha(log)});save(false);
 if(r.status!==0||r.signal||r.error)throw Error('Failed '+label+' '+log);verify();return fs.readFileSync(log,'utf8');
}
const flags=['-O0','-gnat2022','-gnatwa','-gnatwe','-gnatw.X',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];
const project=out+'/admission.gpr';
fs.writeFileSync(project,'project Admission is for Source_Dirs use ("source");for Main use ("sci_tests.adb");for Object_Dir use "obj";for Exec_Dir use "bin";package Compiler is for Default_Switches("Ada") use ("-O0","-gnat2022","-gnatp");'+catalog.selected.map(n=>'for Switches("'+n+'") use ('+flags.map(JSON.stringify).join(',')+');').join('')+'end Compiler;end Admission;');pins[project]=sha(project);
console.log(out);run('build',[compiler,'-q','-p','-j1','-P',project],180);
const text=run('sci',[out+'/bin/sci_tests'],30),match=/Native SCI execution checks\s+(\d+)/.exec(text);
if(!match||Number(match[1])!==catalog.expected)throw Error('Coverage mismatch');
results.push({checks:Number(match[1])});
const alis=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
for(const name of alis){
 const p=out+'/obj/'+name,lines=fs.readFileSync(p,'utf8').split('\n'),stem=name.slice(0,-4);
 const on=mode==='checked'&&(catalog.selected.includes(stem+'.adb')||catalog.selected.includes(stem+'.ads'));
 for(const flag of on?['-gnata','-gnato']:['-gnatp'])if(!lines.includes('A '+flag))throw Error('Missing profile '+name);
 if(lines.includes('A '+(on?'-gnatp':'-gnata')))throw Error('Wrong profile '+name);pins[p]=sha(p);
}
verify();save(true);console.log('Passed SCI '+mode+': 74 checks; receipt '+out+'/RESULT.json');
