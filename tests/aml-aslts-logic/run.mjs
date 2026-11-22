import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
const runner=fileURLToPath(import.meta.url),here=path.dirname(runner);
let root=path.resolve(here,'../..'),validateOnly=false;
for(let i=2;i<process.argv.length;i++){
  if(process.argv[i]==='--validate-only')validateOnly=true;
  else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
  else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
}
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
const catalogPath=here+'/sources.json',catalog=JSON.parse(fs.readFileSync(catalogPath));
if(catalog.version!==1)throw Error('Catalog version');
const pins={[catalogPath]:sha(catalogPath),[runner]:sha(runner)},files=new Map();
for(const e of catalog.inputs){
  const f=path.resolve(root,e.path),name=path.basename(f);
  if(!f.startsWith(root+path.sep)||files.has(name)||sha(f)!==e.sha256)throw Error('Source baseline '+e.path);
  files.set(name,f);pins[f]=e.sha256;
}
const main=here+'/aslts_ordered_runner.adb';
if(sha(main)!==catalog.runner_sha256)throw Error('Ada runner baseline');
files.set('aslts_ordered_runner.adb',main);pins[main]=catalog.runner_sha256;
for(const e of catalog.references){
  const f=path.resolve(here,e.path);
  if(!f.startsWith(here+path.sep)||sha(f)!==e.sha256)throw Error('Reference baseline');pins[f]=e.sha256;
}
for(const test of catalog.cases){
  const obs=JSON.parse(fs.readFileSync(here+'/reference/'+test.width+'/OBSERVATIONS.json'));
  const commands=JSON.parse(fs.readFileSync(here+'/reference/'+test.width+'/commands.json'));
  if(commands.length!==3||commands.some(c=>c.status!==0||c.signal||c.error)||!obs.repair_disabled||sha(here+'/'+test.file)!==test.sha256||test.sha256!==obs.aml_sha256)throw Error('Oracle provenance');
  if(obs.segments.length!==4||obs.segments.some(s=>s.integers.length!==1||s.integers[0]!=='0000000000000000')||JSON.stringify(obs.segments[0].records)!==JSON.stringify(test.records)||JSON.stringify(obs.segments[2].records)!==JSON.stringify([...test.records,...test.records]))throw Error('Original oracle result');
  if(test.records.length!==9||test.expected_MN00!==0||test.expected_ERRS!==0||JSON.stringify(test.expected_RMRC)!=='[9,18]')throw Error('Expected original outcomes');
}
function verify(){
  for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift '+f);
  for(const[dir,list]of Object.entries(catalog.listings)){
    const now=fs.readdirSync(root+'/'+dir,{withFileTypes:true}).filter(e=>e.isFile()&&/\.ad[bs]$/.test(e.name)).map(e=>e.name).sort();
    if(JSON.stringify(now)!==JSON.stringify(list))throw Error('Source inventory changed '+dir);
  }
}
verify();
if(validateOnly){console.log(JSON.stringify({mode:'validate-only',inputs:catalog.inputs.length,references:catalog.references.length,cases:catalog.cases.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set TMPDIR before Nix');
const compiler=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gprbuild').find(fs.existsSync));
if(!compiler.startsWith('/nix/store/'))throw Error('Use Nix development environment');pins[compiler]=sha(compiler);
const out=fs.mkdtempSync(process.env.TMPDIR+'/aslts-logic-'),commands=[],results=[];
console.log(out);fs.mkdirSync(out+'/source');
for(const[name,f]of files){fs.copyFileSync(f,out+'/source/'+name);pins[out+'/source/'+name]=sha(f);}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
function run(label,args,seconds){
  verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],
    {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
  const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
  commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
  fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
  if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+'; '+out);return log;
}
const flags=['-O1','-gnat2022','-gnatwa','-gnatwe','-fstack-usage','-gnatp'];
fs.writeFileSync(out+'/hosted.gpr',`project Hosted is\n for Source_Dirs use ("source");\n for Main use ("aslts_ordered_runner.adb");\n for Object_Dir use "obj";\n for Exec_Dir use "bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(f=>JSON.stringify(f)).join(',')});\n end Compiler;\nend Hosted;\n`);
run('build',[compiler,'-p','-j1','-P',out+'/hosted.gpr'],180);
for(const name of fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){
  const lines=fs.readFileSync(out+'/obj/'+name,'utf8').split('\n');
  for(const f of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+f))throw Error('ALI flags');
  if(lines.includes('A -gnata')||lines.includes('A -gnato'))throw Error('Wrong profile');
}
for(const test of catalog.cases){
  const log=run(test.width,[out+'/bin/aslts_ordered_runner',here+'/'+test.file],30);
  for(const pattern of [/^RESULT 0$/gm,/^ERRS_RESULT 0$/gm])if([...log.matchAll(pattern)].length!==2)throw Error('Original zero outcomes missing');
  for(const n of test.expected_RMRC)if(!log.includes('COMPLETION RMRC '+n+' ALL_RECORDS_LOGIC_PASS RETAINED_RESTORED'))throw Error('Stored record/cleanup observation missing');
  const charges=[...log.matchAll(/^MN00_STATUS RETURNED CHARGED\s+(\d+)$/gm)].map(m=>Number(m[1]));
  if(charges.length!==2)throw Error('Invocation count');results.push({width:test.width,charges,records:test.expected_RMRC,aml_sha256:test.sha256});
}
verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({status:'original-full-ASLTS-logic-n64-n32-release-passed',results,commands:commands.length,profile:'release',new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log('Complete '+JSON.stringify(results));
