import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {audit} from './audit.mjs';
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
  const file=path.resolve(root,e.path),name=path.basename(file);
  if(!file.startsWith(root+path.sep)||files.has(name)||sha(file)!==e.sha256)throw Error('Source baseline '+e.path);
  files.set(name,file);pins[file]=e.sha256;
}
for(const e of catalog.local){
  const file=path.resolve(here,e.path),name=path.basename(file);
  if(!file.startsWith(here+path.sep)||files.has(name)||sha(file)!==e.sha256)throw Error('Proof fixture baseline');
  files.set(name,file);pins[file]=e.sha256;
}
if(sha(here+'/audit.mjs')!==catalog.audit_sha256)throw Error('Audit baseline');
pins[here+'/audit.mjs']=catalog.audit_sha256;
function verify(){for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift '+f);}
verify();
if(validateOnly){console.log(JSON.stringify({mode:'validate-only',inputs:catalog.inputs.length,local:catalog.local.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set TMPDIR before Nix');
const tool=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gnatprove').find(fs.existsSync));
if(!tool.startsWith('/nix/store/'))throw Error('Use Nix development environment');pins[tool]=sha(tool);
const out=fs.mkdtempSync(process.env.TMPDIR+'/gpio-client-proof-'),commands=[],results=[];console.log(out);
for(const level of [1,2])for(const kind of ['client','wire']){
  const dir=out+'/'+kind+'-'+level;fs.mkdirSync(dir+'/source',{recursive:true});
  for(const[name,file]of files){fs.copyFileSync(file,dir+'/source/'+name);pins[dir+'/source/'+name]=sha(file);}
  fs.writeFileSync(dir+'/proof.gpr','project Proof is\n for Source_Dirs use ("source");\n for Object_Dir use "obj";\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022","-gnatwa","-gnatwe");\n end Compiler;\nend Proof;\n');
  const args=[tool,'-P',dir+'/proof.gpr','-u',kind==='client'?'gpio_client_proof_instance.ads':'gpio_control_wire.adb',
    '--mode=all','--level='+level,'--timeout=5','--prover=cvc5','-j1','--report=all','--checks-as-errors=on'];
  verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','180',...args],
    {cwd:dir,encoding:'utf8',maxBuffer:1048576,timeout:183000});
  const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(dir+'/proof.log',log.slice(0,1048576));
  const command={args,status:r.status,signal:r.signal,error:r.error?.message??null};
  fs.writeFileSync(dir+'/command.json',JSON.stringify(command,null,2)+'\n');
  commands.push({kind,level,...command});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
  if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Proof command failed '+dir);
  const checked=audit(dir);results.push({kind,level,directory:dir,audit:checked});
  console.log(kind+' level '+level+': '+checked.proof_checks+' proof/'+checked.flow_checks+' flow; all SPARK, no skips');
}
verify();fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({results,commands:commands.length,scope:'Exact GPIO client instantiation and wire codec at levels1/2; trusted imported transport boundary, per-state token policy; no native/kernel/hardware or whole-AML proof.',native_executed:false,hardware_validated:false},null,2)+'\n');
console.log('Complete '+out);
