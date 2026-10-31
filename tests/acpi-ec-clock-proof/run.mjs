import fs from 'node:fs';import path from 'node:path';import {spawnSync}from'node:child_process';import{fileURLToPath}from'node:url';
import {sha,recipes,inputs,validate}from'./check.mjs';import{audit,negativeControls}from'./audit.mjs';
const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..'),validateOnly=false;
for(let i=2;i<process.argv.length;i++){
 if(process.argv[i]==='--validate-only')validateOnly=true;
 else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
 else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
}
if(validateOnly){console.log(JSON.stringify(validate(here,root)));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR before Nix');
const sources=inputs(here,root),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-ec-clock-proof-');fs.mkdirSync(out+'/source');console.log(out);
const pins={},tools={},recipePins=Object.fromEntries(recipes.map(n=>[n,sha(here+'/'+n)]));
for(const name of ['gnatprove','gprbuild','cvc5']){
 const p=process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync);
 if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('Nix tool '+name);
 const real=fs.realpathSync(p);tools[name]={path:real,sha256:sha(real)};pins[real]=sha(real);
}
for(const[rel,h]of Object.entries(sources)){
 const dest=out+'/source/'+path.basename(rel);fs.copyFileSync(root+'/'+rel,dest);pins[root+'/'+rel]=h;pins[dest]=h;
}
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift');for(const[n,h]of Object.entries(recipePins))if(sha(here+'/'+n)!==h)throw Error('Recipe drift');}
const commands=[],reports={};
for(const level of [1,2]){
 const dir=out+'/level'+level;fs.mkdirSync(dir);
 const project=dir+'/proof.gpr';fs.writeFileSync(project,`project Proof is\n for Source_Dirs use ("../source");\n for Object_Dir use "obj";\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022", "-gnatwa", "-gnatwe");\n end Compiler;\n package Prove is\n for Proof_Dir use "proof";\n end Prove;\nend Proof;\n`);pins[project]=sha(project);
 const args=[tools.gnatprove.path,'-P',project,'-u','acpi_ec_clock_policy.adb','--level='+level,'--timeout=5','-j1','--report=all','--prover=cvc5','--checks-as-errors=on'];
 verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','180',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:183000});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/level'+level+'.log',log);
 commands.push({args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
 if(r.status!==0||r.signal||r.error)throw Error('Proof failed: '+out);
 const from=dir+'/obj/gnatprove/acpi_ec_clock_policy.spark',report=JSON.parse(fs.readFileSync(from));
 reports[level]={...audit(report),negative_controls:negativeControls(report)};
 fs.copyFileSync(from,out+'/level'+level+'.spark');fs.copyFileSync(dir+'/obj/gnatprove/gnatprove.out',out+'/level'+level+'-summary.txt');
}
verify();fs.writeFileSync(out+'/INPUTS.json',JSON.stringify(pins,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({sources,recipes:recipePins,tools,commands,reports,scope:'Pure EC clock policy Decode/Slice only; no callback, clock hardware, interpreter or native proof.'},null,2)+'\n');
console.log('EC clock policy proof accepted: 14 checks/level; 40 rejected negative controls.');
