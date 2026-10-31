import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
import{audit,controls}from'./audit.mjs';
const here=path.dirname(import.meta.filename),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
let root=path.resolve(here,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++)if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else if(process.argv[i]==='--validate-only')validate=true;else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
const catalog=JSON.parse(fs.readFileSync(here+'/sources.json')),inventory=JSON.parse(fs.readFileSync(here+'/inventory.json')),pins={},sources=[];
if(catalog.version!==1||catalog.inputs.length!==3)throw Error('Proof source inventory');
for(const row of catalog.inputs){const p=path.resolve(root,row.path);if(!p.startsWith(root+'/')||fs.realpathSync(p)!==p||hash(p)!==row.sha256)throw Error('Source pin');pins[p]=row.sha256;sources.push(p);}
for(const[name,h]of Object.entries(inventory.source_sha256)){const p=sources.find(p=>path.basename(p)===name);if(!p||hash(p)!==h)throw Error('Proof obligation/source linkage');}
if(new Set(sources.map(p=>path.basename(p))).size!==3)throw Error('Duplicate proof unit');
for(const name of ['run.sh','run.mjs','audit.mjs','inventory.json','sources.json'])pins[here+'/'+name]=hash(here+'/'+name);
if(validate){console.log(JSON.stringify({validated:true,sources:3,entities:inventory.entities.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/ec-supervisor-proof-');fs.mkdirSync(out+'/source');console.log(out);
for(const p of sources){const to=out+'/source/'+path.basename(p);fs.copyFileSync(p,to);pins[to]=hash(to);}
const tools={};for(const name of ['gnatprove','gprbuild','cvc5']){const p=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync));if(!p.startsWith('/nix/store/'))throw Error('Nix required');tools[name]=p;pins[p]=hash(p);}
const commands=[],reports=[];
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
for(const level of [1,2]){
 const dir=out+'/level'+level;fs.mkdirSync(dir);const project=dir+'/proof.gpr';
 fs.writeFileSync(project,'project Proof is\n for Source_Dirs use ("../source");\n for Object_Dir use "obj";\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022","-gnatwa","-gnatwe");\n end Compiler;\nend Proof;\n');pins[project]=hash(project);verify();
 const args=[tools.gnatprove,'-P',project,'-u','acpi_ec_supervisor_control.adb','--mode=all','--level='+level,'--timeout=5','-j1','--report=all','--prover=cvc5','--checks-as-errors=on'];
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','180',...args],{cwd:out,encoding:'utf8',timeout:183000,maxBuffer:1048576});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/level'+level+'.log',log);commands.push({level,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
 if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Proof failed level'+level);
 const p=dir+'/obj/gnatprove/acpi_ec_supervisor_control.spark',report=JSON.parse(fs.readFileSync(p));
 reports.push({level,...audit(report,inventory),rejected_controls:controls(report,inventory)});fs.copyFileSync(p,out+'/level'+level+'.spark');
}
verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({commands,inputs:pins,tools,reports,new_proof:true,scope:'Actual supervisor admission/retained-completion policy only under its contracts. Callback truth,source authentication,Core/GC,loop and physical lifetime remain outside; no whole-driver/interpreter/hardware proof.',whole_service_proved:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log(JSON.stringify(reports));
