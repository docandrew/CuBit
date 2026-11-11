import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';

const suite=import.meta.dirname,repo=path.resolve(suite,'../..');
if(!process.env.TMPDIR)throw Error('Export TMPDIR before entering Nix');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-root-export-'));
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const pins={},commands=[],names=new Set();
let proof=null;
function save(accepted=false,error=null){
 fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,error,out,pins,commands,proof,
  scope:'Pure root inventory count admission. Checked and release exhaustive ceiling partitions, one-over and extreme demands. Optional level-2 SPARK of Root_Export_Admission only. Does not execute Modules, publish capabilities, render diagnostics or validate native boot/hardware.',
  native:false,hardware:false},null,2)+'\n');
}
function verify(){for(const[p,h]of Object.entries(pins))if(!fs.existsSync(p)||hash(p)!==h)throw Error('Input drift: '+p);}
function install(base,rel,expected){
 if(typeof rel!=='string'||!rel||path.isAbsolute(rel)||rel.split('/').some(x=>!x||x==='.'||x==='..')||!/^[a-f0-9]{64}$/.test(expected))throw Error('Invalid source manifest entry');
 const source=path.join(base,rel),name=path.basename(rel);
 if(!fs.lstatSync(source).isFile()||hash(source)!==expected)throw Error('Source mismatch: '+rel);
 if(names.has(name)||!/^\w+\.ad[bs]$/.test(name))throw Error('Invalid or duplicate Ada source: '+name);
 names.add(name);pins[source]=expected;
 const copy=out+'/source/'+name;fs.copyFileSync(source,copy);pins[copy]=expected;
}
function run(name,args,seconds){
 verify();const started=Date.now();
 const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+name+'.log',log);
 commands.push({name,args,status:r.status,milliseconds:Date.now()-started,log});save();verify();
 if(r.status!==0||r.error)throw Error(name+' failed');console.log(name,log.trim());
}
try{
 const args=process.argv.slice(2);
 if(args.length>1||(args.length===1&&args[0]!=='--prove'))throw Error('Usage: run.sh [--prove]');
 for(const name of ['run.mjs','run.sh','sources.json'])pins[suite+'/'+name]=hash(suite+'/'+name);
 const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));
 if(catalog.version!==1||!catalog.inputs||!catalog.fixtures)throw Error('Unsupported manifest');
 fs.mkdirSync(out+'/source');
 for(const[rel,h]of Object.entries(catalog.inputs))install(repo,rel,h);
 for(const[rel,h]of Object.entries(catalog.fixtures))install(suite,rel,h);
 for(const mode of ['checked','release']){
  const gp=out+'/'+mode+'.gpr',flags=['-O2','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];
  fs.writeFileSync(gp,`project ${mode} is for Source_Dirs use ("source");for Object_Dir use "${mode}-obj";for Exec_Dir use "${mode}-bin";for Main use ("root_capacity_tests.adb");package Compiler is for Default_Switches("Ada") use (${flags.map(JSON.stringify).join(',')});end Compiler;end ${mode};`);pins[gp]=hash(gp);
  run(mode+'-build',['gprbuild','-q','-p','-j1','-P',gp],180);
  run(mode+'-test',[out+'/'+mode+'-bin/root_capacity_tests'],30);
 }
 if(args[0]==='--prove'){
  const gp=out+'/proof.gpr';
  fs.writeFileSync(gp,'project Proof is for Source_Dirs use ("source");for Object_Dir use "proof-obj";package Compiler is for Default_Switches("Ada") use ("-gnat2022");end Compiler;end Proof;');pins[gp]=hash(gp);
  run('proof',['gnatprove','-P',gp,'-u','root_export_admission.adb','--mode=all','--level=2','--prover=cvc5,z3','--checks-as-errors=on','-j1'],180);
  const report=out+'/proof-obj/gnatprove/root_export_admission.spark',s=JSON.parse(fs.readFileSync(report));
  if(s.progress!=='PROGRESS_PROOF'||s.stop_reason!=='STOP_REASON_NONE'||!s.proof?.length||[...s.proof,...s.flow].some(c=>c.severity!=='info')||['pragma_assume','skip_proof','skip_flow_proof'].some(k=>!Array.isArray(s[k])||s[k].length))throw Error('Incomplete proof');
  proof={report,sha256:hash(report),checks:s.proof.length,unproved:0,assumptions:0};
 }
 verify();save(true);console.log(JSON.stringify({accepted:true,receipt:out+'/RESULT.json'}));
}catch(e){save(false,String(e.message??e));console.error(String(e.message??e));console.error(JSON.stringify({accepted:false,receipt:out+'/RESULT.json'}));process.exitCode=1;}
