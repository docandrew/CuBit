import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const suite=import.meta.dirname,repo=path.resolve(suite,'../..');
if(!process.env.TMPDIR)throw Error('Export TMPDIR before entering Nix');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-kernel-pci-'));
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),pins={},commands=[],names=new Set();
function save(accepted=false,error=null){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,error,out,pins,commands,scope:'Actual Physical_Range_Map body, classifier and MCFG admission executed with modeled ACPI copy, boot-memory, VM and console dependencies. Checked/release correctness; release synthetic enumeration host timing. No real firmware copy, mappings, kernel boot or hardware execution.',native:false,hardware:false,new_proof:false},null,2)+'\n');}
function source(base,rel,expected,copy){
 if(typeof rel!=='string'||!rel||path.isAbsolute(rel)||rel.split('/').some(x=>!x||x==='.'||x==='..')||!/^[a-f0-9]{64}$/.test(expected))throw Error('Invalid manifest entry');
 const p=path.join(base,rel);if(!fs.lstatSync(p).isFile()||hash(p)!==expected)throw Error('Source mismatch: '+rel);pins[p]=expected;
 if(copy){const n=path.basename(rel);if(names.has(n)||!/^[a-zA-Z0-9_-]+\.ad[bs]$/.test(n))throw Error('Duplicate/invalid source: '+n);names.add(n);const target=out+'/source/'+n;fs.copyFileSync(p,target);pins[target]=expected;}
}
function verify(){for(const[p,h]of Object.entries(pins))if(!fs.existsSync(p)||hash(p)!==h)throw Error('Input drift: '+p);}
function run(name,args,seconds){verify();const started=Date.now(),r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+name+'.log',log);commands.push({name,status:r.status,milliseconds:Date.now()-started,log});save();verify();if(r.status!==0||r.error)throw Error(name+' failed');console.log(name,log.trim());}
try{
 for(const n of ['run.sh','run.mjs','sources.json'])pins[suite+'/'+n]=hash(suite+'/'+n);
 const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));if(catalog.version!==1||!catalog.inputs||!catalog.fixtures||!catalog.references)throw Error('Unsupported manifest');
 fs.mkdirSync(out+'/source');for(const[rel,h]of Object.entries(catalog.inputs))source(repo,rel,h,true);for(const[rel,h]of Object.entries(catalog.fixtures))source(suite,rel,h,true);for(const[rel,h]of Object.entries(catalog.references))source(repo,rel,h,false);
 // The host stubs model no hardware, but preserve these real capacity inputs.
 for(const[unit,name]of [['acpi','MAX_IOAPICS'],['config','MAX_BOOT_MEMORY_REGIONS'],['config','MIN_PHYS_ALLOC']]){
  const pattern=new RegExp('\\b'+name+'\\s*:\\s*constant\\s*:=\\s*([^;]+);','i');
  const real=pattern.exec(fs.readFileSync(repo+'/kernel/src/'+unit+'.ads','utf8'))?.[1].replaceAll('_','').toLowerCase();
  const mock=pattern.exec(fs.readFileSync(out+'/source/'+unit+'.ads','utf8'))?.[1].replaceAll('_','').toLowerCase();
  if(!real||real!==mock)throw Error('Mock capacity differs: '+name);
 }
 for(const mode of ['checked','release']){
  const mains=['adapter_tests','catalog_tests',...(mode==='release'?['enumeration_cost']:[])],flags=['-O2','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])],gp=out+'/'+mode+'.gpr';
  fs.writeFileSync(gp,`project ${mode} is for Source_Dirs use ("source");for Object_Dir use "${mode}-obj";for Exec_Dir use "${mode}-bin";for Main use (${mains.map(n=>JSON.stringify(n+'.adb')).join(',')});package Compiler is for Default_Switches("Ada") use (${flags.map(JSON.stringify).join(',')});end Compiler;end ${mode};`);pins[gp]=hash(gp);
  run(mode+'-build',['gprbuild','-q','-p','-j1','-P',gp],180);
  for(const n of mains)run(mode+'-'+n,[out+'/'+mode+'-bin/'+n],30);
 }
 verify();save(true);console.log(JSON.stringify({accepted:true,receipt:out+'/RESULT.json'}));
}catch(e){save(false,String(e.message??e));console.error(String(e.message??e));console.error(JSON.stringify({accepted:false,receipt:out+'/RESULT.json'}));process.exitCode=1;}
