import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
const here=path.dirname(fileURLToPath(import.meta.url));
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
let root=path.resolve(here,'../..'),validateOnly=false;
for(let i=2;i<process.argv.length;i++){
 if(process.argv[i]==='--validate-only')validateOnly=true;
 else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
 else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
}
const catalog=JSON.parse(fs.readFileSync(here+'/sources.json'));
if(catalog.version!==1)throw Error('Catalog version');
const pins={[here+'/sources.json']:sha(here+'/sources.json'),[here+'/run.mjs']:sha(here+'/run.mjs')};
function checked(base,entry){
 const f=path.resolve(base,entry.path);
 if(!f.startsWith(base+path.sep)||fs.lstatSync(f).isSymbolicLink()||fs.realpathSync(f)!==f||sha(f)!==entry.sha256)throw Error('Source pin '+entry.path);
 pins[f]=entry.sha256;return f;
}
const inputs=catalog.inputs.map(e=>checked(root,e)),tests=catalog.tests.map(e=>checked(here,e));
const names=[...inputs,...tests].map(f=>path.basename(f));
if(new Set(names).size!==names.length)throw Error('Duplicate source basename');
if(validateOnly){console.log(JSON.stringify({validated:true,inputs:inputs.length,tests:tests.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-ec-boot-pipeline-');console.log(out);fs.mkdirSync(out+'/source');
for(const f of [...inputs,...tests]){const dest=out+'/source/'+path.basename(f);fs.copyFileSync(f,dest);pins[dest]=sha(dest);}
const tools={};
for(const name of ['gprbuild','iasl']){
 const f=name==='iasl'?'/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl':process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync);
 if(!f||!fs.existsSync(f))throw Error('Tool '+name);const real=fs.realpathSync(f);if(!real.startsWith('/nix/store/'))throw Error('Nix tool required');tools[name]=real;pins[real]=sha(real);
}
const commands=[],counts={};
function verify(){for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift '+f);}
function run(label,args,seconds){
 verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
 if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+'; '+out);return log;
}
run('iasl',[tools.iasl,'-p',out+'/ec-fields',out+'/source/binding.asl'],5);pins[out+'/ec-fields.aml']=sha(out+'/ec-fields.aml');
run('ecdt-template',[tools.iasl,'-T','ECDT'],5);
const ecdtTemplate=out+'/ecdt.asl',boot=out+'/boot.asl';
const text=fs.readFileSync(ecdtTemplate,'utf8');
if(!text.includes('Namepath : ""'))throw Error('ECDT template changed');
fs.writeFileSync(boot,text.replace('Namepath : ""','Namepath : "\\EC00"'));
pins[ecdtTemplate]=sha(ecdtTemplate);pins[boot]=sha(boot);
run('ecdt-compile',[tools.iasl,'-p',out+'/ecdt',boot],5);pins[out+'/ecdt.aml']=sha(out+'/ecdt.aml');
for(const profile of ['checked','release']){
 counts[profile]={};const flags=['-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato','-O2']:['-gnatp','-O3'])];
 for(const kind of ['binding']){
  const dir=out+'/'+profile+'/'+kind;fs.mkdirSync(dir,{recursive:true});const main='ec_boot_binding_tests.adb',project=dir+'/ec_fields.gpr';
  fs.writeFileSync(project,`project EC_Fields is\n for Source_Dirs use ("../../source");\n for Main use (${JSON.stringify(main)});\n for Object_Dir use "obj";\n for Exec_Dir use "bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(x=>JSON.stringify(x)).join(',')});\n end Compiler;\nend EC_Fields;\n`);pins[project]=sha(project);
  run(profile+'-'+kind+'-build',[tools.gprbuild,'-p','-j1','-P',project],180);
  const exe=dir+'/bin/'+path.basename(main,'.adb');pins[exe]=sha(exe);
  const log=run(profile+'-'+kind+'-runtime',[exe,out+'/ec-fields.aml',out+'/ecdt.aml'],30);
  const pattern=/EC boot binding checks:\s+(\d+)/,match=pattern.exec(log);
  if(!match||Number(match[1])<=0)throw Error('Missing checks');counts[profile][kind]=Number(match[1]);
  const alis=fs.readdirSync(dir+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
  if(!alis.length)throw Error('No ALI evidence');
  for(const name of alis){const lines=fs.readFileSync(dir+'/obj/'+name,'utf8').split('\n');for(const flag of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('ALI profile '+name);}
 }
}
if(JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('Profiles disagree');verify();
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands:commands.length,actual_ECDT_namespace_UID_binding:true,actual_namespace_discovery_and_resource_comparison:true,evaluator_failure_observations_injected:true,locked_fields_supported:false,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify(counts));
