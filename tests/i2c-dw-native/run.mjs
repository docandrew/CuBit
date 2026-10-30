import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..');
const mode=process.argv[2],output=process.argv[3];
if(!['tests','semantic'].includes(mode)||!path.isAbsolute(output??'')||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Need tests|semantic, absolute private output and pinned environment');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const inventory=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={};
for(const [relative,expected]of Object.entries(inventory.production)){
 const p=root+'/'+relative;if(fs.realpathSync(p)!==p||sha(p)!==expected)throw Error('Production drift '+relative);pins[p]=expected;
}
for(const [relative,expected]of Object.entries(inventory.assets)){
 const p=here+'/'+relative;if(fs.realpathSync(p)!==p||sha(p)!==expected)throw Error('Test asset drift');pins[p]=expected;
}
for(const n of ['run.mjs','run.sh','sources.json','README.md'])pins[here+'/'+n]=sha(here+'/'+n);
fs.mkdirSync(output,{recursive:true});const out=fs.mkdtempSync(output+'/i2c-native-'+mode+'-');console.log(out);
const tool=fs.realpathSync(process.env.PATH.split(':').map(p=>p+'/gprbuild').find(fs.existsSync));
if(!tool.startsWith('/nix/store/'))throw Error('Unpinned compiler');
fs.writeFileSync(out+'/tool.json',JSON.stringify({path:tool,sha256:sha(tool)},null,2)+'\n');
const commands=[];
function run(label,args,seconds){
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576),{flag:'wx'});
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 if(r.status!==0||r.error||log.length>1048576)throw Error('Failed '+label);return log;
}
function snapshot(dir,files){
 fs.mkdirSync(dir,{recursive:true});const names=new Map();
 for(const file of files){const name=path.basename(file),hash=sha(file);if(names.has(name)){if(names.get(name)!==hash)throw Error('Ambiguous '+name);continue;}names.set(name,hash);fs.copyFileSync(file,dir+'/'+name,fs.constants.COPYFILE_EXCL);pins[file]=hash;}
}
function project(file,source,profile,semantic,mains){
 const switches=['-gnat2022','-gnatwa','-gnatwe',...(semantic?['-gnatc','-gnatwJ','-gnatw.X']:[]),...(profile==='checked'?['-gnata','-gnato','-O2']:['-gnatp','-O3'])];
 fs.writeFileSync(file,`project Check is
 for Source_Dirs use ("${source}");
 for Object_Dir use "${profile}/obj";
 for Exec_Dir use "${profile}/bin";
 ${mains.length?'for Main use ('+mains.map(n=>JSON.stringify(n+'.adb')).join(',')+');':''}
 package Compiler is
  for Default_Switches ("Ada") use (${switches.map(s=>JSON.stringify(s)).join(',')});
 end Compiler;
end Check;
`);
}
if(mode==='tests'){
 for(const [group,spec]of Object.entries(inventory.groups)){
  const dir=out+'/'+group;snapshot(dir+'/source',spec.sources.map(n=>root+'/'+n));
  for(const profile of ['checked','release']){
   project(dir+'/'+profile+'.gpr','source',profile,false,[...new Set(spec.mains.map(m=>m.name))]);
   run(group+'-'+profile+'-build',[tool,'-p','-j1','-P',dir+'/'+profile+'.gpr'],180);
   for(const [i,main]of spec.mains.entries()){
    const log=run(group+'-'+profile+'-'+i,[dir+'/'+profile+'/bin/'+main.name,...main.args],30);
    if(log.trim()!==main.label+' '+main.checks)throw Error('Unexpected test count '+log);
   }
  }
 }
}else{
 const files=Object.keys(inventory.production).map(n=>root+'/'+n);
 for(const dir of ['userspace/runtime/gnat','shared/capabilities','shared/time','shared/interrupts','shared/launch','shared/publications','shared/boot'])for(const entry of fs.readdirSync(root+'/'+dir,{withFileTypes:true}))if(entry.isFile()&&/\.ad[bs]$/.test(entry.name)&&(dir!=='userspace/runtime/gnat'||entry.name.startsWith('cubit')))files.push(root+'/'+dir+'/'+entry.name);
 files.push(here+'/native_request_fixture.ads',here+'/native_request_fixture.adb');snapshot(out+'/selected',files);
 for(const profile of ['checked','release']){
  project(out+'/'+profile+'.gpr','selected',profile,true,[]);
  run(profile+'-semantic',[tool,'-p','-j1','-c','-u','-P',out+'/'+profile+'.gpr','i2c_grant_copy.adb','i2c_native_grant_copy.adb','i2c_native_controller.adb','dw_i2c_requests.adb','i2c_native_requests.ads','i2c_native_loop.adb','i2c_transfer_wire.adb','i2c_native_results.adb','i2c_work_policy.adb','i2c_event_policy.adb','native_request_fixture.adb'],180);
  if(fs.readdirSync(out+'/'+profile+'/obj').some(n=>n.endsWith('.o')))throw Error('Native object generated');
 }
}
for(const [p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift during validation');
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({mode,commands:commands.length,checks_per_profile:mode==='tests'?Object.values(inventory.groups).reduce((n,g)=>n+g.mains.reduce((v,m)=>v+m.checks,0),0):null,semantic_units_per_profile:mode==='semantic'?11:null,semantic_objects:mode==='semantic'?0:null,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n',{flag:'wx'});
console.log('I2C native '+mode+' complete; pinned inputs verified');
