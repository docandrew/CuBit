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
function admit(file,hash){
  if(sha(file)!==hash)throw Error('Input baseline changed: '+file);
  const name=path.basename(file);
  if(!/\.ad[bs]$/.test(name)||files.has(name))throw Error('Invalid/duplicate source '+name);
  files.set(name,file);pins[file]=hash;
}
for(const entry of catalog.inputs){
  const file=path.resolve(root,entry.path);
  if(!file.startsWith(root+path.sep))throw Error('Outside source root');
  admit(file,entry.sha256);
}
for(const entry of catalog.local){
  if(path.basename(entry.name)!==entry.name)throw Error('Local source name');
  admit(here+'/'+entry.name,entry.sha256);
}
function verify(){for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift: '+f);}
verify();
if(validateOnly){console.log(JSON.stringify({mode:'validate-only',inputs:catalog.inputs.length,local:catalog.local.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set TMPDIR before Nix');
const compiler=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gprbuild').find(fs.existsSync));
if(!compiler.startsWith('/nix/store/'))throw Error('Use the Nix development environment');
pins[compiler]=sha(compiler);
const out=fs.mkdtempSync(process.env.TMPDIR+'/gpio-hid-input-'),commands=[],counts={};
console.log(out);fs.mkdirSync(out+'/source');
for(const[name,file]of files){fs.copyFileSync(file,out+'/source/'+name);pins[out+'/source/'+name]=sha(file);}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
function run(label,args,seconds){
  verify();
  const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],
    {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
  const log=(r.stdout??'')+(r.stderr??'');
  fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
  commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
  fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
  if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+'; '+out);
  return log;
}
for(const profile of ['checked','release']){
  const flags=['-gnat2022','-gnatwa','-gnatwe','-gnatwJ',...(profile==='checked'?['-gnata','-gnato','-O2']:['-gnatp','-O3'])];
  fs.writeFileSync(out+'/'+profile+'.gpr',`project ${profile} is\n for Source_Dirs use ("source");\n for Main use ("gpio_hid_input_tests.adb");\n for Object_Dir use "${profile}/obj";\n for Exec_Dir use "${profile}/bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(f=>JSON.stringify(f)).join(',')});\n end Compiler;\nend ${profile};\n`);
  run(profile+'-build',[compiler,'-p','-j1','-P',out+'/'+profile+'.gpr'],180);counts[profile]={};
  for(const scenario of ['ordered','early-event']){
    const log=run(profile+'-'+scenario,[out+'/'+profile+'/bin/gpio_hid_input_tests',...(scenario==='ordered'?[]:[scenario])],30);
    const match=/^GPIO HID input interop checks\s+(\d+)\s*$/.exec(log);
    if(!match)throw Error('Unexpected test output');counts[profile][scenario]=Number(match[1]);
  }
  for(const name of fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){
    const lines=fs.readFileSync(out+'/'+profile+'/obj/'+name,'utf8').split('\n');
    for(const flag of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('Wrong ALI profile '+name);
  }
}
if(JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('Profile mismatch');verify();
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands:commands.length,sources:files.size,actual_gpio_hid_device_and_input:true,transport_MMIO_I2C_grants_modeled:true,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log('Complete '+JSON.stringify(counts));
