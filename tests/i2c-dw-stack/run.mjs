import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..');
const mode=process.argv[2],outputRoot=process.argv[3];
if(!['tests','proof'].includes(mode)||!path.isAbsolute(outputRoot??'')||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Need mode, absolute private output and pinned environment');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const inventory=JSON.parse(fs.readFileSync(here+'/sources.json'));
const pins={};
for(const [name,expected]of Object.entries(inventory.production)) {
 const file=root+'/'+name;if(fs.realpathSync(file)!==file||sha(file)!==expected)throw Error('Production input changed');pins[file]=expected;
}
for(const name of ['run.mjs','run.sh','prove.sh','suite.gpr','proof.gpr','sources.json',...inventory.mains.map(m=>m.name+'.adb')])pins[here+'/'+name]=sha(here+'/'+name);
fs.mkdirSync(outputRoot,{recursive:true});const out=fs.mkdtempSync(outputRoot+'/i2c-stack-'+mode+'-');console.log(out);
for(const [file,expected]of Object.entries(pins)) {
 const destination=out+'/'+path.basename(file);fs.copyFileSync(file,destination,fs.constants.COPYFILE_EXCL);
 if(sha(destination)!==expected)throw Error('Snapshot mismatch');
}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
const tools={};
for(const name of mode==='proof'?['gnatprove']:['gprbuild','gnatbind','gnatlink']) {
 const file=fs.realpathSync(process.env.PATH.split(':').map(p=>p+'/'+name).find(p=>fs.existsSync(p)));
 if(!file.startsWith('/nix/store/'))throw Error('Unpinned tool');tools[name]={path:file,sha256:sha(file)};
}
fs.writeFileSync(out+'/tools.json',JSON.stringify(tools,null,2)+'\n');
const commands=[],artifacts={};
function run(label,args,seconds) {
 const result=spawnSync('nice',['-n','19','timeout','--kill-after=5s',String(seconds),...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+6)*1000});
 const log=(result.stdout??'')+(result.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576),{flag:'wx'});
 commands.push({label,args,status:result.status,signal:result.signal,error:result.error?.message??null});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 if(result.status!==0||result.error||log.length>1048576)throw Error('Failed '+label);
 return log;
}
if(mode==='proof') {
 run('prove',[tools.gnatprove.path,'-P','proof.gpr','--mode=prove','--level=2','--timeout=5','--checks-as-errors=on','-j1'],180);
 const report=out+'/proof/obj/gnatprove/gnatprove.out';
 if(!fs.existsSync(report))throw Error('Missing proof report');artifacts['proof/obj/gnatprove/gnatprove.out']=sha(report);
} else {
 for(const profile of ['checked','release']) {
  run(profile+'-build',[tools.gprbuild.path,'-p','-j1','-P','suite.gpr','-XPROFILE='+profile],180);
  for(const main of inventory.mains) {
   const log=run(profile+'-'+main.name,[out+'/'+profile+'/bin/'+main.name],30);
   if(log.trim()!==main.label+' '+main.checks)throw Error('Test count mismatch');artifacts[profile+'/bin/'+main.name]=sha(out+'/'+profile+'/bin/'+main.name);
  }
  const alis=fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
  const expected=['dw_i2c_transfer','dw_i2c_registers','dw_i2c_configuration',
   'intel_lpss_i2c_profile','intel_lpss_i2c_reset','dw_i2c_owner',
   ...inventory.mains.map(m=>m.name)].map(n=>n+'.ali').sort();
  if(JSON.stringify([...alis].sort())!==JSON.stringify(expected))throw Error('Unexpected unit closure');
  for(const ali of alis) {
   const file=out+'/'+profile+'/obj/'+ali,flags=fs.readFileSync(file,'utf8').split('\n');
   for(const flag of ['-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp','-O3'])])if(!flags.includes('A '+flag))throw Error('Missing compiler profile');
   if(profile==='release'&&flags.includes('A -gnata'))throw Error('Wrong release profile');artifacts[profile+'/obj/'+ali]=sha(file);
  }
 }
}
for(const [file,expected]of Object.entries(pins))if(sha(file)!==expected)throw Error('Input changed during run');
fs.writeFileSync(out+'/artifacts.json',JSON.stringify(artifacts,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({mode,commands:commands.length,checks_per_profile:mode==='tests'?inventory.mains.reduce((n,m)=>n+m.checks,0):null,proof_scope:mode==='proof'?['DW_I2C_Transfer','Intel_LPSS_I2C_Profile']:[],native_validated:false,hardware_validated:false},null,2)+'\n',{flag:'wx'});
console.log('I2C stack '+mode+' completed; exact inputs and artifacts verified');
