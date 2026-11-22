import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname);
const root=path.resolve(here,'../..');
const hash=b=>crypto.createHash('sha256').update(b).digest('hex');
if(process.argv.length!==3||!path.isAbsolute(process.argv[2])||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Need absolute private output root and prescribed TMPDIR');
const pins=JSON.parse(fs.readFileSync(here+'/sources.json'));
const files={};
for(const [name,expected]of Object.entries(pins)) {
 const absolute=root+'/'+name;
 if(fs.realpathSync(absolute)!==absolute)throw Error('Source is redirected');
 const bytes=fs.readFileSync(absolute);
 if(hash(bytes)!==expected)throw Error('Source changed; revalidate inventory '+name);
 files[absolute]=expected;
}
for(const name of ['run.sh','run.mjs','suite.gpr','sources.json','configuration_tests.adb'])files[here+'/'+name]=hash(fs.readFileSync(here+'/'+name));
fs.mkdirSync(process.argv[2],{recursive:true});
const out=fs.mkdtempSync(process.argv[2]+'/dw-i2c-');
console.log(out);
for(const [file,expected]of Object.entries(files)) {
 const destination=out+'/'+path.basename(file);
 fs.copyFileSync(file,destination,fs.constants.COPYFILE_EXCL);
 if(hash(fs.readFileSync(destination))!==expected)throw Error('Copy mismatch');
}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(files,null,2)+'\n');
const tools={};
for(const name of ['gprbuild','gnatbind','gnatlink']) {
 const candidate=(process.env.PATH??'').split(':').map(p=>p+'/'+name).find(p=>fs.existsSync(p));
 if(!candidate)throw Error('Tool absent '+name);
 const real=fs.realpathSync(candidate);
 if(!real.startsWith('/nix/store/'))throw Error('Tool outside pinned Nix environment');
 tools[name]={path:real,sha256:hash(fs.readFileSync(real))};
}
fs.writeFileSync(out+'/tools.json',JSON.stringify(tools,null,2)+'\n');
const commands=[];
function run(name,args,seconds) {
 const fd=fs.openSync(out+'/'+name+'.log','wx');
 // timeout bounds the process tree; maxBuffer bounds captured compiler logs.
 const result=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+5)*1000});
 const log=(result.stdout??'')+(result.stderr??'');
 fs.writeSync(fd,log.slice(0,1048576));fs.closeSync(fd);
 commands.push({name,args,status:result.status,signal:result.signal,error:result.error?.message??null});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 if(result.status!==0||result.error||log.length>1048576)throw Error('Command failed '+name);
 return log;
}
const artifacts={};
for(const profile of ['checked','release']) {
 run(profile+'-build',[tools.gprbuild.path,'-p','-j1','-P','suite.gpr','-XPROFILE='+profile],180);
 for(const [main,count]of [['configuration_tests',4931]]) {
  const log=run(profile+'-'+main,[out+'/'+profile+'/bin/'+main],30);
  const expected='DesignWare configuration checks '+count;
  if(log.trim()!==expected)throw Error('Unexpected result '+main);
  artifacts[profile+'/bin/'+main]=hash(fs.readFileSync(out+'/'+profile+'/bin/'+main));
 }
 const alis=fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
 if(alis.length!==3)throw Error('Unexpected source inventory');
 for(const ali of alis) {
  const data=fs.readFileSync(out+'/'+profile+'/obj/'+ali,'utf8').split('\n');
  for(const flag of ['-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp','-O3'])])if(!data.includes('A '+flag))throw Error('Missing flag '+flag);
  if(profile==='release'&&data.includes('A -gnata'))throw Error('Wrong release profile');
  artifacts[profile+'/obj/'+ali]=hash(fs.readFileSync(out+'/'+profile+'/obj/'+ali));
 }
}
for(const [file,expected]of Object.entries(files))if(hash(fs.readFileSync(file))!==expected)throw Error('Input changed during run');
fs.writeFileSync(out+'/artifacts.json',JSON.stringify(artifacts,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({profiles:['checked','release'],configuration_checks:4931,commands:commands.length,native_validated:false,hardware_validated:false},null,2)+'\n');
console.log('Both configuration profiles passed: 4931 checks per profile; 4 terminal commands.');
