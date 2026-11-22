import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url)), root=path.resolve(here,'../..');
const validateOnly=process.argv.length===3&&process.argv[2]==='--validate-only';
if(process.argv.length!==2&&!validateOnly)throw Error('Usage: run.sh [--validate-only]');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const catalogPath=here+'/sources.json', catalog=JSON.parse(fs.readFileSync(catalogPath));
if(catalog.version!==1)throw Error('Source catalog version');
const pins={[catalogPath]:sha(catalogPath)}, files=[], names=new Set();
for(const e of catalog.inputs){
 const p=path.resolve(root,e.path);
 if(!p.startsWith(root+path.sep)||sha(p)!==e.sha256)throw Error('Input mismatch '+e.path);
 pins[p]=e.sha256;
 if(e.kind==='ada'){
  const n=path.basename(p);
  if(names.has(n)||!/\.ad[bs]$/.test(n))throw Error('Ambiguous Ada input '+n);
  names.add(n);files.push({path:p,name:n,sha256:e.sha256});
 }
}
if(validateOnly){process.stdout.write(JSON.stringify({status:'source-manifest-valid',ada_sources:files.length,functional_tests_run:false})+'\n');process.exit(0);}
if(!process.env.TMPDIR||!path.isAbsolute(process.env.TMPDIR))throw Error('Select absolute TMPDIR before Nix');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-native-power-'));
fs.mkdirSync(out+'/source');
for(const f of files)fs.copyFileSync(f.path,out+'/source/'+f.name);
const copied={};for(const f of files)copied[out+'/source/'+f.name]=f.sha256;
for(const variant of ["-normal","-absent","-invalid","-changed","-blocked","-source","-empty","-hardware","-fatal","-both","-source-fatal","-invalid-first","-invalid-last","-battery-fatal","-search-after-invalid"]){
 const source=here+'/fixtures/bridge'+variant+'.asl',dest=out+'/bridge'+variant+'.asl';
 fs.copyFileSync(source,dest);copied[dest]=sha(source);
}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
const find=name=>process.env.PATH.split(path.delimiter).map(d=>path.join(d,name)).find(fs.existsSync);
const compiler=fs.realpathSync(find('gprbuild'));
if(!compiler.startsWith('/nix/store/'))throw Error('Use pinned Nix compiler');
const proposed=process.env.ACPICA_BIN?path.join(process.env.ACPICA_BIN,'iasl'):(find('iasl')??catalog.iasl.path);
const iasl=fs.realpathSync(proposed);
if(sha(iasl)!==catalog.iasl.sha256)throw Error('Revalidate the ACPICA compiler version');
const tools={[compiler]:sha(compiler),[iasl]:sha(iasl)};
fs.writeFileSync(out+'/tools.json',JSON.stringify(tools,null,2)+'\n');
const commands=[],counts={},fixtures={};
function verify(){for(const[p,v]of Object.entries({...pins,...copied,...tools}))if(sha(p)!==v)throw Error('Input/tool changed '+p);}
function run(label,args,seconds){
 verify();
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...args],
  {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
 const log=(r.stdout??'')+(r.stderr??'');
 fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 verify();
 if(r.status!==0||r.error||r.signal||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+'; see '+out);
 return log;
}
process.stdout.write(out+'\n');
for(const revision of [1,2])for(const variant of ["-normal","-absent","-invalid","-changed","-blocked","-source","-empty","-hardware","-fatal","-both","-source-fatal","-invalid-first","-invalid-last","-battery-fatal","-search-after-invalid"]){
 const prefix=out+'/bridge'+variant+'-r'+revision;
 run('fixture'+variant+'-r'+revision,[iasl,'-oa','-r',String(revision),'-p',prefix,out+'/bridge'+variant+'.asl'],5);
 const bytes=fs.readFileSync(prefix+'.aml');
 if(bytes.length<36||bytes[8]!==revision||bytes.readUInt32LE(4)!==bytes.length||bytes.reduce((n,b)=>(n+b)&255,0)!==0)
  throw Error('Compiled fixture framing');
 fixtures[prefix+'.aml']=sha(prefix+'.aml');
}
fs.writeFileSync(out+'/fixtures.json',JSON.stringify(fixtures,null,2)+'\n');
for(const profile of ['release']){
 const flags=['-O0','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
 fs.writeFileSync(out+'/'+profile+'.gpr',`project ${profile} is
 for Source_Dirs use ("source");
 for Source_Files use (${files.map(f=>JSON.stringify(f.name)).join(',')});
 for Object_Dir use "${profile}/obj";
 for Exec_Dir use "${profile}/bin";
 for Main use ("native_power_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use (${flags.map(f=>JSON.stringify(f)).join(',')});
 end Compiler;
end ${profile};
`);
 run(profile+'-build',[compiler,'-p','-j1','-P',out+'/'+profile+'.gpr'],180);
 counts[profile]={};
 for(const revision of [1,2]){
  counts[profile][revision]={};
  for(const[mode,expected]of Object.entries(catalog.cases)){
   const fixtureMode=mode==='invalid-first-limit'?'invalid-first':mode==='blocked-limit'?'blocked':mode==='fadt-hardware'?'hardware':mode;
   const fixture=out+'/bridge'+(["normal","absent","invalid","changed","blocked","source","empty","hardware","fatal","both","source-fatal","invalid-first","invalid-last","battery-fatal","search-after-invalid"].includes(fixtureMode)?'-'+fixtureMode:'-normal')+'-r'+revision+'.aml';
   if(sha(fixture)!==fixtures[fixture])throw Error('Fixture changed');
   const log=run(profile+'-r'+revision+'-'+mode,[out+'/'+profile+'/bin/native_power_tests',fixture,mode],30);
   const m=/^Native power checks\s+([0-9]+)\s*$/.exec(log);
   if(!m||Number(m[1])<expected)throw Error('Unexpected test result '+mode);
   counts[profile][revision][mode]=Number(m[1]);
  }
 }
 for(const n of fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){
  const f=fs.readFileSync(out+'/'+profile+'/obj/'+n,'utf8').split('\n');
  if(!f.includes('A -gnatwe')||f.includes('A -gnatc')||
   (profile==='checked'?(!f.includes('A -gnata')||!f.includes('A -gnato')||f.includes('A -gnatp')):
   (!f.includes('A -gnatp')||f.includes('A -gnata'))))throw Error('Profile mismatch '+n);
 }
}

const oracleRuns=[];
const acpiexec=fs.realpathSync(path.join(path.dirname(iasl),'acpiexec'));tools[acpiexec]=sha(acpiexec);
for(const revision of[1,2])for(const[name,expected]of[['invalid-first',[2,1,5000]],['invalid-last',[1,2,5000]]]){
 const log=run('acpica-'+name+'-r'+revision,['prlimit','--as=134217728','--stack=2097152',acpiexec,'-l','-di','-dr','-b','evaluate B0UN;evaluate B1UN;evaluate B1ST',out+'/bridge-'+name+'-r'+revision+'.aml'],5);
 if(/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log))throw Error('ACPICA oracle failure');
 const observed=[...log.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>Number.parseInt(m[1],16));
 if(JSON.stringify(observed)!==JSON.stringify(expected))throw Error('ACPICA oracle disagreement '+name+' '+JSON.stringify(observed));
 oracleRuns.push({name,revision,observed});
}
fs.writeFileSync(out+'/ORACLES.json',JSON.stringify(oracleRuns,null,2)+'\n');
verify();
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands:commands.length,hosted:true,new_proof:false,
 native_executed:false,hardware_validated:false},null,2)+'\n');
process.stdout.write('Complete '+JSON.stringify(counts)+'\n');
