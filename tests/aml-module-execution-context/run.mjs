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
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'aml-active-definition-matrix-'));
fs.mkdirSync(out+'/source');
for(const f of files)fs.copyFileSync(f.path,out+'/source/'+f.name);
const copied={};for(const f of files)copied[out+'/source/'+f.name]=f.sha256;
for(const variant of ['-main','-normal','-fail','-fatal','-distinct','-retag-success','-retag-fail','-origin-ssdt','-origin-oem','-recover']){
 const source=root+'/tests/aml-ready-module-append/fixtures/bridge'+variant+'.asl',dest=out+'/bridge'+variant+'.asl';
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
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],
  {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
 const log=(r.stdout??'')+(r.stderr??'');
 fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');
 verify();
 if(r.status!==0||r.error||Buffer.byteLength(log)>1048576)throw Error('Failed '+label+'; see '+out);
 return log;
}
process.stdout.write(out+'\n');
for(const revision of [1,2])for(const variant of ['-main','-normal','-fail','-fatal','-distinct','-retag-success','-retag-fail','-origin-ssdt','-origin-oem','-recover']){
 const prefix=out+'/bridge'+variant+'-r'+revision;
 run('fixture'+variant+'-r'+revision,[iasl,'-oa','-r',String(revision),'-p',prefix,out+'/bridge'+variant+'.asl'],5);
 const bytes=fs.readFileSync(prefix+'.aml');
 if(bytes.length<36||bytes[8]!==revision||bytes.readUInt32LE(4)!==bytes.length||bytes.reduce((n,b)=>(n+b)&255,0)!==0)
  throw Error('Compiled fixture framing');
 fixtures[prefix+'.aml']=sha(prefix+'.aml');
}
fs.writeFileSync(out+'/fixtures.json',JSON.stringify(fixtures,null,2)+'\n');
for(const profile of ['checked','release']){
 const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
 fs.writeFileSync(out+'/'+profile+'.gpr',`project ${profile} is
 for Source_Dirs use ("source");
 for Source_Files use (${files.map(f=>JSON.stringify(f.name)).join(',')});
 for Object_Dir use "${profile}/obj";
 for Exec_Dir use "${profile}/bin";
 for Main use ("ready_module_append_tests.adb", "module_policy_tests.adb", "module_frames_tests.adb", "module_path_tests.adb", "module_path_operand_tests.adb", "parameter_store_tests.adb", "retained_table_load_tests.adb", "retained_request_tests.adb", "definition_origin_tests.adb");
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
   const fixture=out+'/bridge-'+(['fail','fatal','recover'].includes(mode)?mode:'normal')+'-r'+revision+'.aml';
   if(sha(fixture)!==fixtures[fixture])throw Error('Fixture changed');
   const log=run(profile+'-r'+revision+'-'+mode,[out+'/'+profile+'/bin/ready_module_append_tests',out+'/bridge-main-r'+revision+'.aml',fixture,mode],30);
   const m=/^Ready module append checks\s+([0-9]+)\s*$/.exec(log);
   if(!m||Number(m[1])<expected)throw Error('Unexpected test result '+mode);
   counts[profile][revision][mode]=Number(m[1]);
  }
 }
 for(const revision of [1,2]){
  const log=run(profile+'-module-path-r'+revision,[out+'/'+profile+'/bin/module_path_tests',out+'/bridge-main-r'+revision+'.aml',out+'/bridge-normal-r'+revision+'.aml'],30);
  const match=/^Module path checks\s+([0-9]+)\s*$/.exec(log);
  if(!match||Number(match[1])<90)throw Error('No module path checks');
  counts[profile]['module_path_r'+revision]=Number(match[1]);
 }
 for(const revision of [1,2]){
  const log=run(profile+'-module-operands-r'+revision,[out+'/'+profile+'/bin/module_path_operand_tests',out+'/bridge-main-r'+revision+'.aml',out+'/bridge-normal-r'+revision+'.aml'],30);
  const match=/^Module path operand checks\s+([0-9]+)\s*$/.exec(log);
  if(!match||Number(match[1])<100)throw Error('Missing module path operand checks');
  counts[profile]['module_operands_r'+revision]=Number(match[1]);
 }
 for(const revision of [1,2]){
  const log=run(profile+'-parameter-store-r'+revision,[out+'/'+profile+'/bin/parameter_store_tests',out+'/bridge-main-r'+revision+'.aml',out+'/bridge-normal-r'+revision+'.aml'],30);
  const match=/^Parameter Store checks\s+([0-9]+)\s*$/.exec(log);
  if(!match||Number(match[1])<100)throw Error('Missing parameter Store checks');
  counts[profile]['parameter_store_r'+revision]=Number(match[1]);
 }
 for(const revision of [1,2])for(const mode of ['normal','distinct','fail','fatal','recover']){
  const module=out+'/bridge-'+(['fail','fatal','recover'].includes(mode)?mode:'normal')+'-r'+revision+'.aml';
  const log=run(profile+'-retained-r'+revision+'-'+mode,[out+'/'+profile+'/bin/retained_table_load_tests',out+'/bridge-main-r'+revision+'.aml',module,mode,out+'/bridge-distinct-r'+revision+'.aml'],30);
  const match=/^Retained table load checks\s+([0-9]+)\s*$/.exec(log);
  if(!match||Number(match[1])<60)throw Error('Missing retained table checks');
  if(!counts[profile]['retained_r'+revision])counts[profile]['retained_r'+revision]={};
  counts[profile]['retained_r'+revision][mode]=Number(match[1]);
 }
 for(const revision of [1,2])for(const mode of ['normal','no-parameter','store-fail','retype-success','retype-fail','module-fail','fatal','budget']){
  const variant=mode==='module-fail'?'fail':mode==='fatal'?'fatal':mode==='retype-success'?'retag-success':mode==='retype-fail'?'retag-fail':'normal';
  const log=run(profile+'-request-r'+revision+'-'+mode,[out+'/'+profile+'/bin/retained_request_tests',out+'/bridge-main-r'+revision+'.aml',out+'/bridge-'+variant+'-r'+revision+'.aml',mode],30);
  const match=/^Retained request checks\s+([0-9]+)\s*$/.exec(log);
  if(!match||Number(match[1])<60)throw Error('Missing retained request checks');
  if(!counts[profile]['request_r'+revision])counts[profile]['request_r'+revision]={};
  counts[profile]['request_r'+revision][mode]=Number(match[1]);
 }
 for(const revision of [1,2]){
  const log=run(profile+'-definition-r'+revision,[out+'/'+profile+'/bin/definition_origin_tests',out+'/bridge-main-r'+revision+'.aml',out+'/bridge-origin-ssdt-r'+revision+'.aml',out+'/bridge-origin-oem-r'+revision+'.aml'],30);
  const m=/^Definition origin checks\s+([0-9]+)\s*$/.exec(log);
  if(!m||Number(m[1])<90)throw Error('Missing definition origin checks');
  counts[profile]['definition_r'+revision]=Number(m[1]);
 }
 const policy=run(profile+'-module-policy',[out+'/'+profile+'/bin/module_policy_tests'],30);
 const policyMatch=/^AML module policy checks:\s+([0-9]+)\s*$/.exec(policy);
 if(!policyMatch||Number(policyMatch[1])!==80)throw Error('No module policy checks');
 counts[profile].module_policy=Number(policyMatch[1]);
 const frameLog=run(profile+'-module-frames',[out+'/'+profile+'/bin/module_frames_tests'],30);
 const frameMatch=/^Module frame checks\s+([0-9]+)\s*$/.exec(frameLog);
 if(!frameMatch||Number(frameMatch[1])!==126)throw Error('No frame checks');
 counts[profile].module_frames=Number(frameMatch[1]);
 for(const n of fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){
  const f=fs.readFileSync(out+'/'+profile+'/obj/'+n,'utf8').split('\n');
  if(!f.includes('A -gnatwe')||f.includes('A -gnatc')||
   (profile==='checked'?(!f.includes('A -gnata')||!f.includes('A -gnato')||f.includes('A -gnatp')):
   (!f.includes('A -gnatp')||f.includes('A -gnata'))))throw Error('Profile mismatch '+n);
 }
}
if(JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('Profiles disagree');
verify();
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands:commands.length,hosted:true,new_proof:false,
 native_executed:false,hardware_validated:false},null,2)+'\n');
process.stdout.write('Complete '+JSON.stringify(counts)+'\n');
