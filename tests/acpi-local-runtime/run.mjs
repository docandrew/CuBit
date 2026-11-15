import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,root=path.resolve(d,'../..'),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const catalog=JSON.parse(fs.readFileSync(d+'/sources.json')),pins={[d+'/sources.json']:hash(d+'/sources.json')},runs=[];
const names=new Map();
for(const [base,rows]of [[root,catalog.inputs],[d,catalog.helpers]])for(const r of rows){
 if(path.isAbsolute(r.path)||r.path.split('/').includes('..'))throw Error('relative path');
 const p=base+'/'+r.path;if(!fs.lstatSync(p).isFile()||fs.lstatSync(p).isSymbolicLink()||hash(p)!==r.sha256)throw Error('source baseline '+p);
 pins[p]=r.sha256;if(/\.ad[bs]$/.test(p)){const name=path.basename(p);if(names.has(name))throw Error('duplicate source '+name);names.set(name,p);}
}
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-local-runtime-'),src=out+'/source';fs.mkdirSync(src);
for(const[n,p]of names){fs.copyFileSync(p,src+'/'+n);pins[src+'/'+n]=hash(src+'/'+n);}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift');}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,
 scope:'Actual hosted Core/AML with local runtime controller and modeled owner/clock callbacks. Native parent adapter requires separate SDK checks. No new proof/native/hardware claim.'},null,2)+'\n');}
function run(label,args,seconds){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
 const output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,args,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);
 if(r.status!==0||r.error||r.signal)throw Error(label+' failed '+out);verify();return output;}
const project=out+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Object_Dir use"obj";for Exec_Dir use"bin";for Main use("runtime_tests.adb");for Excluded_Source_Files use("acpi_target_heap.adb");package Naming is for Body("ACPI_Target_Heap") use"acpi_target_heap_host.adb";end Naming;package Compiler is for Default_Switches("Ada") use("-O0","-gnat2022","-gnatwa","-gnatwe","-gnatp");end Compiler;end Test;\n');pins[project]=hash(project);
run('build',['gprbuild','-q','-p','-j1','-P',project],180);
const cases=['normal','fresh-stop','claim-refused','stop-during-claim','enter-refused','begin-refused','end-refused','opaque-args','retained-end','stop-during-eval','budget-stop','io-pending','reentry'];
for(const width of [1,2]){const source=out+'/fixture'+width+'.asl',prefix=out+'/fixture'+width;fs.writeFileSync(source,fs.readFileSync(d+'/runtime.asl','utf8').replace('"DSDT", 2','"DSDT", '+width));pins[source]=hash(source);
 run('compile-'+width,['iasl','-p',prefix,source],5);pins[prefix+'.aml']=hash(prefix+'.aml');
 for(const mode of cases){const output=run('run-'+width+'-'+mode,[out+'/bin/runtime_tests',mode,prefix+'.aml'],30);
  if(!new RegExp('^LOCAL RUNTIME '+mode+' checks\\s+\\d+\\s*$').test(output.trim()))throw Error('case marker');}}
save(true);console.log('Hosted local runtime cases passed; '+out);
