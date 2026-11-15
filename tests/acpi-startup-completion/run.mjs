import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const suite=import.meta.dirname,root=path.resolve(suite,'../..');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const args=process.argv.slice(2),validate=args.length===1&&args[0]==='--validate-only';
if(args.length&&!validate)throw Error('Usage: run.sh [--validate-only]');
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json')),pins={[suite+'/sources.json']:hash(suite+'/sources.json')};
function input(rel){if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('Relative input required');const p=root+'/'+rel;for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked input');if(!fs.statSync(p).isFile())throw Error('Regular input required');return p;}
if(catalog.version!==1)throw Error('Catalog version');
const listed=new Set();for(const row of catalog.inputs){if(listed.has(row.path))throw Error('Duplicate input');listed.add(row.path);const p=input(row.path);if(hash(p)!==row.sha256)throw Error('Input mismatch '+row.path);pins[p]=row.sha256;}
for(const dir of catalog.ada_directories)for(const n of fs.readdirSync(root+'/'+dir).filter(n=>/\.ad[bs]$/.test(n)))if(!listed.has(dir+'/'+n))throw Error('Unlisted Ada input '+dir+'/'+n);
for(const name of ['acpi_native_power_detection.ads'])if(hash(root+'/tests/acpi-native-startup/boundary/'+name)!==hash(root+'/userspace/services/acpi/native/'+name))throw Error('Real report mirror mismatch');
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,inputs:catalog.inputs.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-startup-completion-'),source=out+'/source',runs=[],checks={};
for(const row of catalog.inputs){const d=source+'/'+row.path;fs.mkdirSync(path.dirname(d),{recursive:true});fs.copyFileSync(root+'/'+row.path,d);pins[d]=hash(d);}
function run(label,argv,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...argv],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});fs.writeFileSync(out+'/'+label+'.log',(r.stdout??'')+(r.stderr??''));runs.push({label,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/RUNS.json',JSON.stringify(runs,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error(label+' failed; '+out);return r.stdout;}
function put(from,to){fs.mkdirSync(path.dirname(to),{recursive:true});fs.copyFileSync(source+'/'+from,to);pins[to]=hash(to);}
const native='userspace/services/acpi/native/',canonical='userspace/runtime/gnat/cubit-consumer_initialization_exchange.ads',fixtures='tests/acpi-startup-completion/fixtures/';
for(const kind of ['policy','client','reports']){
 const extra=out+'/'+kind+'/extra';fs.mkdirSync(extra,{recursive:true});
 for(const unit of ['acpi_startup_completion.ads','acpi_startup_completion.adb'])put(native+unit,extra+'/'+unit);
 put(canonical,extra+'/cubit-consumer_initialization_exchange.ads');
 if(kind==='policy'){put(fixtures+'policy/startup_tests.adb',extra+'/startup_tests.adb');put(fixtures+'client/cubit.ads',extra+'/cubit.ads');}
 if(kind==='client'){for(const unit of ['acpi_native_consumer_completion.ads','acpi_native_consumer_completion.adb'])put(native+unit,extra+'/'+unit);for(const n of fs.readdirSync(source+'/'+fixtures+'client'))put(fixtures+'client/'+n,extra+'/'+n);}
 if(kind==='reports'){
  // Compile the exact production report specs and copied-path dependency.
  // The older lifecycle HID mirror is outside this report adapter test.
  for(const unit of ['acpi_native_hid_detection.ads','acpi_native_power_detection.ads','acpi_native_initializer_history.ads','acpi_native_initializer_history.adb'])put(native+unit,extra+'/'+unit);
  for(const unit of ['acpi_native_startup_completion.ads','acpi_native_startup_completion.adb'])put(native+unit,extra+'/'+unit);
  put(fixtures+'reports/report_tests.adb',extra+'/report_tests.adb');
  put(fixtures+'reports/acpi_one_service.ads',extra+'/acpi_one_service.ads');
  // Uncalled fail-closed owners, while using the real mirrored report specs.
  for(const n of ['acpi_native_hid_detection.adb','acpi_native_power_detection.adb']){const d=source+'/tests/acpi-native-startup/models/'+n;put(fixtures+'reports/'+n,d);}
 }
 for(const profile of kind==='reports'?['release']:['checked','release']){
  const dir=out+'/'+kind+'/'+profile,main=kind==='policy'?'startup_tests':kind==='client'?'completion_tests':'report_tests';fs.mkdirSync(dir);
  const flags=[kind==='reports'?'-O0':'-O1','-gnatA','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])],dirs=[extra,...(kind==='reports'?catalog.report_directories.map(d=>source+'/'+d):[])];
  const project=dir+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use('+dirs.map(JSON.stringify).join(',')+');for Main use("'+main+'.adb");for Object_Dir use"obj";for Exec_Dir use"bin";'+(kind==='reports'?'for Excluded_Source_Files use("acpi_target_heap.adb");package Naming is for Body("ACPI_Target_Heap") use"acpi_target_heap_host.adb";end Naming;':'')+'package Compiler is for Default_Switches("Ada") use('+flags.map(JSON.stringify).join(',')+');end Compiler;end Test;');pins[project]=hash(project);
  run(kind+'-'+profile+'-build',['gprbuild','-q','-p','-j1','-P',project],180);
  const required=kind==='policy'?['acpi_startup_completion']:kind==='client'?['acpi_startup_completion','acpi_native_consumer_completion']:['acpi_startup_completion','acpi_native_startup_completion'];
  for(const n of required){const ali=fs.readFileSync(dir+'/obj/'+n+'.ali','utf8').split('\n');for(const f of flags.filter(f=>f.startsWith('-gnat')))if(!ali.includes('A '+f))throw Error('Profile mismatch '+n);}
  let count=0;for(let mode=1;mode<=(kind==='client'?58:1);mode++){const text=run(kind+'-'+profile+'-'+mode,[dir+'/bin/'+main,...(kind==='client'?[String(mode)]:[])],30),match=new RegExp('^'+(kind==='policy'?'Startup completion':kind==='client'?'Consumer completion':'Startup report')+' checks (\\d+)\\n$').exec(text);if(!match)throw Error('Verdict');count+=Number(match[1]);}
  if(kind!=='client'&&count!==(kind==='policy'?1124:2918))throw Error('Check count');
  if(kind==='client'&&profile==='release'&&count!==checks['client-checked'])throw Error('Profile coverage differs');checks[kind+'-'+profile]=count;
 }
}
verify();fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:true,pins,runs,checks,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:'Actual pure phase policy and completion client checked/release; actual report adapter with hosted Core -O0 -gnatp. Discovery bodies fail closed and are never invoked. No whole Native_Instance execution.'},null,2)+'\n');console.log(JSON.stringify({completed:true,out,checks}));
