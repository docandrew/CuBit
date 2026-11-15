import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';

const suite=import.meta.dirname,root=path.resolve(suite,'../..');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const args=process.argv.slice(2),validate=args.length===1&&args[0]==='--validate-only';
let reuse=null;
if(args.length&&!validate){if(args.length!==2||args[0]!=='--reuse-build')throw Error('Usage: run.sh [--validate-only | --reuse-build ABS_RESULT]');reuse=args[1];if(!path.isAbsolute(reuse))throw Error('Absolute build result required');}
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));
const pins={[suite+'/sources.json']:hash(suite+'/sources.json')},listed=new Set(),selected=new Map();
function file(rel){
  if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('Relative path required');
  const p=root+'/'+rel;
  for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked input '+rel);
  if(!fs.statSync(p).isFile())throw Error('Regular input required');
  return p;
}
for(const row of catalog.inputs){
  if(listed.has(row.path))throw Error('Duplicate input');
  listed.add(row.path);const p=file(row.path);
  if(hash(p)!==row.sha256)throw Error('Input mismatch '+row.path);
  pins[p]=row.sha256;
}
function select(rel){
  if(!listed.has(rel))throw Error('Unlisted source '+rel);
  const name=path.basename(rel);
  if(selected.has(name)&&selected.get(name)!==rel)throw Error('Ambiguous Ada source '+name);
  selected.set(name,rel);
}
for(const dir of catalog.ada_directories)for(const n of fs.readdirSync(root+'/'+dir).filter(n=>/\.ad[bs]$/.test(n)))select(dir+'/'+n);
for(const rel of [...catalog.production_files,...catalog.selected_helpers])select(rel);
for(const n of fs.readdirSync(suite,{recursive:true})){
  const p=suite+'/'+n;
  if(fs.lstatSync(p).isSymbolicLink())throw Error('Linked helper');
  if(fs.statSync(p).isFile()&&n!=='sources.json'&&!listed.has(catalog.suite+'/'+n))throw Error('Unlisted helper '+n);
}
for(const t of Object.values(catalog.tools)){
  if(hash(t.path)!==t.sha256)throw Error('Tool drift');pins[t.path]=t.sha256;
}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,inputs:listed.size,selected:selected.size,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const compiler=spawnSync('which',['gcc'],{encoding:'utf8'});
if(compiler.status!==0||fs.realpathSync(compiler.stdout.trim())!==catalog.tools.gcc.path)throw Error('Compiler environment mismatch');
const cat=catalog;
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-sleep-preparation-'),runs=[],observations=[];let compiledBasis=null;
fs.mkdirSync(out+'/source');
for(const[n,rel]of selected){fs.copyFileSync(file(rel),out+'/source/'+n);pins[out+'/source/'+n]=hash(file(rel));}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,observations,compiledBasis,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:'New root PTS invocation driver with real AML/Core and no hardware providers. No native owner/wire activation, register writes or shutdown ordering validation.'},null,2)+'\n');}
function run(label,args,seconds,oracle=false,allowFailure=false){verify();const x=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit',...(oracle?['--as=134217728','--stack=2097152']:['--as=25769803776']),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(x.stdout??'')+(x.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);runs.push({label,status:x.status,error:x.error?.message??null,signal:x.signal});save(false);verify();if(!allowFailure&&(x.status!==0||x.error||x.signal))throw Error(label+'; '+out);return {log,ok:x.status===0&&!x.error&&!x.signal};}
const bodies={normal:'',integer:'Return(Zero)',package:'Return(Package(){One,"ignored"})',reference:'Return(RefOf(SEEN))',metadata:'Return(RefOf(DEV0))',failure:'Divide(One,ZDIV,Local0,Local1)',budget:'While(One){Noop}',absent:'','wrong-kind':'',alias:''};
const fixtures=[];console.log(out);
for(const[mode,body]of Object.entries(bodies))for(const width of[1,2]){
 const method='Method('+(mode==='alias'?'PXXX':'_PTS')+',1){Increment(CALL) Store(Arg0,SEEN) '+body+'}';
 const root=mode==='absent'?'':mode==='wrong-kind'?'Name(ZPTS,Zero)':method+(mode==='alias'?' Alias(PXXX,_PTS)':'');
 const nested=mode==='absent'?'Method(ZPTS,1){Increment(CALL) Store(Arg0,SEEN)}':'';
 const text='DefinitionBlock("","DSDT",'+width+',"CUBIT","PTSTEST",1){Name(CALL,Zero) Name(SEEN,Zero) Name(ZDIV,Zero) Device(DEV0){Name(_HID,"TEST0001") '+nested+'} '+root+'}\n';
 const prefix=out+'/'+mode+width;fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');
 run('iasl-'+mode+width,[cat.tools.iasl.path,'-oa','-p',prefix,prefix+'.asl'],5,true);
 if(mode==='wrong-kind'||mode==='absent'){
   const bytes=fs.readFileSync(prefix+'.aml'),needle=Buffer.from('ZPTS');let count=0;
   for(let i=0;i<=bytes.length-needle.length;i++)if(bytes.subarray(i,i+needle.length).equals(needle)){Buffer.from('_PTS').copy(bytes,i);count++;}
   if(count!==1)throw Error('Wrong-kind patch inventory');bytes[9]=0;bytes[9]=(-bytes.reduce((a,b)=>a+b,0))&255;fs.writeFileSync(prefix+'.aml',bytes);
 }
 pins[prefix+'.aml']=hash(prefix+'.aml');fixtures.push({mode,width,prefix});
}
const project=out+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Main use("preparation_tests.adb");for Object_Dir use"obj";for Exec_Dir use"bin";for Excluded_Source_Files use("acpi_service.ads");package Naming is for Spec("ACPI_Service") use"acpi_service_host.ads";end Naming;package Compiler is for Default_Switches("Ada") use("-O0","-gnatp","-gnat2022","-gnatwa","-gnatwe");end Compiler;end Test;\n');pins[project]=hash(project);
if(reuse){
 const previous=JSON.parse(fs.readFileSync(reuse));
 if(!previous.completed||!previous.runs.some(r=>r.label==='build'&&r.status===0&&!r.error&&!r.signal)||previous.runs.some(r=>r.status!==0||r.error||r.signal))throw Error('Build prerequisite');
 for(const[p,h]of Object.entries(previous.pins)){if(hash(p)!==h)throw Error('Build input drift');pins[p]=h;}
 for(const[n,rel]of selected)if(previous.pins[previous.out+'/source/'+n]!==hash(file(rel)))throw Error('Build source mismatch '+n);
 if(fs.readFileSync(previous.out+'/test.gpr','utf8')!==fs.readFileSync(project,'utf8'))throw Error('Build intent mismatch');
 for(const t of Object.values(cat.tools))if(previous.pins[t.path]!==t.sha256)throw Error('Build tool mismatch');
 const extras=fs.readdirSync(previous.out+'/source').filter(n=>/\.ad[bs]$/.test(n)&&!selected.has(n));
 for(const n of extras)if(fs.existsSync(previous.out+'/obj/'+n.replace(/\.ad[bs]$/,'.ali')))throw Error('Previous consumed extra source '+n);
 const binary=previous.out+'/bin/preparation_tests';pins[reuse]=hash(reuse);pins[binary]=hash(binary);
 fs.mkdirSync(out+'/bin');fs.copyFileSync(binary,out+'/bin/preparation_tests');fs.chmodSync(out+'/bin/preparation_tests',0o755);
 compiledBasis={result:reuse,sha256:hash(reuse),binarySha256:hash(binary),unusedExtraSources:extras,scope:'Reused successful build after exact selected-source, project intent, tool and all prior input checks; all current fixtures executed again.'};
}else run('build',[cat.tools.gprbuild.path,'-q','-p','-j1','-P',project],180);
pins[out+'/bin/preparation_tests']=hash(out+'/bin/preparation_tests');
for(const{mode,width,prefix}of fixtures){const r=run('core-'+mode+width,[out+'/bin/preparation_tests',prefix+'.aml',mode],30,false,true),m=/Preparation ([\w-]+) checks\s+(\d+)\n$/.exec(r.log);observations.push({mode,width,passed:r.ok&&m?.[1]===mode,checks:m?Number(m[2]):null});}
if(observations.length!==catalog.expected_executions||observations.reduce((n,r)=>n+(r.checks??0),0)!==catalog.expected_checks)throw Error('Coverage inventory');verify();save(observations.every(r=>r.passed));console.log(JSON.stringify({completed:observations.every(r=>r.passed),out,executions:observations.length,checks:observations.reduce((n,r)=>n+(r.checks??0),0),failed:observations.filter(r=>!r.passed).map(({mode,width})=>({mode,width}))}));

if(observations.some(r=>!r.passed))process.exitCode=1;
