import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';

const suite=import.meta.dirname,root=path.resolve(suite,'../..');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const args=process.argv.slice(2),validate=args.length===1&&args[0]==='--validate-only';
if(args.length&&!validate)throw Error('Usage: run.sh [--validate-only]');
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
for(const rel of catalog.selected_helpers)select(rel);
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
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-root-port-regression-'),runs=[],observations=[];
fs.mkdirSync(out+'/source');
for(const[n,rel]of selected){fs.copyFileSync(file(rel),out+'/source/'+n);pins[out+'/source/'+n]=hash(file(rel));}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,observations,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:catalog.scope},null,2)+'\n');}
function run(label,argv,seconds,oracle=false){
  verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit',...(oracle?['--as=134217728','--stack=2097152']:['--as=25769803776']),...argv],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
  const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);
  runs.push({label,status:r.status,signal:r.signal,error:r.error?.message??null});save(false);verify();
  if(r.status!==0||r.signal||r.error)throw Error(label+'; '+out);
  return log;
}
const template=fs.readFileSync(suite+'/root-port.asl','utf8');
function replaceExactly(text,from,to,count){
  if(text.split(from).length-1!==count)throw Error('Fixture substitution inventory '+from);
  return text.replaceAll(from,to);
}
const variants=[
  {id:'enabled',vm:true,rd:1},
  {id:'vm-disabled',vm:false,rd:1},
  {id:'pci-absent',vm:true,rd:0},
  {id:'alternate-helper',vm:false,rd:1},
  {id:'helpers-false',vm:false,rd:0},
  {id:'zero-sleep-policy',vm:true,rd:0}
];
console.log(out);
const fixtures=[];
// Validate all ASL before spending time compiling Core.
for(const v of variants)for(const width of[1,2]){
  let text=replaceExactly(template,'"DSDT",2','"DSDT",'+width,1);
  if(!v.vm)text=replaceExactly(text,'Name(VMDE,One)','Name(VMDE,Zero)',1);
  if(v.id==='pci-absent')text=replaceExactly(text,'Name(VDID,0x80868086)','Name(VDID,0xFFFFFFFF)',2);
  if(['alternate-helper','helpers-false'].includes(v.id))text=replaceExactly(text,'Method(PNVM,0){Return(One)}','Method(PNVM,0){Return(Zero)}',2);
  if(v.id==='alternate-helper')text=replaceExactly(text,'Method(PAHC,0){Return(Zero)}','Method(PAHC,0){Return(One)}',2);
  if(v.id==='zero-sleep-policy')text=replaceExactly(text,'Name(STD3,One)','Name(STD3,Zero)',1);
  const prefix=out+'/'+v.id+width;
  fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');
  run('iasl-'+v.id+width,[catalog.tools.iasl.path,'-oa','-p',prefix,prefix+'.asl'],5,true);
  pins[prefix+'.aml']=hash(prefix+'.aml');fixtures.push({v,width,prefix});
}
const project=out+'/test.gpr';
fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Main use("root_port_model_tests.adb");for Object_Dir use"obj";for Exec_Dir use"bin";for Excluded_Source_Files use("acpi_service.ads");package Naming is for Spec("ACPI_Service") use"acpi_service_host.ads";end Naming;package Compiler is for Default_Switches("Ada") use("-O0","-gnatp","-gnat2022","-gnatwa","-gnatwe");end Compiler;end Test;\n');
pins[project]=hash(project);run('build',[catalog.tools.gprbuild.path,'-q','-p','-j1','-P',project],180);
pins[out+'/bin/root_port_model_tests']=hash(out+'/bin/root_port_model_tests');
for(const{v,width,prefix}of fixtures){
  const label=v.id+width;
  const core=run('core-'+label,[out+'/bin/root_port_model_tests',prefix+'.aml'],30);
  const rows=[...core.matchAll(/^(B00[0-9]) (\w+)(?:\s+(\d+))?$/gm)].map(m=>({name:m[1],status:m[2],value:m[3]??null}));
  const log=run('acpica-'+label,[catalog.tools.acpiexec.path,'-l','-di','-dr','-b',Array.from({length:10},(_,i)=>'execute B00'+i).join(';'),prefix+'.aml'],5,true);
  const values=[...log.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
  const ones=((1n<<BigInt(width===1?32:64))-1n).toString();
  const expected=[v.rd,v.rd,ones,0,6,8,v.vm?1:0,7,ones,0].map(String);
  if(rows.length!==10||values.length!==10||/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log))throw Error('Result inventory '+label);
  const coreAgrees=rows.every((r,i)=>r.name==='B00'+i&&r.status==='RETURNED'&&r.value===expected[i]);
  const acpicaAgrees=values.every((n,i)=>n===expected[i]);
  observations.push({variant:v.id,width,rows,acpica:values,expected,coreAgrees,acpicaAgrees});save(false);
  if(!coreAgrees||!acpicaAgrees)throw Error('Expected result disagreement '+label+'; '+out);
}
if(observations.length!==catalog.expected_oracles||observations.reduce((n,r)=>n+r.rows.length,0)!==catalog.expected_cases)throw Error('Coverage inventory');
verify();save(true);
console.log(JSON.stringify({completed:true,out,cases:catalog.expected_cases,ACPICARuns:observations.length,newProof:false,nativeExecuted:false,hardwareValidated:false}));
