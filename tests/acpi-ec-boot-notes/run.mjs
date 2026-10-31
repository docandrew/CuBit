import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..'),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),catalog=JSON.parse(fs.readFileSync(here+'/sources.json'));
const pins={[here+'/sources.json']:hash(here+'/sources.json')},names=new Set();
if(catalog.version!==1||catalog.inputs.length!==168||Object.keys(catalog.expected).length!==12)throw Error('catalog scope');
for(const row of catalog.inputs){const p=path.resolve(root,row.path);if(!p.startsWith(root+'/')||fs.realpathSync(p)!==p||!fs.lstatSync(p).isFile()||hash(p)!==row.sha256||names.has(path.basename(p)))throw Error('source pin '+row.path);names.add(path.basename(p));pins[p]=row.sha256;}
for(const row of catalog.tests){const p=path.resolve(here,row.path);if(!p.startsWith(here+'/')||hash(p)!==row.sha256)throw Error('test pin');pins[p]=row.sha256;}
for(const name of ['run.sh','run.mjs'])pins[here+'/'+name]=hash(here+'/'+name);
if(process.argv.length===3&&process.argv[2]==='--validate-only'){console.log(JSON.stringify({source_rows:catalog.inputs.length,fixture_rows:catalog.tests.length,executed:false}));process.exit(0);}
if(process.argv.length!==2||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Usage: run.sh [--validate-only], with TMPDIR set before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-ec-boot-notes-');fs.mkdirSync(out+'/source');fs.mkdirSync(out+'/fixtures');console.log(out);
for(const row of catalog.inputs){const p=root+'/'+row.path,to=out+'/source/'+path.basename(row.path);fs.copyFileSync(p,to);pins[to]=row.sha256;}
for(const row of catalog.tests){const to=out+'/'+(row.path.endsWith('.asl')?row.path:'source/'+path.basename(row.path));if(fs.existsSync(to))throw Error('test/source collision');fs.copyFileSync(here+'/'+row.path,to);pins[to]=row.sha256;}
const tools={};for(const name of ['gprbuild','iasl']){const p=process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync);if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('cached Nix tool required');const real=fs.realpathSync(p);tools[name]=real;pins[real]=hash(real);}if(pins[tools.iasl]!==catalog.iasl_sha256)throw Error('ACPICA tool drift');
function verify(){for(const[p,sha]of Object.entries(pins))if(hash(p)!==sha)throw Error('input drift '+p);}
const commands=[],results=[];
function run(label,args,seconds){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});fs.writeFileSync(out+'/'+label+'.log',(r.stdout??'')+(r.stderr??''));commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error(label+' failed; '+out);return r.stdout;}
for(const name of ['base','hardware','reference'])run('iasl-'+name,[tools.iasl,'-p',out+'/fixtures/'+name,out+'/fixtures/'+name+'.asl'],5);
run('reference-disassemble',[tools.iasl,'-p',out+'/fixtures/reference-roundtrip','-d',out+'/fixtures/reference.aml'],5);
run('reference-recompile',[tools.iasl,'-p',out+'/fixtures/reference-recompiled',out+'/fixtures/reference-roundtrip.dsl'],5);
if(!fs.readFileSync(out+'/fixtures/reference.aml').equals(fs.readFileSync(out+'/fixtures/reference-recompiled.aml')))throw Error('ACPICA ECDT roundtrip');
for(const profile of ['checked','release']){
 const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
 fs.writeFileSync(out+'/'+profile+'.gpr',`project Host is\n for Source_Dirs use ("source");\n for Main use ("ec_notes_tests.adb");\n for Object_Dir use "${profile}-obj";\n for Exec_Dir use "${profile}-bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(x=>'"'+x+'"').join(', ')});\n end Compiler;\nend Host;\n`);
 run(profile+'-build',[tools.gprbuild,'-p','-j1','-P',out+'/'+profile+'.gpr'],180);
 for(const [mode,wanted] of Object.entries(catalog.expected)){
  const stdout=run(profile+'-'+mode,[out+'/'+profile+'-bin/ec_notes_tests',out+'/fixtures/'+(mode==='hardware'?'hardware':'base')+'.aml',mode,out+'/fixtures/reference.aml'],30),m=/EC boot observation checks\s+(\d+)/.exec(stdout);
  if(!m||Number(m[1])!==wanted)throw Error('case count '+mode);results.push({profile,mode,checks:Number(m[1]),stdout});
 }
 for(const name of fs.readdirSync(out+'/'+profile+'-obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){const lines=fs.readFileSync(out+'/'+profile+'-obj/'+name,'utf8').split('\n');for(const flag of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('ALI switches');}
}
for(const row of results.filter(x=>x.profile==='checked'))if(row.stdout!==results.find(x=>x.profile==='release'&&x.mode===row.mode).stdout)throw Error('profile mismatch');
verify();fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');const checks=results.reduce((s,r)=>s+r.checks,0);if(checks!==376)throw Error('total checks');fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({checks,runtime_processes:results.length,results,commands:commands.length,ACPICA_ECDT_roundtrip_bytes:true,new_proof:false,native_executed:false,hardware_validated:false,scope:'Actual catalog/observer/formatter/native logging hook with explicit hosted logger; not native runtime/logstore delivery or hardware authority'},null,2)+'\n');console.log('EC boot notes '+checks+' checks passed');
