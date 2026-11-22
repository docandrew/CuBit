import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const here=import.meta.dirname;let root=path.resolve(here,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++)if(process.argv[i]==='--validate-only')validate=true;else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else throw Error('Usage: run.sh [--validate-only] [--source-root DIR]');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),cp=here+'/sources.json',caseFile=here+'/cases.json',catalog=JSON.parse(fs.readFileSync(cp)),cases=JSON.parse(fs.readFileSync(caseFile));
if(catalog.version!==1||cases.length!==48||cases.reduce((n,c)=>n+c.checks,0)!==640)throw Error('Catalog');
const pins={[cp]:hash(cp),[caseFile]:hash(caseFile),[import.meta.filename]:hash(import.meta.filename)},selected=new Map();
for(const row of catalog.inputs){
 if(path.isAbsolute(row.path)||row.path.split('/').includes('..')||row.kind!=='ada')throw Error('Relative Ada source required');
 const p=root+'/'+row.path;for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked source');
 if(!fs.statSync(p).isFile()||hash(p)!==row.sha256)throw Error('Source drift '+row.path);
 const n=path.basename(p);if(selected.has(n))throw Error('Duplicate compilation source');selected.set(n,p);pins[p]=row.sha256;
}
for(const t of Object.values(catalog.tools)){if(hash(t.path)!==t.sha256)throw Error('Tool drift');pins[t.path]=t.sha256;}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,sources:selected.size,cases:48,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-module-delay-portable-'),runs=[],observations=[],oracles=[];fs.mkdirSync(out+'/source');for(const[n,p]of selected){fs.copyFileSync(p,out+'/source/'+n);pins[out+'/source/'+n]=hash(out+'/source/'+n);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,root,pins,runs,observations,oracles,scope:catalog.scope,newProof:false,nativeExecuted:false,hardwareValidated:false},null,2)+'\n');}
function run(label,args,seconds,oracle=false){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit',...(oracle?['--as=134217728','--stack=2097152']:['--as=25769803776']),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);runs.push({label,status:r.status,signal:r.signal,error:r.error?.message??null});save(false);if(r.status!==0||r.signal||r.error){console.log(text);throw Error(label+' '+out);}verify();return text;}
console.log(out);const project=out+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Main use("module_delay_tests.adb");for Object_Dir use"obj";for Exec_Dir use"bin";package Compiler is for Default_Switches("Ada") use("-O0","-gnatp","-gnat2022","-gnatwa");end Compiler;end Test;\n');pins[project]=hash(project);
run('build',[catalog.tools.gprbuild.path,'-q','-p','-j1','-P',project],180);
const alis=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));if(alis.length!==catalog.expected_non_binder_alis)throw Error('Compiled inventory '+alis.length);
for(const n of alis){const p=out+'/obj/'+n,lines=fs.readFileSync(p,'utf8').split('\n');for(const f of ['-O0','-gnatp','-gnat2022','-gnatwa'])if(!lines.includes('A '+f))throw Error('Compiler profile '+n);pins[p]=hash(p);}
const binary=out+'/bin/module_delay_tests';pins[binary]=hash(binary);
for(const c of cases){
 if(!/^[a-z]+$/.test(c.scenario)||![1,2].includes(c.width)||!['completed','unavailable','failed'].includes(c.provider))throw Error('Case shape');
 const label=c.scenario+'-'+c.width+'-'+c.provider,input=out+'/'+label+'.dsdt',text=run(label,[binary,c.scenario,String(c.width),c.provider,'true',input],30);
 if(text!=='Module delay '+c.scenario+' checks '+c.checks+'\n')throw Error('Exact verdict '+label);
 pins[input]=hash(input);observations.push({...c,input});
}
for(const c of observations.filter(c=>['stall','sleep','sequence','nested'].includes(c.scenario)&&c.provider==='completed')){
 const label='oracle-'+c.scenario+'-'+c.width,text=run(label,[catalog.tools.acpiexec.path,'-l','-di','-dr','-b','evaluate MARK',c.input],5,true);
 if(/Evaluation of .* failed|Aborting method|Could not resolve symbol|\bAE_[A-Z_]+/.test(text)||!text.includes('[Integer] = 0000000000000001'))throw Error('Module continuation mismatch');
 oracles.push({scenario:c.scenario,width:c.width,moduleContinuationMatched:true,cacheTrackingDiagnostic:/Outstanding cache allocations/.test(text)});
}
if(observations.length!==48||observations.reduce((n,c)=>n+c.checks,0)!==640||oracles.length!==8)throw Error('Coverage');verify();save(true);
console.log(JSON.stringify({completed:true,out,sources:selected.size,nonBinderALIs:alis.length,executions:48,checks:640,ACPICAContinuationChecks:8,newProof:false,nativeExecuted:false}));
