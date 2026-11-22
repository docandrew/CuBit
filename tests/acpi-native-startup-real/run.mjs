import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const here=import.meta.dirname;let root=path.resolve(here,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++)if(process.argv[i]==='--validate-only')validate=true;else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else throw Error('Usage: run.sh [--validate-only] [--source-root DIR]');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
function file(rel){if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('Relative source required');const p=root+'/'+rel;for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked source');if(!fs.statSync(p).isFile())throw Error('Regular source required');return p;}
const catalogPath=here+'/sources.json',casesPath=here+'/cases.json',catalog=JSON.parse(fs.readFileSync(catalogPath)),cases=JSON.parse(fs.readFileSync(casesPath));
if(catalog.version!==1||cases.filter(c=>!c.supplemental).length!==88||cases.filter(c=>c.supplemental).length!==2||cases.reduce((n,c)=>n+c.checks,0)!==1726)throw Error('Case inventory');
const pins={[catalogPath]:hash(catalogPath),[casesPath]:hash(casesPath),[import.meta.filename]:hash(import.meta.filename)},selected=new Map();
for(const row of catalog.inputs){const p=file(row.path);if(hash(p)!==row.sha256)throw Error('Source drift '+row.path);pins[p]=row.sha256;if(row.kind==='ada'){const n=path.basename(p);if(selected.has(n))throw Error('Duplicate source '+n);selected.set(n,p);}else if(row.kind!=='metadata')throw Error('Input kind');}
for(const t of Object.values(catalog.tools)){if(hash(t.path)!==t.sha256)throw Error('Tool drift');pins[t.path]=t.sha256;}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,sources:selected.size,cases:cases.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-startup-portable-'),runs=[],observations=[],oracles=[];fs.mkdirSync(out+'/source');
for(const[n,p]of selected){fs.copyFileSync(p,out+'/source/'+n);pins[out+'/source/'+n]=hash(out+'/source/'+n);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,root,pins,runs,observations,oracles,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:catalog.scope},null,2)+'\n');}
function run(label,args,seconds,oracle=false){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit',...(oracle?['--as=134217728','--stack=2097152']:['--as=25769803776']),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);runs.push({label,status:r.status,signal:r.signal,error:r.error?.message??null});save(false);if(r.status!==0||r.signal||r.error){console.log(text);throw Error(label+' '+out);}verify();return text;}
console.log(out);
const mains=[...new Set(['owner_tests',...cases.map(c=>c.main)])];
for(const m of mains)if(!/^[a-z_]+$/.test(m)||!selected.has(m+'.adb'))throw Error('Main selection');
const project=out+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Main use('+mains.map(n=>'"'+n+'.adb"').join(',')+');for Object_Dir use"obj";for Exec_Dir use"bin";package Naming is for Spec("ACPI_Service") use"acpi_service_host.ads";for Body("ACPI_Target_Heap") use"acpi_target_heap_host.adb";end Naming;package Compiler is for Default_Switches("Ada") use("-O0","-gnatp","-gnat2022","-gnatwa");end Compiler;end Test;\n');pins[project]=hash(project);
for(const unit of ['acpi_native_instance.adb','owner_tests.adb',null])run('compile-'+(unit??'closure'),[catalog.tools.gprbuild.path,'-m','-q','-p','-j1','-P',project,'-c',...(unit?['-u',unit]:[])],180);
for(const main of mains)run('link-'+main,[catalog.tools.gprbuild.path,'-m','-q','-p','-j1','-P',project,main+'.adb'],180);
const alis=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
if(alis.length!==catalog.expected_non_binder_alis)throw Error('Compiled ALI inventory '+alis.length);
for(const n of alis){const p=out+'/obj/'+n,lines=fs.readFileSync(p,'utf8').split('\n');for(const flag of ['-O0','-gnatp','-gnat2022','-gnatwa'])if(!lines.includes('A '+flag))throw Error('Compiler profile '+n);pins[p]=hash(p);}
for(const main of mains)pins[out+'/bin/'+main]=hash(out+'/bin/'+main);
for(const c of cases){
 if(!['32','64'].includes(String(c.width))||!Number.isInteger(c.checks)||c.checks<1)throw Error('Case shape');
 const label=c.supplemental?'stall-'+c.width:c.scenario+'-'+c.width+'-'+c.broken,prefix=out+'/'+label;
 const args=c.supplemental?[String(c.width)]:[c.scenario,String(c.width),String(c.broken),...(c.exports?[prefix]:[])];
 const text=run(label,[out+'/bin/'+c.main,...args],30);
 if(text!==c.verdict+' '+c.checks+'\n')throw Error('Exact verdict '+label+' '+text);
 if(c.exports)for(const suffix of ['dsdt','fadt',...(c.group==='fatal'?['fatal']:c.group==='error'?['observations']:[])])pins[prefix+'.'+suffix]=hash(prefix+'.'+suffix);
 observations.push({...c,label});
}
const methods={module_fatal:'evaluate \\_REV',root_fatal:'execute \\_INI',bus_fatal:'execute \\_SB._INI',device_fatal:'execute \\DEV0._INI'};
for(const[scenario,batch]of Object.entries(methods))for(const width of[32,64]){
 const prefix=out+'/'+scenario+'-'+width+'-false',core=fs.readFileSync(prefix+'.fatal','utf8');
 const cm=/^acpi fatal origin=(module table|initializer scope)=([0-9A-F]{16}) type=([0-9A-F]{2}) code=([0-9A-F]{8}) arg=([0-9A-F]{16}) width=(32|64)\n$/.exec(core);
 if(!cm||Number(cm[6])!==width)throw Error('Copied Fatal record');
 const text=run('oracle-'+scenario+'-'+width,[catalog.tools.acpiexec.path,'-l','-di','-dr','-b',batch,prefix+'.dsdt'],5,true);
 const signals=[...text.matchAll(/Fatal ACPI BIOS error \(Type 0x([0-9A-Fa-f]+) Code 0x([0-9A-Fa-f]+) Arg 0x([0-9A-Fa-f]+)\)/g)];
 if(signals.length!==1||BigInt('0x'+cm[3])!==BigInt('0x'+signals[0][1])||BigInt('0x'+cm[4])!==BigInt('0x'+signals[0][2])||(BigInt('0x'+cm[5])&0xFFFFFFFFn)!==BigInt('0x'+signals[0][3]))throw Error('Fatal payload comparison');
 const method=batch.replace(/^execute /,'');
 if(scenario!=='module_fatal'&&!text.includes('Evaluation of '+method+' failed with status AE_ERROR'))throw Error('Initializer abort');
 if(scenario==='module_fatal'&&(cm[1]!=='module table'||!/^0*1$/.test(cm[2])||!text.includes('Evaluation of \\_REV returned object')))throw Error('Module origin');
 if(scenario!=='module_fatal'&&cm[1]!=='initializer scope')throw Error('Initializer origin');
 if([...text.matchAll(/\bAE_[A-Z_]+\b/g)].some(m=>m[0]!=='AE_ERROR'))throw Error('Unexpected ACPICA status');
 const raw=fs.readFileSync(prefix+'.dsdt');if(raw[8]!==({32:1,64:2})[width]||raw.reduce((n,b)=>(n+b)&255,0)!==0)throw Error('DSDT revision/checksum');
 if(hash(prefix+'.dsdt')!==hash(out+'/'+scenario+'-'+width+'-true.dsdt')||core!==fs.readFileSync(out+'/'+scenario+'-'+width+'-true.fatal','utf8'))throw Error('Logger mode changed AML/Fatal');
 oracles.push({scenario,width,payloadMatched:true,initializerAbortChecked:scenario!=='module_fatal',argumentProjection:'low32',cacheTrackingDiagnostic:/Outstanding cache allocations/.test(text)});
}
if(observations.length!==90||observations.reduce((n,c)=>n+c.checks,0)!==1726||oracles.length!==8)throw Error('Coverage');
verify();save(true);console.log(JSON.stringify({completed:true,out,originalCases:88,supplementalCases:2,checks:1726,nonBinderALIs:alis.length,ACPICAFatalComparisons:oracles.length,newProof:false,nativeExecuted:false}));
