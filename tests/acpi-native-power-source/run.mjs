import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const here=import.meta.dirname;let root=path.resolve(here,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++)if(process.argv[i]==='--validate-only')validate=true;else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
function file(rel){if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('Relative source required');const p=root+'/'+rel;for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked source');if(!fs.statSync(p).isFile())throw Error('Regular source required');return p;}
const cp=here+'/sources.json',catalog=JSON.parse(fs.readFileSync(cp)),casesPath=here+'/cases.json',cases=JSON.parse(fs.readFileSync(casesPath));
if(catalog.version!==1||cases.length!==catalog.expected_executions)throw Error('Catalog');
const pins={[cp]:hash(cp),[casesPath]:hash(casesPath),[import.meta.filename]:hash(import.meta.filename)},selected=new Map();
for(const r of catalog.inputs){const p=file(r.path);if(hash(p)!==r.sha256)throw Error('Source drift '+r.path);pins[p]=r.sha256;if(r.kind==='ada'){const n=path.basename(p);if(selected.has(n))throw Error('Duplicate source '+n);selected.set(n,p);}else if(r.kind!=='metadata')throw Error('Input kind');}
for(const t of Object.values(catalog.tools)){if(hash(t.path)!==t.sha256)throw Error('Tool drift');pins[t.path]=t.sha256;}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input changed '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,compiledSources:selected.size,metadata:catalog.inputs.length-selected.size,cases:cases.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-native-power-source-'),runs=[],observations=[],oracles=[];fs.mkdirSync(out+'/source');
for(const[n,p]of selected){const d=out+'/source/'+n;fs.copyFileSync(p,d);pins[d]=hash(d);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,root,pins,runs,observations,oracles,profile:'release-O0-gnatp',newProof:false,nativeExecuted:false,hardwareValidated:false,scope:catalog.scope},null,2)+'\n');}
function run(label,args,seconds,oracle=false){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit',...(oracle?['--as=134217728','--stack=2097152']:['--as=25769803776']),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal});save(false);if(r.status!==0||r.error||r.signal)throw Error(label+' '+out);verify();return log;}
console.log(out);
for(const{mode,width}of cases){if(!/^[a-z][a-z-]*$/.test(mode)||![1,2].includes(width))throw Error('Case shape');const prefix=out+'/'+mode+width;
 const hid=mode==='no-match'?'"TEST0001"':'EisaId("PNP0C0C")';
 const member=mode==='notify'?'Method(_HID,0){Notify(BTN0,2) Notify(OTHR,0x80) Notify(BTN0,0x80) Return('+hid+')}':'Name(_HID,'+hid+')';
 const text='DefinitionBlock("","DSDT",'+width+',"CUBIT","BTNBRDG",1){Device(BTN0){'+member+' Name(_STA,0x0F)} Device(OTHR){Name(_HID,"TEST0002")}}\n';
 fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');run('iasl-'+mode+width,[catalog.tools.iasl.path,'-oa','-p',prefix,prefix+'.asl'],5,true);pins[prefix+'.aml']=hash(prefix+'.aml');
}
const project=out+'/test.gpr';fs.writeFileSync(project,'project Test is for Source_Dirs use("source");for Main use("native_button_tests.adb");for Object_Dir use"obj";for Exec_Dir use"bin";package Naming is for Spec("ACPI_Service") use"acpi_service_host.ads";end Naming;package Compiler is for Default_Switches("Ada") use("-O0","-gnatp","-gnat2022","-gnatwa","-gnatwe");end Compiler;end Test;\n');pins[project]=hash(project);
run('build',[catalog.tools.gprbuild.path,'-q','-p','-j1','-P',project],180);
const units=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__')).sort();
if(units.length!==catalog.expected_non_binder_alis)throw Error('Compiled unit inventory');
for(const n of units){const p=out+'/obj/'+n,lines=fs.readFileSync(p,'utf8').split('\n');for(const flag of ['-gnatp','-gnatwe','-gnat2022'])if(!lines.includes('A '+flag))throw Error('Compiler profile '+n);pins[p]=hash(p);}
const binary=out+'/bin/native_button_tests';pins[binary]=hash(binary);
for(const{mode,width}of cases){const prefix=out+'/'+mode+width,log=run('core-'+mode+width,[binary,prefix+'.aml',mode],30),m=/Native button ([a-z-]+) checks\s+(\d+)\n$/.exec(log);if(!m||m[1]!==mode)throw Error('Case verdict');observations.push({mode,width,checks:Number(m[2])});}
for(const mode of ['normal','no-match','notify'])for(const width of[1,2]){
 const prefix=out+'/'+mode+width,log=run('oracle-'+mode+width,[catalog.tools.acpiexec.path,'-l','-di','-dr','-b','evaluate BTN0._HID; evaluate BTN0._STA',prefix+'.aml'],5,true);
 const values=[...log.matchAll(/Evaluation of \\BTN0\.(_HID|_STA) returned object[^\n]*\n\s*\[(Integer|String)\][^\n]*/g)].map(m=>m[0]);
 if(values.length!==2||!(mode==='no-match'?/"TEST0001"/.test(values[0]):/\[Integer\]\s*=\s*0*C0CD041\b/i.test(values[0]))||!/\[Integer\]\s*=\s*0*F\b/i.test(values[1])||/Evaluation of .*failed|Aborting method/.test(log))throw Error('Oracle identity/status');
 const events=[...log.matchAll(/Received a (System|Device) Notify on \[(\w+)\][^\n]*?Value 0x([0-9A-Fa-f]+)/g)].map(m=>({kind:m[1],target:m[2],number:Number.parseInt(m[3],16)}));
 const expected=mode==='notify'?[{kind:'System',target:'BTN0',number:2},{kind:'Device',target:'OTHR',number:128},{kind:'Device',target:'BTN0',number:128}]:[];
 if(JSON.stringify(events)!==JSON.stringify(expected))throw Error('Oracle notifications');oracles.push({mode,width,identityMatched:true,statusMatched:true,events,cacheTrackingDiagnostic:/Outstanding cache allocations/.test(log)});
}
if(observations.length!==catalog.expected_executions||observations.reduce((n,r)=>n+r.checks,0)!==catalog.expected_checks)throw Error('Case coverage');
verify();save(true);console.log(JSON.stringify({completed:true,out,executions:observations.length,checks:catalog.expected_checks,nonBinderALIs:units.length,ACPICAComparisons:oracles.length,newProof:false,nativeExecuted:false}));
