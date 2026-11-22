import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';import{fileURLToPath}from'node:url';
const here=path.dirname(fileURLToPath(import.meta.url)),sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const groups=['basic','nested','reciprocal','wide','dependencies','budgets'];let root=path.resolve(here,'../../..'),selection='all',profile='both',validate=false;
for(let i=2;i<process.argv.length;i++){
 const a=process.argv[i];if(a==='--validate-only')validate=true;
 else if(a==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
 else if(a==='--selection'&&process.argv[i+1])selection=process.argv[++i];
 else if(a==='--profile'&&process.argv[i+1])profile=process.argv[++i];
 else throw Error('Usage: run.sh [--source-root DIR] [--selection all|'+groups.join('|')+'] [--profile checked|release|both] [--validate-only]');
}
if(!['all',...groups].includes(selection)||!['both','checked','release'].includes(profile))throw Error('Selection/profile');
const chosen=selection==='all'?groups:[selection],catalog=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={},ada=[],fixtures=[];
if(catalog.version!==1)throw Error('Catalog version');
function select(base,row,local){const p=path.resolve(base,row.path);if(!p.startsWith(base+'/')||fs.realpathSync(p)!==p||sha(p)!==row.sha256)throw Error('Source pin '+row.path);pins[p]=row.sha256;if(p.endsWith('.asl'))fixtures.push(p);else if(/\.ad[bs]$/.test(p))ada.push(p);else throw Error('Source kind');}
for(const r of catalog.inputs)select(root,r,false);for(const r of catalog.tests)select(here,r,true);
if(new Set(ada.map(p=>path.basename(p))).size!==ada.length)throw Error('Ambiguous source basename');
if(validate){console.log(JSON.stringify({validated:true,sources:catalog.inputs.length,tests:catalog.tests.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-module-composed-');fs.mkdirSync(out+'/source');fs.mkdirSync(out+'/fixtures');console.log(out);
for(const p of ada){const to=out+'/source/'+path.basename(p);fs.copyFileSync(p,to);pins[to]=sha(to);}for(const p of fixtures){const to=out+'/fixtures/'+path.basename(p);fs.copyFileSync(p,to);pins[to]=sha(to);}
for(const n of ['run.sh','run.mjs','sources.json'])pins[here+'/'+n]=sha(here+'/'+n);
const tools={},commands=[],counts={},oracles=[],compiled=new Map();
for(const name of ['gprbuild','iasl','acpiexec']){const p=process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync);if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('Nix tool '+name);const real=fs.realpathSync(p);tools[name]={path:real,sha256:sha(real)};pins[real]=sha(real);}
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift '+p);}
function run(label,args,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/COMMANDS.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error('Failed '+label+'; '+out);return log;}
function fixture(family,kind,revision,scalar=false,oracle=false,read=false){const key=[family,kind,revision,scalar?'scalar':'buffer',oracle?'oracle':'module',read?'read':'write'].join('-');if(compiled.has(key))return compiled.get(key);
 const defs=[...(kind==='bank'?['-DBANK']:kind==='index'?['-DINDEX']:kind==='Wide_Data'?['-DDATA_FIELD']:[]),...(scalar?['-DSCALAR']:[]),...(oracle?['-DORACLE']:[]),...(read?['-DDATA_READ']:[])];
 const input={basic:'module',nested:'nested',reciprocal:'reciprocal',wide:'wide',dependencies:'dependencies'}[family];if(!input)throw Error('Fixture family');
 run('iasl-'+key,[tools.iasl.path,'-ve',...defs,'-r',String(revision),'-p',out+'/'+key,out+'/fixtures/'+input+'.asl'],30);const p=out+'/'+key+'.aml';pins[p]=sha(p);compiled.set(key,p);return p;
}
function reference(family,kind,revision,scalar=false){const file=fixture(family,kind,revision,scalar,true),label='reference-'+[family,kind,revision,scalar].join('-');let cmd,want;
 if(family==='basic'){cmd='execute TEST; evaluate OUT0; evaluate FVAL';want=[165,165,195];}
 else if(family==='nested'){cmd='execute TEST; evaluate OUT0; evaluate IDX0; evaluate DAT0; evaluate MDAT';want=[166,166,4,3,166];}
 else if(family==='reciprocal'){cmd='execute TEST; evaluate OUT0; evaluate BSEL; evaluate IDAT; evaluate MDAT';want=[166,166,3,4,166];}
 else cmd='execute TEST';
 const log=run(label,[tools.acpiexec.path,'-l','-di','-dr','-b',cmd,file],30);
 if(/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log))throw Error('Reference method failure');
 const row={family,kind,revision,scalar,cache_cleanup_diagnostic:log.includes('Outstanding cache allocations')};
 if(want){row.values=[...log.matchAll(/^  \[Integer\] = ([0-9A-F]+)\r?$/gm)].map(m=>Number.parseInt(m[1],16));if(JSON.stringify(row.values)!==JSON.stringify(want))throw Error('Reference integers');row.value_equivalent=true;}
 else if(family==='wide'){
  const m=/\[Buffer\] Length 09 =\s+0000: ((?:[0-9A-F]{2} ){8}[0-9A-F]{2})/.exec(log),expected=scalar?(revision===1?'10 32 54 76 00 00 00 00 00':'10 32 54 76 98 BA DC FE 00'):'88 77 66 55 44 33 22 11 FE';
  if(m?.[1]!==expected)throw Error('Reference wide buffer');row.buffer=m[1];row.value_equivalent=true;
 }else{
  const m=/\[Buffer\] Length 0B =\s+0000: ((?:[0-9A-F]{2} ){10}[0-9A-F]{2})/.exec(log),expected=kind==='Wide_Data'?'02 C3 00 00 00 00 00 00 00 00 00':'10 32 54 76 00 00 00 00 00 00 A6';
  if(m?.[1]!==expected)throw Error('Reference dependency buffer');row.buffer=m[1];row.value_equivalent=kind!=='Wide_Selector'||revision===1;row.known_32bit_bank_value_difference=!row.value_equivalent;
 }
 row.scope='Explicit initialized scalar/buffer values only; no automatic startup, physical protocol/request count, fault-policy or lifetime equivalence. Wide indexed bus is modeled. BankValue revision2 discrepancy is not equivalence.';oracles.push(row);
}
// Basic controls preseed data explicitly so the method and module models start
// from the same values; this does not compare automatic startup ordering.
if(chosen.includes('basic'))for(const k of ['bank','index'])for(const r of [1,2])reference('basic',k,r);
for(const family of ['nested','reciprocal'])if(chosen.includes(family))for(const r of [1,2])reference(family,'plain',r);
if(chosen.includes('wide'))for(const k of ['direct','bank'])for(const r of [1,2])for(const scalar of [false,true])reference('wide',k,r,scalar);
if(chosen.includes('dependencies'))for(const k of ['Wide_Selector','Wide_Data'])for(const r of [1,2])reference('dependencies',k,r);
const mains={basic:['module_composed_tests'],nested:['nested_startup_tests'],reciprocal:['reciprocal_startup_tests'],wide:['wide_startup_tests'],dependencies:['dependency_startup_tests'],budgets:['module_work_tests','module_remaining_work_tests']};
const expectedChecks={basic:396,nested:652,reciprocal:652,wide:1606,dependencies:4432,budgets:60};
for(const mode of (profile==='both'?['checked','release']:[profile])){
 const dir=out+'/'+mode;fs.mkdirSync(dir);const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])],project=dir+'/host.gpr';
 fs.writeFileSync(project,`project Host is\n for Source_Dirs use ("../source");\n for Main use (${chosen.flatMap(g=>mains[g]).map(n=>JSON.stringify(n+'.adb')).join(',')});\n for Object_Dir use "obj";\n for Exec_Dir use "bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(s=>JSON.stringify(s)).join(',')});\n end Compiler;\nend Host;\n`);pins[project]=sha(project);counts[mode]={};
 for(const group of chosen){counts[mode][group]={};for(const main of mains[group]){run(mode+'-build-'+main,[tools.gprbuild.path,'-p','-j1','-P',project,main+'.adb'],180);pins[dir+'/bin/'+main]=sha(dir+'/bin/'+main);}
  function test(main,label,args,pattern){const log=run(mode+'-'+group+'-'+label,[dir+'/bin/'+main,...args],30),m=pattern.exec(log);if(!m)throw Error('Missing case verdict');counts[mode][group][label]=Number(m[1]);}
  if(group==='basic')for(const k of ['bank','index'])for(const r of [1,2])for(const s of ['Ordinary','Missing','Ambiguous','Stale','Selector_Denied','Data_Read_Denied','Data_Write_Denied','Geometry_Drift','Unknown_Write','Default_Path'])test(mains[group][0],[k,r,s].join('-'),[fixture('basic',k,r),k==='bank'?'3':'2',s],/^Module composed checks (\d+)\n$/);
  else if(group==='budgets')for(const main of mains[group])for(const k of ['bank','index'])for(const r of [1,2])test(main,[main,k,r].join('-'),[fixture('basic',k,r),k==='bank'?'3':'2','Ordinary'],/^Module (?:remaining )?work checks (\d+)\n$/);
  else if(group==='nested'||group==='reciprocal'){
   const cases=['Ordinary','Missing_Data','Missing_Index','Ambiguous','Mixed_Providers','Unused_Provider','Stale','Read_Denied','Write_Denied','Selector_Denied','Index_Denied','Geometry_Drift','Requests_Five'].map(s=>[s,0]);for(const s of ['Before_Effect','After_Effect'])for(let n=1;n<=6;n++)cases.push([s,n]);
   for(const r of [1,2])for(const[s,n]of cases)test(mains[group][0],[r,s,n].join('-'),[fixture(group,'plain',r),s,String(n)],/^(?:Nested|Reciprocal) startup checks (\d+)\n$/);
  }else if(group==='wide')for(const k of ['direct','bank','index'])for(const r of [1,2])for(const s of ['Ordinary','Scalar','Missing_Last','Ambiguous_Last','Denied_Write','Denied_Read','Mixed_Providers','Unknown_Last'])test(mains[group][0],[k,r,s].join('-'),[fixture(group,k,r,s==='Scalar'),k+'_Field',s,String(r)],/^Wide startup checks (\d+)\n$/);
  else for(const k of ['Wide_Selector','Wide_Data']){
   const cases=['Ordinary','Source_Read_Denied','Source_Write_Denied','Missing_Last','Ambiguous_Last','Mixed_Providers','Requests_Ten'].map(s=>[s,0]);if(k==='Wide_Data')cases.push(['Read_Unsupported',0]);for(const s of ['Before_Effect','After_Effect'])for(let n=1;n<=(k==='Wide_Selector'?22:11);n++)cases.push([s,n]);
   for(const r of [1,2])for(const[s,n]of cases)test(mains[group][0],[k,r,s,n].join('-'),[fixture(group,k,r,false,false,s==='Read_Unsupported'),k,s,String(r),String(n)],/^Dependency startup checks (\d+)\n$/);
  }
  if(Object.values(counts[mode][group]).reduce((a,b)=>a+b,0)!==expectedChecks[group])throw Error('Group inventory '+group);
 }
 const alis=fs.readdirSync(dir+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));for(const n of alis){const lines=fs.readFileSync(dir+'/obj/'+n,'utf8').split('\n');for(const flag of flags.filter(s=>s.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('ALI profile '+n);}
}
if(profile==='both'&&JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('Profiles disagree');verify();
const completedGroups=Object.values(counts).flatMap(p=>Object.values(p));
const total=completedGroups.reduce((sum,g)=>sum+Object.values(g).reduce((a,b)=>a+b,0),0);
const processes=completedGroups.reduce((sum,g)=>sum+Object.keys(g).length,0);
fs.writeFileSync(out+'/INPUTS.json',JSON.stringify(pins,null,2)+'\n');fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({selection,profile,counts,checks:total,runtime_processes:processes,oracles,tools,commands:commands.length,new_proof:false,native_executed:false,hardware_validated:false,scope:'Actual module Field executor with modeled provider/cell hardware and qualified explicit ACPICA controls. Startup source/controller authority and native lifecycle remain independent. Wide DataName reads beyond scalar width remain unsupported.'},null,2)+'\n');console.log(JSON.stringify({checks:total,runtime_processes:processes,complete:out+'/COMPLETE.json'}));
