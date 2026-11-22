import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';import{fileURLToPath}from'node:url';
const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..'),validateOnly=false;
for(let i=2;i<process.argv.length;i++){if(process.argv[i]==='--validate-only')validateOnly=true;else if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');}
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),catalog=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={},inputs=[];
if(catalog.version!==1)throw Error('Catalog version');
function select(base,row){const p=path.resolve(base,row.path);if(!p.startsWith(base+'/')||fs.realpathSync(p)!==p||sha(p)!==row.sha256)throw Error('Source pin '+row.path);pins[p]=row.sha256;inputs.push(p);}
for(const row of catalog.inputs)select(root,row);for(const row of catalog.tests)select(here,row);
const basenames=inputs.map(p=>path.basename(p));if(new Set(basenames).size!==basenames.length)throw Error('Ambiguous source');
if(validateOnly){console.log(JSON.stringify({validated:true,sources:catalog.inputs.length,tests:catalog.tests.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-region-cleanup-');fs.mkdirSync(out+'/source');console.log(out);
for(const p of inputs){const to=out+'/source/'+path.basename(p);fs.copyFileSync(p,to);pins[to]=sha(to);}
for(const n of ['run.sh','run.mjs','sources.json'])pins[here+'/'+n]=sha(here+'/'+n);
const tools={},commands=[],counts={},oracle={};
for(const name of ['gprbuild','iasl','acpiexec']){const p=process.env.PATH.split(':').map(d=>d+'/'+name).find(fs.existsSync);if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('Nix tool '+name);const real=fs.realpathSync(p);tools[name]={path:real,sha256:sha(real)};pins[real]=sha(real);}
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift '+p);}
function run(label,args,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error('Failed '+label+'; '+out);return log;}
for(const revision of [1,2]){
 run('iasl-'+revision,[tools.iasl.path,'-ve','-r',String(revision),'-p',out+'/region-'+revision,out+'/source/region.asl'],30);
 pins[out+'/region-'+revision+'.aml']=sha(out+'/region-'+revision+'.aml');
 const log=run('acpica-'+revision,[tools.acpiexec.path,'-l','-di','-dr','-b','evaluate CALL; execute DEV0._REG 3 1; evaluate CALL; execute DEV0._REG 3 1; evaluate CALL; execute DEV0._REG 3 0; evaluate CALL; execute DEV0._REG 3 0; evaluate CALL',out+'/region-'+revision+'.aml'],30);
 const integers=[...log.matchAll(/^Evaluation of \\CALL returned object [^\n]+\n  \[Integer\] = ([0-9A-F]+)\n/gm)].map(m=>Number.parseInt(m[1],16));
 const packages=[...log.matchAll(/^Evaluation of \\DEV0\._REG returned object [^\n]+\n  \[Package\] Contains 2 Elements:\n    \[Integer\] = 0000000000000001\n    \[Integer\] = 0000000000000002\n/gm)];
 if(JSON.stringify(integers)!=='[0,1,2,3,4]'||packages.length!==4||/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log))throw Error('Reference mismatch');
 oracle[revision]={integers,packages:packages.length,cache_cleanup_diagnostic:log.includes('Outstanding cache allocations'),scope:'Explicit alias method effects/package values only; no cleanup-policy or fault-model equivalence.'};
}
const scenarios=['Ordinary','Overcharge','Overcharge_Refused','Refused_Release','Uncleared_Release','Cleared_Failure','Readiness_Lost','Access_Failure','Missing_Handle','Disconnect_Refused'];
for(const profile of ['checked','release']){
 const dir=out+'/'+profile;fs.mkdirSync(dir);
 const flags=['-gnat2022','-gnatwa','-gnatwe','-O1',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])],project=dir+'/host.gpr';
 fs.writeFileSync(project,`project Host is\n for Source_Dirs use ("../source");\n for Main use ("region_cleanup_tests.adb");\n for Object_Dir use "obj";\n for Exec_Dir use "bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(x=>JSON.stringify(x)).join(',')});\n end Compiler;\nend Host;\n`);pins[project]=sha(project);
 run(profile+'-build',[tools.gprbuild.path,'-p','-j1','-P',project],180);
 const exe=dir+'/bin/region_cleanup_tests';pins[exe]=sha(exe);counts[profile]={};
 for(const revision of [1,2])for(const scenario of scenarios){const label=profile+'-'+revision+'-'+scenario,log=run(label,[exe,out+'/region-'+revision+'.aml',scenario],30),m=/^Region cleanup checks (\d+)\n$/.exec(log);if(!m)throw Error('Missing checks');counts[profile][revision+'-'+scenario]=Number(m[1]);}
 const alis=fs.readdirSync(dir+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));
 if(!alis.includes('acpi_region_initialization.ali'))throw Error('Missing production ALI');
 for(const n of alis){const lines=fs.readFileSync(dir+'/obj/'+n,'utf8').split('\n');for(const flag of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('ALI profile '+n);}
}
if(JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('Profile mismatch');verify();
const total=Object.values(counts).reduce((s,p)=>s+Object.values(p).reduce((a,b)=>a+b,0),0);if(total!==948)throw Error('Case inventory changed');
fs.writeFileSync(out+'/INPUTS.json',JSON.stringify(pins,null,2)+'\n');fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,total_checks:total,runtime_processes:40,oracle,commands:commands.length,tools,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify({total_checks:total,runtime_processes:40}));
