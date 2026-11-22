import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const here=path.dirname(import.meta.filename),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');let root=path.resolve(here,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++)if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else if(process.argv[i]==='--validate-only')validate=true;else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
const cat=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={},sources=[];
for(const[base,rows]of [[root,cat.inputs],[here,cat.tests]])for(const row of rows){const p=path.resolve(base,row.path);if(!p.startsWith(base+'/')||fs.realpathSync(p)!==p||hash(p)!==row.sha256)throw Error('Source pin '+row.path);pins[p]=row.sha256;sources.push(p);}
if(new Set(sources.map(p=>path.basename(p))).size!==sources.length)throw Error('Duplicate unit');
for(const name of ['run.sh','run.mjs','sources.json'])pins[here+'/'+name]=hash(here+'/'+name);
if(validate){console.log(JSON.stringify({validated:true,sources:cat.inputs.length,tests:cat.tests.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const out=fs.mkdtempSync(process.env.TMPDIR+'/ec-supervisor-control-');fs.mkdirSync(out+'/source');console.log(out);for(const p of sources){const to=out+'/source/'+path.basename(p);fs.copyFileSync(p,to);pins[to]=hash(to);}
const tool=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gprbuild').find(fs.existsSync));if(!tool.startsWith('/nix/store/'))throw Error('Nix tool');pins[tool]=hash(tool);
const commands=[],counts={};function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Input drift '+p);}
function run(label,args,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});fs.writeFileSync(out+'/'+label+'.log',(r.stdout??'')+(r.stderr??''));commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error)throw Error('Failed '+label+' '+out);return r.stdout;}
for(const profile of ['checked','release']){
 const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])],project=out+'/'+profile+'.gpr';
 fs.writeFileSync(project,`project ${profile} is\n for Source_Dirs use ("source");\n for Main use ("control_tests.adb");\n for Object_Dir use "${profile}/obj";\n for Exec_Dir use "${profile}/bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(x=>JSON.stringify(x)).join(',')});\n end Compiler;\nend ${profile};\n`);pins[project]=hash(project);
 run(profile+'-build',[tool,'-p','-j1','-P',project],180);const log=run(profile+'-runtime',[out+'/'+profile+'/bin/control_tests'],30),m=/Control checks\s+(\d+)/.exec(log);if(!m)throw Error('Checks');counts[profile]=Number(m[1]);
 for(const n of fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){const lines=fs.readFileSync(out+'/'+profile+'/obj/'+n,'utf8').split('\n');for(const f of flags.filter(f=>f.startsWith('-gnat')))if(!lines.includes('A '+f))throw Error('ALI flags');}
}
if(counts.checked!==counts.release)throw Error('Profile disagreement');verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({commands,counts,inputs:pins,hardware_source_and_callback_behavior_modeled:true,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify(counts));
