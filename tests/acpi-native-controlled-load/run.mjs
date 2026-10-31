import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{fileURLToPath}from'node:url';import{spawnSync}from'node:child_process';
const suite=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(suite,'../..'),mode=process.argv[2]??'checked';
const resume=process.argv[3]==='--resume'?process.argv[4]:undefined;
if(!['checked','release','--validate-only'].includes(mode)||(process.argv.length>3&&(!resume||process.argv.length!==5||mode==='--validate-only')))throw Error('usage: run.sh [checked|release|--validate-only] [--resume RESULT_DIRECTORY]');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const files=fs.readFileSync(suite+'/source-files.txt','utf8').trim().split('\n');
const pins=new Map(fs.readFileSync(suite+'/inputs.sha256','utf8').trim().split('\n').map(l=>{const m=/^([0-9a-f]{64})  (.+)$/.exec(l);if(!m)throw Error('pin syntax');return[m[2],m[1]];}));
if(new Set(files).size!==files.length||files.some(f=>!pins.has(f)))throw Error('missing/duplicate source pin');
function verify(){for(const[rel,h]of pins){if(path.isAbsolute(rel)||rel.split('/').includes('..')||sha(root+'/'+rel)!==h)throw Error('input changed: '+rel);}}
verify();if(mode==='--validate-only'){console.log('Native controlled loader inputs verified; no compile.');process.exit(0);}
fs.mkdirSync(suite+'/results',{recursive:true});const work=resume?fs.realpathSync(resume):fs.mkdtempSync(suite+'/results/'+mode+'-');
if(resume&&(path.dirname(work)!==fs.realpathSync(suite+'/results')||!path.basename(work).startsWith(mode+'-')))throw Error('resume directory outside this profile');
for(const d of ['source','obj','bin']){if(resume){if(!fs.lstatSync(work+'/'+d).isDirectory()||fs.realpathSync(work+'/'+d)!==work+'/'+d)throw Error('resume directory layout');}else fs.mkdirSync(work+'/'+d);}
const names=new Set();for(const rel of files){const name=path.basename(rel);if(!/\.(ads|adb)$/.test(name))throw Error('non-Ada source');if(names.has(name))throw Error('duplicate unit');names.add(name);if(resume){if(!fs.lstatSync(work+'/source/'+name).isFile()||sha(work+'/source/'+name)!==sha(root+'/'+rel))throw Error('resume source drift');}else fs.copyFileSync(root+'/'+rel,work+'/source/'+name);}
if(resume){if(JSON.stringify(fs.readdirSync(work+'/source').sort())!==JSON.stringify([...names].sort()))throw Error('resume source inventory');for(const file of ['COMMANDS.json','RESULT.json'])if(fs.existsSync(work+'/'+file))fs.copyFileSync(work+'/'+file,work+'/'+file+'.before-resume-'+Date.now());}
const mains=['native_controlled_tests'];
const switches=['-O1','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];
fs.writeFileSync(work+'/host.gpr',`project Host is
 for Source_Dirs use ("source");
 for Object_Dir use "obj";
 for Exec_Dir use "bin";
 for Main use (${mains.map(m=>JSON.stringify(m+'.adb')).join(', ')});
 package Compiler is
 for Default_Switches ("Ada") use (${switches.map(s=>JSON.stringify(s)).join(', ')});
 end Compiler;
end Host;
`);
const commands=[];
function run(label,args,seconds=30){const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:work,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});fs.writeFileSync(work+'/'+label+'.log',(r.stdout??'')+(r.stderr??''));commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(work+'/COMMANDS.json',JSON.stringify(commands,null,2)+'\n');if(r.status!==0||r.signal||r.error)throw Error(label+' failed: '+work);return r.stdout;}
const oracles=[];
for(const main of mains)run('build-'+main,['gprbuild','-p','-j1','-P','host.gpr',main+'.adb'],180);
const observations=[];
for(const name of ['normal','reentry','default','info-denied','info-exception','not-ready','describe-denied','clock-unavailable','early-deadline','copy-refused','module-exception','module-refused','late-deadline'])for(const revision of ['1','2']){
 const output=run(name+'-'+revision,['bin/native_controlled_tests',name,revision]);
 const m=/Native controlled loader checks\s*(\d+)/.exec(output);if(!m)throw Error('missing native verdict');observations.push({name,revision,checks:Number(m[1])});
}
const checks=observations.reduce((n,r)=>n+r.checks,0);if(checks!==256||observations.length!==26)throw Error('unexpected native check total');
verify();
for(const f of fs.readdirSync(work+'/obj').filter(f=>f.endsWith('.ali')&&!f.startsWith('b__'))){
 const text=fs.readFileSync(work+'/obj/'+f,'utf8');
 if(!text.includes('A -gnatwe\n')||!text.includes(mode==='checked'?'A -gnata\n':'A -gnatp\n')||text.includes(mode==='checked'?'A -gnatp\n':'A -gnata\n'))throw Error('object profile mismatch: '+f);
 if(mode==='checked'&&!text.includes('A -gnato\n'))throw Error('missing checked overflow checks');
}
const artifactFiles=[...mains.map(n=>'bin/'+n),...fs.readdirSync(work+'/obj').filter(f=>f.endsWith('.ali')).map(f=>'obj/'+f)];
const artifacts=artifactFiles.map(file=>({file,sha256:sha(work+'/'+file)}));
fs.writeFileSync(work+'/RESULT.json',JSON.stringify({mode,checks,processes:observations.length,observations,oracles,source_files:files.length,artifacts,new_proof:false,native_executed:false,hardware_validated:false,scope:'Actual native reader/default/controlled facade and AML startup; modeled kernel authority/Startup/clock/EC/region barrier. No native syscall, provisioning, hardware or new proof.'},null,2)+'\n');console.log(JSON.stringify({mode,checks,processes:observations.length,result:work+'/RESULT.json'}));
