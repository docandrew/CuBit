import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{fileURLToPath}from'node:url';import{spawnSync}from'node:child_process';
const suite=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(suite,'../..'),mode=process.argv[2]??'checked';
const resume=process.argv[3]==='--resume'?process.argv[4]:undefined;
if(!['checked','release','--validate-only'].includes(mode)||(process.argv.length>3&&(!resume||process.argv.length!==5||mode==='--validate-only')))throw Error('usage: run.sh [checked|release|--validate-only] [--resume RESULT_DIRECTORY]');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const files=fs.readFileSync(suite+'/source-files.txt','utf8').trim().split('\n');
const pins=new Map(fs.readFileSync(suite+'/inputs.sha256','utf8').trim().split('\n').map(l=>{const m=/^([0-9a-f]{64})  (.+)$/.exec(l);if(!m)throw Error('pin syntax');return[m[2],m[1]];}));
if(new Set(files).size!==files.length||files.some(f=>!pins.has(f)))throw Error('missing/duplicate source pin');
function verify(){for(const[rel,h]of pins){if(path.isAbsolute(rel)||rel.split('/').includes('..')||sha(root+'/'+rel)!==h)throw Error('input changed: '+rel);}}
verify();if(mode==='--validate-only'){console.log('Module startup inputs verified; no compile.');process.exit(0);}
fs.mkdirSync(suite+'/results',{recursive:true});const work=resume?fs.realpathSync(resume):fs.mkdtempSync(suite+'/results/'+mode+'-');
if(resume&&(path.dirname(work)!==fs.realpathSync(suite+'/results')||!path.basename(work).startsWith(mode+'-')))throw Error('resume directory outside this profile');
for(const d of ['source','obj','bin']){if(resume){if(!fs.lstatSync(work+'/'+d).isDirectory()||fs.realpathSync(work+'/'+d)!==work+'/'+d)throw Error('resume directory layout');}else fs.mkdirSync(work+'/'+d);}
const names=new Set();for(const rel of files){const name=path.basename(rel);if(!/\.(ads|adb)$/.test(name))throw Error('non-Ada source');if(names.has(name))throw Error('duplicate unit');names.add(name);if(resume){if(!fs.lstatSync(work+'/source/'+name).isFile()||sha(work+'/source/'+name)!==sha(root+'/'+rel))throw Error('resume source drift');}else fs.copyFileSync(root+'/'+rel,work+'/source/'+name);}
if(resume){if(JSON.stringify(fs.readdirSync(work+'/source').sort())!==JSON.stringify([...names].sort()))throw Error('resume source inventory');for(const file of ['COMMANDS.json','RESULT.json'])if(fs.existsSync(work+'/'+file))fs.copyFileSync(work+'/'+file,work+'/'+file+'.before-resume-'+Date.now());}
const mains=['field_binding_set_tests','module_field_tests','module_startup_tests','module_datum_tests','module_datum_boundaries'];
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
for(const[name,second]of [['module','00000000000000E1'],['read','0000000000000033']]){
 fs.copyFileSync(suite+'/fixtures/'+name+'.asl',work+'/'+name+'.asl');
 run('iasl-'+name,['iasl','-p',name,name+'.asl']);
 const output=run('acpica-'+name,['acpiexec','-b','execute _INI; evaluate FLD1; evaluate FLD2; evaluate MARK',name+'.aml']);
 const values=[...output.matchAll(/\[Integer\] = ([0-9A-F]{16})/g)].map(m=>m[1]);
 if(JSON.stringify(values)!==JSON.stringify(['0000000000000044',second,'000000000000005A']))throw Error('ACPICA scalar comparison');
 oracles.push({name,values,cache_cleanup_diagnostic:output.includes('Outstanding cache allocations')});
}
const datumOutput=run('datum-oracle',[process.execPath,suite+'/datum-oracle.mjs']);
const datumDirectory=datumOutput.split('\n').find(line=>line.startsWith('/home/doc/cubit-build-tmp/module-datum-oracle-'));
if(!datumDirectory)throw Error('missing datum comparison record');
oracles.push({name:'datum-plans',record:datumDirectory+'/COMPLETE.json',sha256:sha(datumDirectory+'/COMPLETE.json'),scope:'Simulated EC scalar/partial preservation; no automatic startup/protocol comparison.'});
const nineOutput=run('nine-oracle',[process.execPath,suite+'/nine-oracle.mjs']);
const nineDirectory=nineOutput.split('\n').find(line=>line.startsWith('/home/doc/cubit-build-tmp/module-nine-comparison-'));
if(!nineDirectory)throw Error('missing nine-cell comparison record');
oracles.push({name:'nine-cell-comparison',record:nineDirectory+'/COMPLETE.json',sha256:sha(nineDirectory+'/COMPLETE.json'),scope:'Tight EC reference discrepancy recorded separately; memory/padded EC value controls match. No protocol/startup comparison.'});
for(const main of mains)run('build-'+main,['gprbuild','-p','-j1','-P','host.gpr',main+'.adb'],180);
const observations=[];
function test(main,name,args,pattern){const output=run(main+'-'+name,['bin/'+main,...args]);const m=pattern.exec(output);if(!m)throw Error('missing verdict');observations.push({main,name,checks:Number(m[1])});}
test('field_binding_set_tests','baseline',[],/Field binding set checks\s*(\d+)/);
for(const name of ['normal','read','absent','ambiguous','unknown','fatal','denied','stale','forged-base','forged-space','default'])test('module_field_tests',name,[name],/Module field checks\s*(\d+)/);
for(const name of ['normal','read','absent','ambiguous','unknown','fatal','denied','stale','forged-base','forged-space','default','unselected','callback-exception','overbudget'])test('module_startup_tests',name,[name],/Module startup checks\s*(\d+)/);
for(const name of ['wide','partial','write-only','partial-write-only','missing','ambiguous','denied-second','stale','forged-base','unknown','partial-unknown','drift','default'])test('module_datum_tests',name,[name],/Module datum checks\s*(\d+)/);
for(const name of ['normal','missing-last','ambiguous-last','denied-last','read-denied-last','unknown-last'])test('module_datum_boundaries',name,[name],/Module datum boundary checks\s*(\d+)/);
const checks=observations.reduce((n,r)=>n+r.checks,0);if(checks!==1030||observations.length!==45)throw Error('unexpected check total');
verify();
for(const f of fs.readdirSync(work+'/obj').filter(f=>f.endsWith('.ali')&&!f.startsWith('b__'))){
 const text=fs.readFileSync(work+'/obj/'+f,'utf8');
 if(!text.includes('A -gnatwe\n')||!text.includes(mode==='checked'?'A -gnata\n':'A -gnatp\n')||text.includes(mode==='checked'?'A -gnatp\n':'A -gnata\n'))throw Error('object profile mismatch: '+f);
 if(mode==='checked'&&!text.includes('A -gnato\n'))throw Error('missing checked overflow checks');
}
const artifactFiles=[...mains.map(n=>'bin/'+n),...fs.readdirSync(work+'/obj').filter(f=>f.endsWith('.ali')).map(f=>'obj/'+f)];
const artifacts=artifactFiles.map(file=>({file,sha256:sha(work+'/'+file)}));
fs.writeFileSync(work+'/RESULT.json',JSON.stringify({mode,checks,processes:observations.length,observations,oracles,source_files:files.length,artifacts,new_proof:false,native_executed:false,hardware_validated:false,scope:'Actual copied snapshot/module/device startup and explicit named direct single/multi-datum accesses with a modeled EC provider and modeled region barrier. Native provisioning, region connection and recovery lifetime remain separate obligations.'},null,2)+'\n');console.log(JSON.stringify({mode,checks,processes:observations.length,result:work+'/RESULT.json'}));
