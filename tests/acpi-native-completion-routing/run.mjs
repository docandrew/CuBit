import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const here=import.meta.dirname,root=path.resolve(here,'../..'),sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
let mode='release',validate=false;
for(let i=2;i<process.argv.length;i++){if(process.argv[i]==='--validate-only')validate=true;else if(process.argv[i]==='--mode'&&['checked','release'].includes(process.argv[i+1]))mode=process.argv[++i];else throw Error('Usage: run.sh [--mode checked|release] [--validate-only]');}
const pins={},manifest=here+'/inputs.sha256';pins[manifest]=sha(manifest);
function resolve(rel){if(path.isAbsolute(rel)||rel.split(/[\\/]/).includes('..'))throw Error('Invalid relative path');let p=root+'/'+rel;for(let q=p;q!==root;q=path.dirname(q))if(fs.lstatSync(q).isSymbolicLink())throw Error('Linked input');if(!fs.lstatSync(p).isFile())throw Error('Nonregular input');return p;}
for(const line of fs.readFileSync(manifest,'utf8').trim().split('\n')){const m=/^([0-9a-f]{64})  (.+)$/.exec(line);if(!m)throw Error('Manifest syntax');const p=resolve(m[2]);if(sha(p)!==m[1])throw Error('Input mismatch '+m[2]);pins[p]=m[1];}
const dirs=fs.readFileSync(here+'/source-dirs.txt','utf8').trim().split('\n');
const files=dirs.flatMap(d=>fs.readdirSync(root+'/'+d).filter(n=>/\.ad[bs]$/.test(n)&&fs.lstatSync(root+'/'+d+'/'+n).isFile()).map(n=>d+'/'+n)).sort();
if(files.join('\n')+'\n'!==fs.readFileSync(here+'/source-files.txt','utf8'))throw Error('Source inventory mismatch');
for(const line of fs.readFileSync(here+'/mirrors.tsv','utf8').trim().split('\n')){const[a,b]=line.split('\t');if(sha(resolve(a))!==sha(resolve(b)))throw Error('Mirror mismatch '+a);}
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,inputs:Object.keys(pins).length,sources:files.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
function tool(n,nix=true){const p=process.env.PATH.split(path.delimiter).map(d=>d+'/'+n).find(p=>fs.existsSync(p));if(!p)throw Error('Missing '+n);const real=fs.realpathSync(p);if(nix&&!real.startsWith('/nix/store/'))throw Error('Use Nix '+n);pins[p]=sha(p);pins[real]=sha(real);return p;}
const compiler=tool('gprbuild'),timeout=tool('timeout'),limit=tool('prlimit',false),nice=tool('nice');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-completion-routing-'),runs=[];
function save(complete){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({complete,root,out,mode,pins,runs,proved:false,nativeExecuted:false,hardwareValidated:false},null,2)+'\n');}
function run(label,args,seconds){verify();const r=spawnSync(nice,['-n','19',timeout,'--kill-after=2s',String(seconds),limit,'--as=25769803776','--stack=67108864','--core=0',...args],{cwd:out,encoding:'utf8',timeout:(seconds+4)*1000,maxBuffer:1048576}),log=out+'/'+label+'.log';fs.writeFileSync(log,(r.stdout??'')+(r.stderr??''));runs.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null,log,sha256:sha(log)});save(false);if(r.status!==0||r.signal||r.error)throw Error('Failed '+label+' '+log);verify();return fs.readFileSync(log,'utf8');}
const project=out+'/routing.gpr',flags=['-O0','-gnat2022','-gnatwa','-gnatwe','-gnatp'],checked=['-O0','-gnat2022','-gnatwa','-gnatwe','-gnata','-gnato'];
const targets=['acpi_native_server.adb','observer_tests.adb','completion_tests.adb'];
const special=mode==='checked'?targets.map(n=>'for Switches ('+JSON.stringify(n)+') use ('+checked.map(JSON.stringify).join(',')+');').join(''):'';
fs.writeFileSync(project,'project Routing is for Source_Dirs use ('+dirs.map(d=>JSON.stringify(root+'/'+d)).join(',')+');for Main use ("observer_tests.adb","completion_tests.adb");for Object_Dir use "obj";for Exec_Dir use "bin";for Excluded_Source_Files use ("acpi_target_heap.adb");package Naming is for Body("ACPI_Target_Heap") use "acpi_target_heap_host.adb";end Naming;package Compiler is for Default_Switches("Ada") use ('+flags.map(JSON.stringify).join(',')+');'+special+'end Compiler;end Routing;');
console.log(out);run('build',[compiler,'-q','-p','-j1','-P',project],180);
if(run('observer',[out+'/bin/observer_tests'],30).trim()!=='Observer and native runner checks 812')throw Error('Observer verdict mismatch');
if(run('completion',[out+'/bin/completion_tests'],30).trim()!=='Native completion routing checks 107')throw Error('Completion verdict mismatch');
const alis=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__')).sort();
if(alis.join('\n')+'\n'!==fs.readFileSync(here+'/expected-alis.txt','utf8'))throw Error('ALI inventory mismatch');
for(const n of alis){const lines=fs.readFileSync(out+'/obj/'+n,'utf8').split('\n'),on=mode==='checked'&&targets.includes(n.replace(/\.ali$/,'.adb'));for(const f of ['-gnatwe',...(on?['-gnata','-gnato']:['-gnatp'])])if(!lines.includes('A '+f))throw Error('Missing ALI flag '+n+' '+f);if(lines.includes('A '+(on?'-gnatp':'-gnata')))throw Error('Wrong ALI profile '+n);}
verify();save(true);console.log('Passed; record '+out+'/RESULT.json');
