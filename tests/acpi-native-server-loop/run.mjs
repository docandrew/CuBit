import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..'),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');let profile='release',validate=false;
for(let i=2;i<process.argv.length;i++){if(process.argv[i]==='--validate-only')validate=true;else if(process.argv[i]==='--mode'&&['checked','release'].includes(process.argv[i+1]))profile=process.argv[++i];else throw Error('Usage: run.sh [--mode checked|release] [--validate-only]');}
const pins={},manifest=here+'/inputs.sha256';pins[manifest]=hash(manifest);
function resolve(rel){if(path.isAbsolute(rel)||rel.split(/[\\/]/).includes('..'))throw Error('invalid inventory path');const p=root+'/'+rel;let q=p;while(q!==root){if(fs.lstatSync(q).isSymbolicLink())throw Error('linked input');q=path.dirname(q);}if(!fs.lstatSync(p).isFile())throw Error('nonregular input');return p;}
for(const line of fs.readFileSync(manifest,'utf8').trim().split('\n')){const m=line.match(/^([0-9a-f]{64}) [ *](.+)$/);if(!m)throw Error('manifest syntax');const p=resolve(m[2]);if(hash(p)!==m[1])throw Error('input mismatch '+m[2]);pins[p]=m[1];}
const dirs=fs.readFileSync(here+'/source-dirs.txt','utf8').trim().split('\n');const files=dirs.flatMap(dir=>fs.readdirSync(root+'/'+dir).filter(n=>/\.ad[bs]$/.test(n)&&fs.lstatSync(root+'/'+dir+'/'+n).isFile()).map(n=>dir+'/'+n)).sort();
if(files.join('\n')+'\n'!==fs.readFileSync(here+'/source-files.txt','utf8'))throw Error('source inventory mismatch');
for(const line of fs.readFileSync(here+'/mirrors.tsv','utf8').trim().split('\n')){const[copy,original]=line.split('\t');if(hash(resolve(copy))!==hash(resolve(original)))throw Error('mirror mismatch '+copy);}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);}
if(validate){verify();console.log(JSON.stringify({validated:true,inputs:Object.keys(pins).length,sources:files.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
function tool(n,nix=true){const p=process.env.PATH.split(path.delimiter).map(d=>path.join(d,n)).find(p=>fs.existsSync(p));if(!p)throw Error('missing '+n);const real=fs.realpathSync(p);if(nix&&!real.startsWith('/nix/store/'))throw Error('use Nix '+n);pins[p]=hash(p);pins[real]=hash(real);return p;}
const compiler=tool('gprbuild'),timeout=tool('timeout'),limit=tool('prlimit',false),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-server-loop-'),runs=[];
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,root,out,profile,pins,runs,proved:false,nativeExecuted:false,hardwareValidated:false},null,2)+'\n');}
function run(label,args,seconds){verify();const r=spawnSync(timeout,['--kill-after=2s',String(seconds),limit,'--as=25769803776',...args],{cwd:root,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');runs.push({label,status:r.status,error:r.error?.message,output});save(false);fs.writeFileSync(out+'/'+label+'.log',output);if(r.status!==0||r.error)throw Error(label+' failed; '+out+'/'+label+'.log');verify();return output;}
console.log('Hosted native serving-loop logs: '+out);
run('build',[compiler,'-q','-p','-j1','-P',here+'/'+profile+'.gpr','-XOBSERVER_BUILD='+out],180);
const output=run('test',[out+'/bin/observer_tests'],30);if(output.trim()!=='Observer and native runner checks 812')throw Error('check marker mismatch');
const alis=fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__')).sort();
if(alis.join('\n')+'\n'!==fs.readFileSync(here+'/expected-alis.txt','utf8'))throw Error('ALI unit inventory mismatch');
for(const n of alis){const text=fs.readFileSync(out+'/obj/'+n,'utf8'),checked=profile==='checked'&&['observer_tests.ali','acpi_native_server.ali'].includes(n);
 for(const flag of ['-gnatwe',...(checked?['-gnata','-gnato']:['-gnatp'])])if(!text.split('\n').includes('A '+flag))throw Error('missing ALI flag '+n+' '+flag);
 if(text.split('\n').includes('A '+(checked?'-gnatp':'-gnata')))throw Error('wrong ALI profile '+n);
}
verify();save(true);console.log('Passed; record '+out+'/RESULT.json');
