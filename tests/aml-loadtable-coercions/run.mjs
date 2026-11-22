import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
const here=path.dirname(fileURLToPath(import.meta.url)), root=path.resolve(here,'../..');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
let profile='release', validateOnly=false;
for(let i=2;i<process.argv.length;i++) {
 if(process.argv[i]==='--validate-only') validateOnly=true;
 else if(process.argv[i]==='--mode'&&['checked','release'].includes(process.argv[i+1])) profile=process.argv[++i];
 else throw Error('Usage: run.sh [--mode checked|release] [--validate-only]');
}
const pins={}, catalogPath=here+'/sources.json', suite=JSON.parse(fs.readFileSync(catalogPath));
pins[catalogPath]=sha(catalogPath);
for(const row of suite.inputs) {
 const p=path.resolve(root,row.path);
 if(!p.startsWith(root+path.sep)||sha(p)!==row.sha256) throw Error('Suite input drift '+row.path);
 pins[p]=row.sha256;
}
const sourceCatalogPath=root+'/tests/aml-module-execution-context/sources.json';
const catalog=JSON.parse(fs.readFileSync(sourceCatalogPath));
pins[sourceCatalogPath]=sha(sourceCatalogPath);
const files=[],names=new Set();
for(const row of catalog.inputs.filter(r=>r.kind==='ada')) {
 const p=root+'/'+row.path,name=path.basename(p);
 if(sha(p)!==row.sha256||names.has(name)) throw Error('Source selection drift '+row.path);
 names.add(name); files.push({p,name,hash:row.sha256}); pins[p]=row.sha256;
}
const cases=JSON.parse(fs.readFileSync(here+'/cases.json'));
if(cases.length!==10||new Set(cases).size!==10) throw Error('Case inventory');
if(validateOnly) { console.log('LoadTable coercion source/fixture inventory verified; no build or reference execution.'); process.exit(0); }
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp') throw Error('Set TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/aml-loadtable-coercions-'), inputs={...pins}, commands=[], counts={};
fs.mkdirSync(out+'/source');
for(const f of files) { const p=out+'/source/'+f.name; fs.copyFileSync(f.p,p); inputs[p]=f.hash; }
fs.copyFileSync(here+'/coercion_tests.adb',out+'/source/coercion_tests.adb');
inputs[out+'/source/coercion_tests.adb']=sha(out+'/source/coercion_tests.adb');
const find=name=>process.env.PATH.split(path.delimiter).map(p=>p+'/'+name).find(fs.existsSync);
const compiler=fs.realpathSync(find('gprbuild')),iasl=catalog.iasl.path;
if(!compiler.startsWith('/nix/store/')||sha(iasl)!==catalog.iasl.sha256) throw Error('Pinned tools required');
inputs[compiler]=sha(compiler); inputs[iasl]=sha(iasl);
function verify() { for(const[p,h]of Object.entries(inputs)) if(sha(p)!==h) throw Error('Input/tool drift '+p); }
function run(label,args,seconds) {
 verify();
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],
  {cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});
 const log=(r.stdout??'')+(r.stderr??''); fs.writeFileSync(out+'/'+label+'.log',log);
 commands.push({label,args,status:r.status,error:r.error?.message??null,signal:r.signal});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n'); verify();
 if(r.status!==0||r.error||r.signal) throw Error('Failed '+label+'; see '+out);
 return log;
}
console.log(out);
for(const revision of [1,2]) for(const name of cases) for(const part of ['main','child']) {
 const src=here+'/fixtures/'+name+'-'+part+'.asl',prefix=out+'/'+name+'-'+part+'-r'+revision;
 run('fixture-'+name+'-'+part+'-r'+revision,[iasl,'-oa','-r',String(revision),'-p',prefix,src],5);
 inputs[prefix+'.aml']=sha(prefix+'.aml');
}
const boundarySizes=[306,307,512];
for(const size of boundarySizes) for(const revision of [1,2]) {
 const src=out+'/boundary-'+size+'-r'+revision+'.asl',prefix=src.slice(0,-4);
 fs.writeFileSync(src,`DefinitionBlock ("", "DSDT", ${revision}, "CUBIT", "BOUNDARY", 1) {
 Method (LD00,5) { Return (LoadTable (Arg0,Arg1,Arg2,Arg3,Arg4,Zero)) }
 Method (GO00,0) { Return (LD00 ("OEMX","","00000001",Buffer (${size}) {},"")) }
}\n`); inputs[src]=sha(src);
 run('boundary-fixture-'+size+'-r'+revision,[iasl,'-oa','-r',String(revision),'-p',prefix,src],5);
 inputs[prefix+'.aml']=sha(prefix+'.aml');
}
const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
fs.writeFileSync(out+'/test.gpr',`project Test is
 for Source_Dirs use ("source");
 for Object_Dir use "obj";
 for Exec_Dir use "bin";
 for Main use ("coercion_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use (${flags.map(f=>JSON.stringify(f)).join(',')});
 end Compiler;
end Test;\n`);
run('build',[compiler,'-p','-j1','-P',out+'/test.gpr'],180);
function execute(label,main,child,mode) {
 const log=run(label,[out+'/bin/coercion_tests',main,child,mode,'candidate'],30);
 const m=/^LoadTable coercion checks\s+(\d+)\s*$/.exec(log);
 if(!m) throw Error('Missing check result'); counts[label]=Number(m[1]);
}
for(const revision of [1,2]) for(const name of cases)
 execute(name+'-r'+revision,out+'/'+name+'-main-r'+revision+'.aml',out+'/'+name+'-child-r'+revision+'.aml',name);
for(const size of boundarySizes) for(const revision of [1,2])
 execute('boundary-'+size+'-r'+revision,out+'/boundary-'+size+'-r'+revision+'.aml',out+'/strings-child-r'+revision+'.aml','boundary-root-'+size);
const total=Object.values(counts).reduce((s,n)=>s+n,0);
if(total!==431) throw Error('Coercion regression count '+total);
const artifacts={};
for(const n of fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali'))) {
 const p=out+'/obj/'+n; artifacts[p]=sha(p);
 if(n.startsWith('b__')) continue;
 const args=fs.readFileSync(p,'utf8').split('\n');
 for(const f of ['-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])]) if(!args.includes('A '+f)) throw Error('ALI profile '+p);
 if(args.includes('A '+(profile==='checked'?'-gnatp':'-gnata'))) throw Error('Mixed profile');
}
artifacts[out+'/bin/coercion_tests']=sha(out+'/bin/coercion_tests');
verify();
fs.writeFileSync(out+'/inputs.json',JSON.stringify(inputs,null,2)+'\n');
fs.writeFileSync(out+'/artifacts.json',JSON.stringify(artifacts,null,2)+'\n');
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({profile,counts,checks:total,commands:commands.length,
 short_oem_matching_difference_retained:true,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log({profile,checks:total,commands:commands.length,native_executed:false});
