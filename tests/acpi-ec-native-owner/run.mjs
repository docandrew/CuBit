import fs from'node:fs';import path from'node:path';import crypto from'node:crypto';import{spawnSync}from'node:child_process';
const dir=path.dirname(import.meta.filename);let root=path.resolve(dir,'../..'),validate=false;
for(let i=2;i<process.argv.length;i++){if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else if(process.argv[i]==='--validate-only')validate=true;else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');}
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const catalog=JSON.parse(fs.readFileSync(dir+'/sources.json'));
if(catalog.version!==1)throw Error('catalog version');
const pins={[dir+'/sources.json']:sha(dir+'/sources.json'),[dir+'/run.mjs']:sha(dir+'/run.mjs')},origins={};
for(const row of catalog.inputs){const p=path.resolve(root,row.path);if(!p.startsWith(root+path.sep)||fs.realpathSync(p)!==p||sha(p)!==row.sha256)throw Error('source pin '+row.path);pins[p]=row.sha256;const name=path.basename(p);if(origins[name])throw Error('duplicate unit');origins[name]=p;}
if(validate){console.log(JSON.stringify({validated:true,sources:catalog.inputs.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR before Nix');
const out=fs.mkdtempSync(process.env.TMPDIR+'/ec-native-owner-');console.log(out);fs.mkdirSync(out+'/source');
for(const[name,origin]of Object.entries(origins)){const dest=out+'/source/'+name;fs.copyFileSync(origin,dest);pins[dest]=sha(dest);}
const tool=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gprbuild').find(fs.existsSync));if(!tool.startsWith('/nix/store/'))throw Error('Nix required');pins[tool]=sha(tool);
const commands=[],counts={};function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('source drift '+p);}
function run(label,args,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('failed '+label+' '+out);return log;}
for(const profile of ['checked','release']){
 const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
 fs.writeFileSync(out+'/'+profile+'.gpr',`project ${profile} is\n for Source_Dirs use ("source");\n for Object_Dir use "${profile}/obj";\n for Exec_Dir use "${profile}/bin";\n for Main use ("owner_tests.adb");\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(s=>JSON.stringify(s)).join(',')});\n end Compiler;\nend ${profile};\n`);
 run(profile+'-build',[tool,'-p','-j1','-P',out+'/'+profile+'.gpr'],180);counts[profile]={};
 for(const args of catalog.cases){
  const mode=args.join("-");
  const log=run(profile+'-'+mode,[out+'/'+profile+'/bin/owner_tests',...args],30),m=/EC owner checks\s+(\d+)/.exec(log);if(!m)throw Error('missing checks');counts[profile][mode]=Number(m[1]);
 }
 const alis=fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));if(JSON.stringify(alis.map(n=>n.slice(0,-4)).sort())!==JSON.stringify(catalog.units))throw Error('compiled closure');
 for(const n of alis){const text=fs.readFileSync(out+'/'+profile+'/obj/'+n,'utf8').split('\n');for(const f of flags.filter(s=>s.startsWith('-gnat')))if(!text.includes('A '+f))throw Error('ALI profile');}
}
if(JSON.stringify(counts.checked)!==JSON.stringify(counts.release))throw Error('profile disagreement');verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands,inputs:pins,origins,hosted:true,hardware_clock_and_kernel_queues_modeled:true,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');console.log(JSON.stringify(counts));
