import fs from 'node:fs';import path from 'node:path';import {spawnSync} from 'node:child_process';
import {sourcePins,hash,recipes,validate} from './check.mjs';import {audit} from './audit.mjs';
const here=path.dirname(new URL(import.meta.url).pathname);
if(process.argv.length===3&&process.argv[2]==='--validate-only'){console.log(JSON.stringify(validate(here)));process.exit(0);}
if(process.argv.length!==2)throw Error('Usage: run.sh [--validate-only]');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR must be set before Nix');
const {root,sources}=sourcePins(here),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-hid-channel-proof-');fs.mkdirSync(out+'/source');
const inputs={},copies={},recipePins=Object.fromEntries(recipes.map(n=>[n,hash(here+'/'+n)])),commands=[],tools={};
for(const name of ['gnatprove','gprbuild','cvc5']){const p=process.env.PATH.split(':').map(x=>x+'/'+name).find(fs.existsSync);if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('cached Nix tool required '+name);tools[name]={path:fs.realpathSync(p),sha256:hash(fs.realpathSync(p))};}
for(const[rel,sha]of Object.entries(sources)){const name=path.basename(rel),dest=out+'/source/'+name;fs.copyFileSync(root+'/'+rel,dest);inputs[root+'/'+rel]=sha;inputs[dest]=sha;copies[name]=sha;}
for(const name of ['channel_proof_model.ads','policy_main.adb']){fs.copyFileSync(here+'/'+name,out+'/source/'+name);inputs[out+'/source/'+name]=recipePins[name];}
function verify(){for(const[p,sha]of Object.entries(inputs))if(hash(p)!==sha)throw Error('input drift');for(const[n,sha]of Object.entries(recipePins))if(hash(here+'/'+n)!==sha)throw Error('recipe drift');for(const t of Object.values(tools))if(hash(t.path)!==t.sha256)throw Error('tool drift');}
function command(name,args,seconds,extra={}){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+(extra.level?'level'+extra.level+'-':'')+name+'.log',log);commands.push({name,...extra,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();if(r.status!==0||r.error||r.signal)throw Error(name+' failed; see '+out);return r;}
const project=`project Proof is
 for Source_Dirs use ("../source");
 for Object_Dir use "obj";
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnatwa", "-gnatwe");
 end Compiler;
 package Prove is
  for Proof_Dir use "proof";
 end Prove;
end Proof;
`;
let negative=0;
for(const level of [1,2]){
 const dir=out+'/level'+level;fs.mkdirSync(dir);fs.writeFileSync(dir+'/proof.gpr',project);
 const reports=[];
 for(const s of [{name:'prepare',unit:'channel_proof_model.ads',at:'acpi_hid_i2c_channels.adb:6',report:'channel_proof_model'},{name:'i2c-valid',unit:'i2c_channel_badges.ads',at:'i2c_channel_badges.ads:26',report:'i2c_channel_badges'},{name:'gpio-valid',unit:'gpio_channel_badges.ads',at:'gpio_channel_badges.ads:20',report:'gpio_channel_badges'}]){
  command(s.name,[tools.gnatprove.path,'-P',dir+'/proof.gpr','-u',s.unit,'--limit-subp='+s.at,'--level='+level,'--timeout=5','-j1','--report=all','--prover=cvc5'],180,{level});
  const bytes=fs.readFileSync(dir+'/obj/gnatprove/'+s.report+'.spark');fs.writeFileSync(out+'/level'+level+'-'+s.name+'.spark',bytes);reports.push(JSON.parse(bytes));
 }
 audit(...reports);
 const mutations=[r=>r[0].spark={},r=>r[0].proof.pop(),r=>r[0].proof[0].severity='medium',r=>r[0].flow[0].severity='warning',r=>r[0].skip_proof=[1],r=>r[0].skip_flow_proof=[1],r=>r[0].pragma_assume=[1],r=>r[0].assumptions[0].assumptions.push({arg:-1,predicate:'CLAIM_POST'}),r=>r[0].entities[' 3'].name='Unexpected.Valid',r=>r[1].assumptions=[{}],r=>r[2].spark={},r=>r[0].stop_reason='STOP_REASON_ERROR'];
 for(const mutate of mutations){const copy=structuredClone(reports);mutate(copy);let rejected=false;try{audit(...copy);}catch{rejected=true;}if(!rejected)throw Error('invalid proof accepted');negative++;}
}
const outputs=[];
for(const mode of ['checked','release']){
 const switches=['-O1','-gnat2022','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])];
 const gpr=`project Host is\n for Source_Dirs use ("source");\n for Main use ("policy_main.adb");\n for Object_Dir use "${mode}-obj";\n for Exec_Dir use "${mode}-bin";\n package Compiler is\n for Default_Switches ("Ada") use (${switches.map(x=>'"'+x+'"').join(', ')});\n end Compiler;\nend Host;\n`;
 fs.writeFileSync(out+'/'+mode+'.gpr',gpr);command(mode+'-build',[tools.gprbuild.path,'-p','-j1','-P',out+'/'+mode+'.gpr'],180);
 const r=command(mode+'-test',[out+'/'+mode+'-bin/policy_main'],30);if(r.stdout!=='channel policy checks= 325\n')throw Error('hosted check count');outputs.push(r.stdout);
}
if(outputs[0]!==outputs[1])throw Error('profile difference');verify();
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({accepted:true,commands,recipes:recipePins,source_copies:copies,tools,negative_controls:negative,hosted_checks_per_profile:325,scope:'Native-sized typed description adapter only; no hardware authority, parser, discovery or whole-interpreter proof'},null,2)+'\n');console.log(out);
