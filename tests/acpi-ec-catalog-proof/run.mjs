import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname);
let root=path.resolve(here,'../..'),validateOnly=false;
for(let n=2;n<process.argv.length;n++){
 if(process.argv[n]==='--validate-only')validateOnly=true;
 else if(process.argv[n]==='--source-root'&&process.argv[n+1])root=path.resolve(process.argv[++n]);
 else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
}
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
const catalog=JSON.parse(fs.readFileSync(here+'/sources.json'));
const pins={[here+'/sources.json']:sha(here+'/sources.json'),[here+'/run.mjs']:sha(here+'/run.mjs'),[here+'/run.sh']:sha(here+'/run.sh')};
const source=[];
for(const [base,rows] of [[root,catalog.inputs],[here,catalog.helpers]])for(const e of rows){
 const f=path.resolve(base,e.path);
 if(!f.startsWith(base+path.sep)||sha(f)!==e.sha256)throw Error('Source baseline '+e.path);
 pins[f]=e.sha256;
 if(/\.ad[bs]$/.test(f))source.push(f);
}
if(catalog.version!==1||new Set(source.map(f=>path.basename(f))).size!==source.length)throw Error('Catalog');
if(validateOnly){console.log(JSON.stringify({mode:'validate-only',inputs:catalog.inputs.length,proof_units:source.length,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set TMPDIR before Nix');
const tool=fs.realpathSync(process.env.PATH.split(':').map(p=>p+'/gnatprove').find(fs.existsSync));
if(!tool.startsWith('/nix/store/'))throw Error('Nix required');pins[tool]=sha(tool);
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-ec-catalog-proof-');console.log(out);
fs.mkdirSync(out+'/source');
for(const f of source){const target=out+'/source/'+path.basename(f);fs.copyFileSync(f,target);pins[target]=sha(target);}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
function verify(){for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Source drift '+f);}
const reports=[];
for(const level of [1,2]){
 const dir=out+'/level'+level;fs.mkdirSync(dir);
 fs.writeFileSync(dir+'/proof.gpr','project Proof is\n for Source_Dirs use ("../source");\n for Object_Dir use "obj";\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022","-gnatwa","-gnatwe");\n end Compiler;\nend Proof;\n');
 const args=[tool,'-P',dir+'/proof.gpr','-u','ec_catalog_instance.ads','--mode=all','--level='+level,'--timeout=5','--prover=cvc5','-j1','--report=all','--checks-as-errors=on'];
 verify();
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','180',...args],{cwd:dir,encoding:'utf8',maxBuffer:1048576,timeout:183000});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(dir+'/proof.log',log.slice(0,1048576));
 fs.writeFileSync(dir+'/command.json',JSON.stringify({args,status:r.status,signal:r.signal,error:r.error?.message??null},null,2)+'\n');
 verify();if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Proof failed level'+level+' '+dir);
 const audit=spawnSync(process.execPath,[here+'/audit.mjs',dir],{encoding:'utf8',maxBuffer:1048576,timeout:10000});
 fs.writeFileSync(dir+'/audit.log',(audit.stdout??'')+(audit.stderr??''));
 if(audit.status!==0||audit.signal||audit.error)throw Error('Audit failed level'+level+' '+dir);
 verify();reports.push({level,...JSON.parse(fs.readFileSync(dir+'/AUDIT.json'))});
}
verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({reports,prover_commands:2,actual_engine_and_catalog_instance:true,trusted_callbacks:true,whole_EC_proved:false,native_executed:false,hardware_validated:false},null,2)+'\n');
console.log('EC catalog proof complete '+JSON.stringify(reports.map(r=>({level:r.level,proof_checks:r.proof_checks,flow_checks:r.flow_checks,valid_CVC5:r.valid_CVC5}))));
