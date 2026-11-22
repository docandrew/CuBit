import fs from 'node:fs';
import path from 'node:path';
import cp from 'node:child_process';
import crypto from 'node:crypto';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..');
if(process.argv.length!==4||process.argv[2]!=='--output-root')throw Error('usage: run.sh --output-root <private-directory>');
const out=fs.mkdtempSync(path.join(process.argv[3],'orphan-reg-'));console.log(out);
const sourceDir=out+'/source';fs.mkdirSync(sourceDir);
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const names=JSON.parse(fs.readFileSync(here+'/sources.json','utf8')),pins={},seen=new Set();
for(const n of names){
 if(path.isAbsolute(n)||n.split('/').includes('..')||!fs.lstatSync(root+'/'+n).isFile()||seen.has(path.basename(n)))throw Error('invalid input '+n);
 seen.add(path.basename(n));pins[n]=sha(root+'/'+n);fs.copyFileSync(root+'/'+n,sourceDir+'/'+path.basename(n));
 if(sha(root+'/'+n)!==pins[n]||sha(sourceDir+'/'+path.basename(n))!==pins[n])throw Error('changed input');
}
for(const n of ['run.sh','run.mjs','sources.json']){pins['tests/acpi-orphan-reg/'+n]=sha(here+'/'+n);fs.copyFileSync(here+'/'+n,out+'/'+n);}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+String.fromCharCode(10));
const commands=[];
function run(args,label,seconds){
 const r=cp.spawnSync('timeout',['--kill-after=5s',seconds+'s',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
 commands.push({command:args,label,seconds,status:r.status,error:r.error?.message??null,truncated:log.length>=1048576});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');console.log(label,r.status);
 if(r.status!==0||r.error||log.length>=1048576){console.log(log.slice(0,4096));throw Error('command failed');}return log;
}
const tools={};for(const n of ['gprbuild','iasl','acpiexec']){const match=(n==='gprbuild'?process.env.PATH:process.env.ACPICA_BIN??process.env.PATH).split(':').map(d=>path.join(d,n)).find(p=>fs.existsSync(p));if(!match)throw Error('missing tool');const p=fs.realpathSync(match);tools[n]={path:p,sha256:sha(p)};}
fs.writeFileSync(out+'/tools.json',JSON.stringify(tools,null,2)+'\n');
const oracle={};for(const revision of [1,2]){
 run([tools.iasl.path,'-ve','-r',String(revision),'-p',out+'/orphans-'+revision,sourceDir+'/orphans.asl'],'iasl-'+revision,5);
 run([tools.iasl.path,'-ve','-DDEFERRED_CASE','-r',String(revision),'-p',out+'/deferred-'+revision,sourceDir+'/orphans.asl'],'iasl-deferred-'+revision,5);
 const text=run([tools.acpiexec.path,'-l','-di','-dr','-b','evaluate CALL; execute EC00._REG 3 1; execute GP00._REG 8 1; execute MEM0._REG 3 1; evaluate CALL; evaluate SPAC; evaluate CODE; execute EC00._REG 3 0; execute GP00._REG 8 0; execute MEM0._REG 3 0; evaluate CALL; evaluate CODE',out+'/orphans-'+revision+'.aml'],'acpica-'+revision,5);
 const integers=[...text.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));if(JSON.stringify(integers)!==JSON.stringify([0,3,3,1,6,0])||/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(text))throw Error('oracle mismatch');oracle[revision]={integers,aml_sha256:sha(out+'/orphans-'+revision+'.aml'),cache_note:text.includes('Outstanding cache allocations')};
}
const cases=['EC_Normal','GPIO_Normal','Other_Space','No_Local_Method','EC_Region_Present','GPIO_Region_Present','Deferred','Nonmethod','Foreign_Device','Unavailable','Zero_Budget','Readiness_Lost','Unknown_Completion','Overcharge','Missing_Handle','Compound_Cleanup','Fatal_Handler','Wrong_Arity','Foreign_Disconnect'];
const profiles={};for(const profile of ['checked','release']){
 const gpr=out+'/'+profile+'.gpr';fs.writeFileSync(gpr,`project ${profile} is
 for Source_Dirs use ("${sourceDir}");
 for Main use ("orphan_reg_tests.adb");
 for Object_Dir use "${profile}/obj";
 for Exec_Dir use "${profile}/bin";
 package Compiler is
 for Default_Switches ("Ada") use ("-gnat2022", "-O1", "-gnatwa", "-gnatwe", ${profile==='checked'?'"-gnata", "-gnato"':'"-gnatp"'});
 end Compiler;
end ${profile};
`);
 run([tools.gprbuild.path,'-p','-j1','-P',gpr],profile+'-build',180);profiles[profile]={checks:0,cases:0};
 for(const revision of [1,2])for(const scenario of cases){const fixture=scenario==='Deferred'?'deferred':'orphans';const text=run([out+'/'+profile+'/bin/orphan_reg_tests',out+'/'+fixture+'-'+revision+'.aml',scenario],profile+'-'+revision+'-'+scenario,30);const m=text.match(/^Orphan REG checks (\d+)\n$/);if(!m)throw Error('missing checks');profiles[profile].checks+=Number(m[1]);profiles[profile].cases++;}
 profiles[profile].binary_sha256=sha(out+'/'+profile+'/bin/orphan_reg_tests');
}
for(const[n,h]of Object.entries(pins))if(sha(root+'/'+n)!==h)throw Error('input changed '+n);
const inputs=Object.fromEntries(fs.readdirSync(sourceDir).map(n=>[n,sha(sourceDir+'/'+n)]));
fs.writeFileSync(out+'/complete.json',JSON.stringify({profiles,oracle,commands:commands.length,source_sha256:inputs,inputs_unchanged:true,new_proof:false,native_validated:false,hardware_validated:false,full_goal_complete:false},null,2)+'\n');console.log(JSON.stringify({profiles,commands:commands.length}));
