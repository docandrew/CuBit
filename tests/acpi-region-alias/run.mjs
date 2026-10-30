import fs from 'node:fs';
import path from 'node:path';
import cp from 'node:child_process';
import crypto from 'node:crypto';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..');
if(process.argv.length!==4||process.argv[2]!=='--output-root')throw Error('usage: run.sh --output-root <private-directory>');
const out=fs.mkdtempSync(path.join(process.argv[3],'region-alias-'));console.log(out);
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const names=JSON.parse(fs.readFileSync(here+'/sources.json','utf8')),pins={},adaNames=new Set();
for(const n of names){
 if(path.isAbsolute(n)||n.split('/').includes('..')||!fs.lstatSync(root+'/'+n).isFile())throw Error('invalid input '+n);
 const target=out+'/source/'+n;fs.mkdirSync(path.dirname(target),{recursive:true});pins[n]=sha(root+'/'+n);fs.copyFileSync(root+'/'+n,target);
 if(sha(target)!==pins[n]||sha(root+'/'+n)!==pins[n])throw Error('changed input');
 if(/\.ad[bs]$/.test(n)){if(adaNames.has(path.basename(n)))throw Error('duplicate unit');adaNames.add(path.basename(n));}
}
for(const n of ['run.sh','run.mjs','sources.json']){pins['tests/acpi-region-alias/'+n]=sha(here+'/'+n);fs.copyFileSync(here+'/'+n,out+'/'+n);}
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');
const tools={};
for(const name of ['gprbuild','iasl','acpiexec']){
 const directories=name==='gprbuild'?process.env.PATH:(process.env.ACPICA_BIN??process.env.PATH);
 const match=directories.split(path.delimiter).map(d=>path.join(d,name)).find(p=>fs.existsSync(p));if(!match)throw Error('missing tool '+name);
 const resolved=fs.realpathSync(match);tools[name]={path:resolved,sha256:sha(resolved)};
}
fs.writeFileSync(out+'/tools.json',JSON.stringify(tools,null,2)+'\n');
const commands=[];
function run(args,label,seconds){
 const result=cp.spawnSync('timeout',['--kill-after=5s',seconds+'s',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576});
 const log=(result.stdout??'')+(result.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));
 commands.push({command:args,label,seconds,status:result.status,signal:result.signal,error:result.error?.message??null,truncated:log.length>=1048576});
 fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');console.log(label,result.status);
 if(result.status!==0||result.error||log.length>=1048576){console.log(log.slice(0,4096));throw Error('command failed '+label);}
 return log;
}
const fixtures=out+'/source/tests/acpi-region-alias';
const oracle={};
for(const revision of [1,2]){
 for(const fixture of ['aliases','invalid'])run([tools.iasl.path,'-ve','-r',String(revision),'-p',out+'/'+fixture+'-'+revision,fixtures+'/'+fixture+'.asl'],'iasl-'+fixture+'-'+revision,5);
 const log=run([tools.acpiexec.path,'-l','-di','-dr','-b','evaluate CALL; execute DV00._REG 3 1; execute DV01._REG 3 1; evaluate CALL; evaluate SPAC; evaluate CODE; execute DV00._REG 3 0; execute DV01._REG 3 0; evaluate CALL; evaluate CODE',out+'/aliases-'+revision+'.aml'],'acpica-'+revision,5);
 const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));
 if(JSON.stringify(integers)!==JSON.stringify([0,2,3,1,4,0])||/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log))throw Error('oracle mismatch');
 oracle[revision]={integers,aml_sha256:sha(out+'/aliases-'+revision+'.aml'),outstanding_cache_diagnostic:log.includes('Outstanding cache allocations')};
}
const ada=names.filter(n=>/\.ad[bs]$/.test(n)),dirs=[...new Set(ada.map(n=>path.dirname(out+'/source/'+n)))].sort();
const profiles={};
for(const profile of ['checked','release']){
 const gpr=out+'/'+profile+'.gpr';fs.writeFileSync(gpr,`project ${profile} is
 for Source_Dirs use (${dirs.map(d=>JSON.stringify(d)).join(', ')});
 for Source_Files use (${[...adaNames].sort().map(n=>JSON.stringify(n)).join(', ')});
 for Main use ("region_alias_tests.adb", "existing_region_tests.adb");
 for Object_Dir use "${profile}/obj";
 for Exec_Dir use "${profile}/bin";
 package Compiler is
 for Default_Switches ("Ada") use ("-gnat2022", "-O1", "-gnatwa", "-gnatwe", ${profile==='checked'?'"-gnata", "-gnato"':'"-gnatp"'});
 end Compiler;
end ${profile};
`);
 run([tools.gprbuild.path,'-p','-j1','-P',gpr],profile+'-build',180);
 profiles[profile]={alias_checks:0,existing_checks:0,binaries:{}};
 for(const revision of [1,2])for(const scenario of ['normal','invalid','unavailable','foreign']){
  const fixture=scenario==='invalid'?'invalid':'aliases';
  const log=run([out+'/'+profile+'/bin/region_alias_tests',out+'/'+fixture+'-'+revision+'.aml',scenario],profile+'-'+revision+'-'+scenario,30);
  const match=log.match(/^Region alias checks (\d+)\n$/);if(!match)throw Error('missing alias checks');profiles[profile].alias_checks+=Number(match[1]);
 }
 const log=run([out+'/'+profile+'/bin/existing_region_tests'],profile+'-existing',30);
 const match=log.match(/^Existing region checks (\d+)\n$/);if(!match)throw Error('missing existing checks');profiles[profile].existing_checks=Number(match[1]);
 for(const n of ['region_alias_tests','existing_region_tests'])profiles[profile].binaries[n]=sha(out+'/'+profile+'/bin/'+n);
}
if(profiles.checked.alias_checks!==172||profiles.release.alias_checks!==172||profiles.checked.existing_checks!==profiles.release.existing_checks)throw Error('profile mismatch');
for(const [n,h]of Object.entries(pins))if(sha(root+'/'+n)!==h)throw Error('source changed '+n);
const summary={profiles,oracle,commands:commands.length,inputs_unchanged:true,new_proof:false,native_validated:false,hardware_validated:false,full_goal_complete:false};
fs.writeFileSync(out+'/complete.json',JSON.stringify(summary,null,2)+'\n');console.log(JSON.stringify(summary));
