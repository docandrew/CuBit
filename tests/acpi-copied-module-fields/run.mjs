import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=path.dirname(import.meta.filename),root=path.resolve(here,'../..');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const check=(ok,why)=>{if(!ok)throw Error(why)};
const validateOnly=process.argv.length===3&&process.argv[2]==='--validate-only';
check(process.argv.length===2||validateOnly,'Usage: run.sh [--validate-only]');
const catalog=here+'/sources.json',manifest=JSON.parse(fs.readFileSync(catalog));
check(manifest.version===1&&manifest.expected_alis===81,'source schema');
const pins={[catalog]:sha(catalog)},names=new Set();
for(const group of ['sources','fixtures','scripts'])for(const [rel,h] of Object.entries(manifest[group])){
 check(!path.isAbsolute(rel)&&!rel.split('/').includes('..'),'unsafe input');
 const p=root+'/'+rel;check(fs.realpathSync(p)===p&&sha(p)===h,'input drift '+rel);pins[p]=h;
 if(group==='sources'){const name=path.basename(rel);check(/\.ad[bs]$/.test(name)&&!names.has(name),'duplicate/non-Ada input');names.add(name)}
}
if(validateOnly){console.log(JSON.stringify({source_manifest_valid:true,ada_sources:names.size,runtime_executed:false}));process.exit(0)}
check(process.env.TMPDIR==='/home/doc/cubit-build-tmp','Set TMPDIR before Nix');
const tools={};for(const name of ['gprbuild','iasl','acpiexec']){
 const selected=process.env.PATH.split(':').map(d=>d+'/'+name).find(p=>fs.existsSync(p));check(selected,'missing tool '+name);
 const p=fs.realpathSync(selected);check(p.startsWith('/nix/store/')&&sha(p)===manifest.tools[name],'tool drift '+name);tools[name]=p;pins[p]=sha(p);
}
pins[process.execPath]=sha(process.execPath);
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-copied-module-fields-'));fs.mkdirSync(out+'/source');console.log(out);
for(const rel of Object.keys(manifest.sources)){const dest=out+'/source/'+path.basename(rel);fs.copyFileSync(root+'/'+rel,dest);pins[dest]=sha(dest)}
const commands=[];
function verify(){for(const[p,h]of Object.entries(pins))check(sha(p)===h,'input changed '+p)}
function run(label,args,seconds){
 verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
 const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log);
 commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/COMMANDS.json',JSON.stringify(commands,null,2)+'\n');
 verify();check(r.status===0&&!r.signal&&!r.error&&Buffer.byteLength(log)<=1048576,'failed '+label+'; '+out);return log;
}
for(const stem of ['boot1','boot2','copy','duplicate-ref','duplicate-table']){
 const log=run(stem+'-compile',[tools.iasl,'-oa','-p',out+'/'+stem,here+'/fixtures/'+stem+'.asl'],10);
 check(log.includes('Compilation successful. 0 Errors, 0 Warnings, 0 Remarks'),'compiler diagnostics '+stem);
 const aml=out+'/'+stem+'.aml';check(sha(aml)===manifest.compiled_fixtures[stem],'compiled fixture drift '+stem);pins[aml]=sha(aml);
}
const reference=run('duplicate-reference',[tools.acpiexec,'-di','-dr','-b','execute DUPL; execute MARK',out+'/duplicate-ref.aml',out+'/duplicate-table.aml'],30);
check(reference.includes('Evaluation of \\DUPL failed with status AE_ALREADY_EXISTS')&&!reference.includes('Evaluation of \\DUPL returned object'),'duplicate reference');
check(reference.includes('[Integer] = 000000000000005A')&&reference.includes('4 (0x4) Outstanding cache allocations'),'reference marker/diagnostics');
const results={},alis={};
for(const profile of ['release','checked']){
 const flags=['-O1','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])],project=out+'/'+profile+'.gpr';
 fs.writeFileSync(project,`project ${profile} is\n for Source_Dirs use ("source");\n for Object_Dir use "${profile}/obj";\n for Exec_Dir use "${profile}/bin";\n for Main use ("module_copied_field_tests.adb");\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(s=>JSON.stringify(s)).join(',')});\n end Compiler;\nend ${profile};\n`);pins[project]=sha(project);
 run(profile+'-build',[tools.gprbuild,'-p','-j1','-P',project],180);results[profile]={};
 for(const [mode,count]of Object.entries(manifest.cases)){
  const log=run(profile+'-'+mode,[out+'/'+profile+'/bin/module_copied_field_tests',out+'/boot1.aml',out+'/boot2.aml',mode,out+'/copy.aml'],30);
  const m=/^Module copied field checks\s+(\d+)\s*$/.exec(log);check(m&&Number(m[1])===count,'wrong verdict '+mode);results[profile][mode]=Number(m[1]);
 }
 const units=fs.readdirSync(out+'/'+profile+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));check(units.length===manifest.expected_alis,'compiled unit inventory');alis[profile]=units.length;
 for(const n of units){const s=fs.readFileSync(out+'/'+profile+'/obj/'+n,'utf8').split('\n');for(const f of flags.filter(f=>f.startsWith('-gnat')))check(s.includes('A '+f),'missing profile flag '+n);check(!s.includes('A -gnatc')&&(profile==='release'?!s.includes('A -gnata'):!s.includes('A -gnatp')),'unexpected profile '+n)}
 console.log(profile+' joined cases passed');
}
check(JSON.stringify(results.release)===JSON.stringify(results.checked),'profile disagreement');verify();
const checks=Object.values(results).flatMap(Object.values).reduce((a,b)=>a+b,0);check(checks===152,'total count');
fs.writeFileSync(out+'/INPUTS.json',JSON.stringify(pins,null,2)+'\n');fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({checks,processes:8,results,alis,commands:commands.length,reference:{duplicate_status:'AE_ALREADY_EXISTS',marker:90,outstanding_cache_allocations:4,scope:'Separate ordinary duplicate-load reference, not hardware/startup equivalence'},new_proof:false,native_executed:false,hardware_validated:false,scope:'Actual private merged Core/AML copied LoadTable during controlled module Field startup; modeled provider/rights/uncertainty'},null,2)+'\n');console.log(JSON.stringify({checks,processes:8,alis}));
