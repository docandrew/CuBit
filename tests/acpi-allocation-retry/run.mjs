import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const here=import.meta.dirname,repo=path.resolve(here,'../..'),sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),meta=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={};
for(const r of meta.production)pins[repo+'/'+r.path]=r.sha256;
for(const r of meta.fixtures)pins[here+'/fixtures/'+r.name]=r.sha256;
for(const p of [import.meta.filename,here+'/sources.json'])pins[p]=sha(p);
const argv=process.argv.slice(2);let capture;
if(argv.length===1&&argv[0]==='--validate-only'){}else if(argv.length===2&&argv[0]==='--capture-dir')capture=path.resolve(argv[1]);else if(argv.length!==0)throw Error('Use [--validate-only | --capture-dir DIRECTORY]');
if(capture)for(const r of meta.captured)pins[capture+'/'+r.name]=r.sha256;
const verify=()=>{for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('source/input drift '+p);};verify();
if(argv[0]==='--validate-only'){console.log('ACPI allocation-retry source validation passed '+meta.production.length+' production/'+meta.fixtures.length+' fixtures.');process.exit(0);}
if(!process.env.TMPDIR)throw Error('Set TMPDIR before Nix');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'acpi-allocation-retry-')),commands=[];
function run(label,args,limit=30){verify();const start=Date.now(),r=spawnSync('timeout',['--kill-after=2s',String(limit),...args],{cwd:out,encoding:'utf8',timeout:(limit+3)*1000,maxBuffer:1048576});const output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);verify();commands.push({label,status:r.status,wall_ms:Date.now()-start});write(false);if(r.status!==0)throw Error(label+' see '+out);return output;}
function write(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,pins,commands,captured_replay:!!capture,new_proof:false,native_executed:false,scope:'Actual release-hosted collecting arena/owner/entry/reclamation regressions, small helper checked/release and independent ACPICA fixture controls. Optional captured replay uses modeled hardware and checks exact ordered transcript, fuel/outcomes and zero collections. No whole interpreter proof or native performance claim.'},null,2)+'\n');}
const mains=['pressure_tests.adb','collecting_owner_tests.adb','collecting_entry_tests.adb','collecting_reclamation_tests.adb',...(capture?['reproduce.adb']:[])];
const gp=out+'/aml.gpr';fs.writeFileSync(gp,`project AML is for Source_Dirs use ("${here}/fixtures/aml");for Main use (${mains.map(n=>JSON.stringify(n)).join(',')});for Object_Dir use "aml/obj";for Exec_Dir use "aml/bin";package Compiler is for Default_Switches ("Ada") use ("-O1","-gnat2022","-gnatwa","-gnatwe","-gnatp");end Compiler;end AML;`);
run('aml-build',['gprbuild','-q','-p','-j1','-P',gp],180);
for(const n of mains.filter(n=>n!=='reproduce.adb'))run(n.replace('.adb',''),[out+'/aml/bin/'+n.replace('.adb','')]);
for(const mode of ['checked','release']){
 const gp=out+'/'+mode+'.gpr';fs.writeFileSync(gp,`project ${mode} is for Source_Dirs use ("${here}/fixtures/retry");for Main use ("retry_tests.adb");for Object_Dir use "${mode}/obj";for Exec_Dir use "${mode}/bin";package Compiler is for Default_Switches ("Ada") use ("-O1","-gnat2022","-gnatwa","-gnatwe",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;end ${mode};`);
 run(mode+'-build',['gprbuild','-q','-p','-j1','-P',gp],180);run(mode+'-retry',[out+'/'+mode+'/bin/retry_tests']);
}
for(const rev of [1,2]){
 const name='pressure-'+rev;run(name+'-compile',['iasl','-p',out+'/'+name,here+'/fixtures/'+name+'.asl']);
 const result=run(name+'-reference',['acpiexec','-b','execute TEMP; execute BUFF; execute QUOT',out+'/'+name+'.aml']);
 for(const p of [/\[Integer\] = 0*1\b/,/\[Buffer\] Length 01/,/\[Package\] Contains 1 Elements/,/\[Integer\] = 0*55\b/])if(!p.test(result))throw Error('ACPICA reference missing '+p);
 if(/AE_AML_|AE_ERROR|Exception:/.test(result))throw Error('ACPICA reference execution error');
}
if(capture)for(const branch of ['0','4']){
 const log=run('captured-'+branch,[out+'/aml/bin/reproduce',capture+'/dsdt.dat',branch,branch==='0'?'0':'1','pci',...meta.captured.slice(1).map(t=>capture+'/'+t.name)]),lines=log.split('\n'),e=meta.expected[branch];
 for(const s of [e.result,e.model,e.field,'COLLECTIONS attempted= 0 completed= 0'])if(!lines.includes(s))throw Error('captured branch '+branch+' mismatch '+s);
 const digest=crypto.createHash('sha256').update(lines.filter(s=>s.startsWith('MODEL_')).join('\n')).digest('hex');if(digest!==e.transcript_sha256)throw Error('modeled hardware transcript changed');
}
write(true);console.log('ACPI allocation-retry '+commands.length+' commands passed; '+out+'/RESULT.json');
