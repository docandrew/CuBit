import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';

const here=path.dirname(fileURLToPath(import.meta.url));
let root=path.resolve(here,'../..'),scopeOnly=false,withProof=false,validateOnly=false;
for(let i=2;i<process.argv.length;i++){
  const arg=process.argv[i];
  if(arg==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
  else if(arg==='--scope-only')scopeOnly=true;
  else if(arg==='--with-proof')withProof=true;
  else if(arg==='--validate-only')validateOnly=true;
  else throw Error('Usage: run.sh [--source-root DIR] [--scope-only] [--with-proof] [--validate-only]');
}
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
function regular(p){if(!fs.lstatSync(p).isFile()||fs.lstatSync(p).isSymbolicLink())throw Error('Expected regular input '+p);}
function inside(base,rel){
  if(path.isAbsolute(rel)||rel.split(/[\\/]/).includes('..'))throw Error('Invalid relative input '+rel);
  const p=path.resolve(base,rel);
  if(!p.startsWith(base+path.sep))throw Error('Input outside root '+rel);
  let parent=path.dirname(p);
  while(parent!==base){if(fs.lstatSync(parent).isSymbolicLink())throw Error('Linked input parent '+parent);parent=path.dirname(parent);}
  regular(p);return p;
}
const catalogPath=here+'/sources.json',catalog=JSON.parse(fs.readFileSync(catalogPath));
if(catalog.version!==1||!Array.isArray(catalog.inputs)||!Array.isArray(catalog.helpers))throw Error('Source catalog');
const pins={[catalogPath]:sha(catalogPath),[here+'/run.mjs']:sha(here+'/run.mjs'),[here+'/run.sh']:sha(here+'/run.sh')};
const sources=new Map(),seenPaths=new Set();
for(const[base,rows]of [[root,catalog.inputs],[here,catalog.helpers]])for(const entry of rows){
  const p=inside(base,entry.path);
  if(seenPaths.has(p)||sha(p)!==entry.sha256)throw Error('Source baseline '+entry.path);
  seenPaths.add(p);pins[p]=entry.sha256;
  if(/\.ad[bs]$/.test(p)){
    const name=path.basename(p);
    if(sources.has(name))throw Error('Ambiguous unit filename '+name);
    sources.set(name,p);
  }
}
if(validateOnly){console.log(JSON.stringify({validated:true,inputs:catalog.inputs.length,helpers:catalog.helpers.length,adaFiles:sources.size,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before entering the Nix environment');
function tool(name,mustBeNix=true){
  const candidate=process.env.PATH.split(path.delimiter).map(p=>path.join(p,name)).find(p=>fs.existsSync(p));
  if(!candidate)throw Error('Missing Nix tool '+name);
  const p=fs.realpathSync(candidate);if(mustBeNix&&!p.startsWith('/nix/store/'))throw Error('Use the Nix development environment for '+name);
  // Keep argv[0]: Nix coreutils uses applet symlinks such as timeout.
  pins[p]=sha(p);pins[candidate]=sha(candidate);return candidate;
}
const gprbuild=tool('gprbuild');
const timeout=tool('timeout'),prlimit=tool('prlimit',false);
const iasl=scopeOnly?null:tool('iasl'),acpiexec=scopeOnly?null:tool('acpiexec');
const prover=withProof?tool('gnatprove'):null;
if(withProof){tool('cvc5');tool('z3');}
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-local-fields-');
const src=out+'/source';fs.mkdirSync(src);
for(const[name,p]of sources){const target=src+'/'+name;fs.copyFileSync(p,target);pins[target]=sha(target);}
const runs=[],proofs=[];
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Source drift '+p);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({version:1,completed,root,out,scopeOnly,withProof,pins,runs,proofs,nativeExecuted:false,hardwareValidated:false,wholeInterpreterProved:false},null,2)+'\n');}
function run(label,args,seconds=30,addressSpace=24*1024**3){
  verify();const r=spawnSync(timeout,['--kill-after=2s',String(seconds),prlimit,'--as='+addressSpace,...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1024**2});
  const output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);
  runs.push({label,args,status:r.status,signal:r.signal,error:r.error?.message,output});save(false);
  if(r.status!==0||r.signal||r.error)throw Error(label+' failed; see '+out+'/'+label+'.log');
  verify();return output;
}
function project(profile,main){
  fs.mkdirSync(out+'/'+profile,{recursive:true});
  const p=out+'/'+profile+'/test.gpr',checks=profile==='checked'?['-gnata','-gnato']:['-gnatp'];
  fs.writeFileSync(p,`project Test is
 for Source_Dirs use ("../source");
 for Main use ("${main}");
 for Object_Dir use "obj";
 for Exec_Dir use "bin";
 for Excluded_Source_Files use ("acpi_target_heap.adb");
 package Naming is for Body ("ACPI_Target_Heap") use "acpi_target_heap_host.adb"; end Naming;
 package Compiler is for Default_Switches ("Ada") use ("-O2","-gnat2022",${checks.map(x=>'"'+x+'"').join(',')}); end Compiler;
end Test;
`);pins[p]=sha(p);return p;
}
console.log('Hosted ACPI local Field logs: '+out);
const cases=['normal','reentry','repeated','capacity','child-failure','zero-window','forever-window','unavailable-window','closed-admission','acquire-late','acquire-unknown','acquire-refused','contradictory-acquire','expire-read','expire-end','data-failure','cleanup-parent','release-unknown','signal-deferred','contradictory-release','unsolicited-end'];
for(const profile of ['release','checked']){
  run(profile+'-build',[gprbuild,'-q','-p','-j1','-P',project(profile,'local_scope_tests.adb')],180);
  for(const scenario of cases){
    const output=run(profile+'-'+scenario,[out+'/'+profile+'/bin/local_scope_tests',scenario]);
    if(!new RegExp('^LOCAL SCOPE '+scenario+' checks\\s+\\d+\\s*$').test(output.trim()))throw Error('Missing case marker '+scenario);
  }
}
if(!scopeOnly){
  const asl=fs.readFileSync(inside(root,'tests/acpi-native-register-startup/startup.asl'),'utf8');
  for(const mode of ['unlocked','locked'])for(const width of ['32','64']){
    const name=out+'/reference-'+mode+'-'+width;
    let text=asl.replace('"DSDT", 2,','"DSDT", '+(width==='32'?'1':'2')+',');
    if(text===asl&&width==='32')throw Error('ASL header version not replaced');
    if(mode==='locked')text=text.replaceAll('NoLock','Lock');
    fs.writeFileSync(name+'.asl',text);pins[name+'.asl']=sha(name+'.asl');
    run('iasl-'+mode+'-'+width,[iasl,'-tc','-p',name,name+'.asl']);pins[name+'.aml']=sha(name+'.aml');
    if(width==='64'){
      const unit=mode==='locked'?'Locked_Fixture':'Composed_Fixture',p=src+'/'+unit.toLowerCase()+'.ads';
      const bytes=[...fs.readFileSync(name+'.aml')].map(x=>'16#'+x.toString(16).padStart(2,'0').toUpperCase()+'#');
      fs.writeFileSync(p,'with Firmware_Tables;\npackage '+unit+' is\nData : constant Firmware_Tables.Bytes := ['+bytes.join(',')+'];\nend '+unit+';\n');pins[p]=sha(p);
    }
    const output=run('acpica-'+mode+'-'+width,[prlimit,'--cpu=5',acpiexec,'-b','evaluate RD00; evaluate DAT1; execute WRW0 0x12345678; execute PART 0x0A; execute BRW0 0xAB; execute IRW0 0xCD',name+'.aml'],5,128*1024**2);
    const values=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString(16));
    const expected=width==='32'?['42','43','7a','ab','cd']:['42','43','12345678','7a','ab','cd'];
    if(JSON.stringify(values)!==JSON.stringify(expected)||(width==='32'&&!/\[Buffer\] Length 08[^\n]*0000:\s+78 56 34 12 00 00 00 00/.test(output)))throw Error('ACPICA typed result mismatch '+mode+'-'+width);
    runs.at(-1).typedValues={integers:values,wide:width==='32'?'Buffer08:7856341200000000':'Integer:12345678'};
    runs.at(-1).referenceDiagnostics=output.split('\n').filter(x=>/Error|Warning|Outstanding.*allocation/i.test(x));save(false);
  }
  run('core-build',[gprbuild,'-q','-p','-j1','-P',project('core-release','local_field_core_tests.adb')],180);
  for(const mode of ['unlocked','locked'])for(const width of ['32','64']){
    const output=run('core-'+mode+'-'+width,[out+'/core-release/bin/local_field_core_tests',width,mode]);
    if(!/^LOCAL FIELD CORE width=(32|64) mode=(unlocked|locked) checks\s+\d+\s*$/.test(output.trim()))throw Error('Missing Core marker');
  }
}
if(withProof){
  const p=out+'/proof.gpr';fs.writeFileSync(p,'project Proof is for Source_Dirs use("source");for Object_Dir use "proof-obj";for Excluded_Source_Files use("acpi_target_heap.adb");package Naming is for Body("ACPI_Target_Heap") use "acpi_target_heap_host.adb";end Naming;package Compiler is for Default_Switches("Ada") use("-gnat2022");end Compiler;end Proof;\n');pins[p]=sha(p);
  for(const level of [1,2]){
    run('proof-L'+level,[prover,'-P',p,'-u','local_scope_proof.adb','aml_field_layout.adb','aml_register_views.adb','aml_field_plans.adb','--mode=prove','--level='+level,'--timeout=10','--prover=cvc5,z3','--report=all','-j1'],180);
    const files=[];
    for(const name of ['local_scope_proof','aml_field_layout','aml_register_views','aml_field_plans']){
      const f=out+'/proof-obj/gnatprove/'+name+'.spark',s=JSON.parse(fs.readFileSync(f));
      const skipped=(s.skip_flow_proof?.length??0)+(s.skip_proof?.length??0),assume=s.pragma_assume?.length??0;
      if(s.progress!=='PROGRESS_PROOF'||s.stop_reason!=='STOP_REASON_NONE'||skipped||assume||[...s.flow,...s.proof].some(x=>x.severity!=='info'))throw Error('Unresolved selected proof '+name);
      const copy=out+'/L'+level+'-'+name+'.spark';fs.copyFileSync(f,copy);pins[copy]=sha(copy);
      files.push({unit:name,sha256:sha(f),proof:s.proof.length,flow:s.flow.length,skipped,assume});
    }
    proofs.push({level,files});save(false);
  }
}
verify();save(true);
console.log('Hosted ACPI local Field checks passed: '+JSON.stringify({smallCases:cases.length,profiles:2,coreVariants:scopeOnly?0:4,acpicaTypedResults:scopeOnly?0:24,proofLevels:proofs.length,result:out+'/RESULT.json',nativeExecuted:false}));
