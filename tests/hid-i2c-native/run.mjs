import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';

const runnerPath=fileURLToPath(import.meta.url), here=path.dirname(runnerPath);
let root=path.resolve(here,'../..'), validateOnly=false, proveOnly=false;
for(let i=2;i<process.argv.length;i++) {
  if(process.argv[i]==='--validate-only') validateOnly=true;
  else if(process.argv[i]==='--prove-only') proveOnly=true;
  else if(process.argv[i]==='--source-root' && process.argv[i+1]) root=path.resolve(process.argv[++i]);
  else throw Error('Usage: run.sh [--source-root DIR] [--validate-only | --prove-only]');
}
if(validateOnly && proveOnly) throw Error('Select one validation mode');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const catalogPath=path.join(here,'sources.json'), catalog=JSON.parse(fs.readFileSync(catalogPath));
if(catalog.version!==1) throw Error('Unsupported source catalog');
function rooted(relative) {
  const p=path.resolve(root,relative);
  if(!p.startsWith(root+path.sep)) throw Error('Source outside selected root');
  return p;
}
const pins={[catalogPath]:sha(catalogPath)}, files=[], names=new Set();
for(const entry of catalog.inputs) {
  const p=rooted(entry.path), name=path.basename(p);
  if(!/\.ad[bs]$/.test(name) || names.has(name)) throw Error('Invalid/duplicate Ada source '+name);
  names.add(name);
  if(sha(p)!==entry.sha256) throw Error('Source baseline changed: '+entry.path);
  pins[p]=entry.sha256; files.push({path:p,name,kind:entry.kind});
}
const provenancePath=path.join(here,'generated/provenance.json');
const provenance=JSON.parse(fs.readFileSync(provenancePath)); pins[provenancePath]=sha(provenancePath);
const manifest=rooted(provenance.manifest_path);
if(sha(manifest)!==provenance.manifest_sha256) throw Error('Regenerate parameter fixtures for changed manifest');
pins[manifest]=sha(manifest);
const schema=rooted('userspace/ccl/interfaces/executable-manifest.ccl');
if(sha(schema)!==provenance.schema_sha256) throw Error('Revalidate parameter fixtures for changed manifest schema');
pins[schema]=sha(schema);
const descriptorPath=path.join(here,'generated/descriptor.bin'), descriptor=fs.readFileSync(descriptorPath);
if(sha(descriptorPath)!==provenance.descriptor_sha256 || descriptor.subarray(0,4).toString()!=='PRMS' || descriptor.readUInt16LE(4)!==1)
  throw Error('Invalid generated descriptor fixture');
const rootLength=descriptor[40], signature=descriptor.subarray(41+rootLength);
if(descriptor.subarray(41,41+rootLength).toString()!==provenance.descriptor_root || signature.length!==descriptor.readUInt16LE(6))
  throw Error('Invalid descriptor shape');
const digest=crypto.createHash('sha256').update(signature).digest();
if(!digest.equals(descriptor.subarray(8,40)) || digest.toString('hex')!==provenance.descriptor_identity)
  throw Error('Generated signature identity mismatch');
const parameterSpec=files.find(f=>f.kind==='generated' && f.name==='parameters.ads');
if(!parameterSpec) throw Error('Generated parameter spec missing');
const identity=fs.readFileSync(parameterSpec.path,'utf8').split('Identity : constant CuBit.Parameter_Signatures.Identity :=')[1]?.split('];')[0];
if(!identity) throw Error('Generated parameter identity missing');
const declared=Buffer.from([...identity.matchAll(/16#([0-9A-F]{2})#/g)].map(m=>parseInt(m[1],16)));
if(!declared.equals(digest)) throw Error('Generated decoder/descriptor identity mismatch');
pins[descriptorPath]=sha(descriptorPath); pins[runnerPath]=sha(runnerPath);
if(validateOnly) {
  process.stdout.write(JSON.stringify({status:'metadata-and-fixtures-valid',sources:files.length,functional_tests_run:false})+'\n');
  process.exit(0);
}
if(!process.env.TMPDIR || !path.isAbsolute(process.env.TMPDIR)) throw Error('Set an absolute TMPDIR for private test outputs');
const out=fs.mkdtempSync(path.join(process.env.TMPDIR,'hid-i2c-native-'));
fs.mkdirSync(path.join(out,'source'));
for(const item of files) fs.copyFileSync(item.path,path.join(out,'source',item.name));
fs.writeFileSync(path.join(out,'inputs.json'),JSON.stringify(pins,null,2)+'\n');
process.stdout.write(out+'\n');
const buildSeconds=180, testSeconds=30, logBytes=1024*1024;
const commands=[],counts={};
if(proveOnly) {
  const selected=catalog.proof_inputs.map(relative=> {
    const entry=catalog.inputs.find(e=>e.path===relative);
    if(!entry || !['production','runtime-pure'].includes(entry.kind)) throw Error('Invalid proof source');
    return path.basename(relative);
  });
  fs.writeFileSync(path.join(out,'proof.gpr'),`project Proof is\n for Source_Dirs use ("source");\n for Source_Files use (${selected.map(n=>JSON.stringify(n)).join(',')});\n for Object_Dir use "proof/obj";\n package Compiler is\n for Default_Switches ("Ada") use ("-gnat2022","-gnata","-gnato");\n end Compiler;\nend Proof;\n`);
  for(const mode of ['flow','prove']) {
    const args=['gnatprove','-P','proof.gpr','-u','hid_startup_readiness.adb','--mode='+mode,'-j1','--checks-as-errors=on','--report=all',...(mode==='prove'?['--level=1','--timeout=5']:[])];
    const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(buildSeconds),...args],
      {cwd:out,encoding:'utf8',maxBuffer:logBytes,timeout:(buildSeconds+3)*1000});
    const log=(r.stdout??'')+(r.stderr??'');
    fs.writeFileSync(path.join(out,'proof-'+mode+'.log'),log.slice(0,logBytes));
    commands.push({mode,args,status:r.status,signal:r.signal,error:r.error?.message??null});
    fs.writeFileSync(path.join(out,'commands.json'),JSON.stringify(commands,null,2)+'\n');
    if(r.status!==0 || r.error || Buffer.byteLength(log)>logBytes) throw Error('Proof failed '+mode+'; see '+out);
  }
  const evidencePath=path.join(out,'proof/obj/gnatprove/hid_startup_readiness.spark');
  const evidence=JSON.parse(fs.readFileSync(evidencePath));
  if(evidence.progress!=='PROGRESS_PROOF' || evidence.stop_reason!=='STOP_REASON_NONE' ||
    !Object.keys(evidence.spark).length || Object.values(evidence.spark).some(s=>s!=='all') ||
    evidence.pragma_assume.length || evidence.skip_flow_proof.length || evidence.skip_proof.length ||
    evidence.flow.some(c=>c.severity!=='info') || evidence.proof.length!==5 ||
    evidence.proof.some(c=>c.severity!=='info' || !c.check_tree.length || c.check_tree.some(t=>
      !Object.values(t.proof_attempts).some(a=>a.result==='Valid')))) throw Error('Incomplete policy proof evidence');
  const ids=new Set(Object.keys(evidence.entities).map(k=>Number(k)));
  for(const item of evidence.assumptions) for(const claim of [item.claim,...item.assumptions])
    if(!ids.has(claim.arg)) throw Error('External proof dependency needs audit');
  for(const [p,hash] of Object.entries(pins)) if(sha(p)!==hash) throw Error('Source changed during proof: '+p);
  const summary={unit:'HID_Startup_Readiness',level:1,flow_checks:evidence.flow.length,
    functional_contracts:evidence.proof.length,prover_leaves:evidence.proof.reduce((n,c)=>n+c.check_tree.length,0),
    entities:Object.keys(evidence.spark).length,pragma_assume:0,skipped:0,
    source_files:selected,external_claim_dependencies:0,native_executed:false,hardware_validated:false};
  fs.writeFileSync(path.join(out,'COMPLETE.json'),JSON.stringify({proof:summary,commands:commands.length,main_executed:false},null,2)+'\n');
  process.stdout.write('Policy proof complete '+JSON.stringify(summary)+'\n');
  process.exit(0);
}
for(const profile of ['checked','release']) {
  const flags=['-gnat2022','-gnatwa','-gnatwe','-gnatwJ',...(profile==='checked'?['-gnata','-gnato','-O2']:['-gnatp','-O3'])];
  const mains=[...new Set(catalog.tests.map(t=>t.main))];
  const project=`project Suite is\n for Source_Dirs use ("source");\n for Main use (${mains.map(n=>JSON.stringify(n+'.adb')).join(',')});\n for Object_Dir use "${profile}/obj";\n for Exec_Dir use "${profile}/bin";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(x=>JSON.stringify(x)).join(',')});\n end Compiler;\nend Suite;\n`;
  fs.writeFileSync(path.join(out,'suite.gpr'),project);
  for(const test of [null,...catalog.tests]) {
    const phase=test?.id??test?.main??'build', seconds=test?testSeconds:buildSeconds;
    const args=test?[path.join(out,profile,'bin',test.main),...(test.args??[])]:['gprbuild','-p','-j1','-P','suite.gpr'];
    const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],
      {cwd:out,encoding:'utf8',maxBuffer:logBytes,timeout:(seconds+3)*1000});
    const log=(r.stdout??'')+(r.stderr??'');
    fs.writeFileSync(path.join(out,profile+'-'+phase+'.log'),log.slice(0,logBytes));
    commands.push({profile,phase,args,status:r.status,signal:r.signal,error:r.error?.message??null});
    fs.writeFileSync(path.join(out,'commands.json'),JSON.stringify(commands,null,2)+'\n');
    if(r.status!==0 || r.error || Buffer.byteLength(log)>logBytes) throw Error('Failed '+profile+' '+phase+'; see '+out);
    if(test) {
      const prefix=test.prefix.replace(/[.*+?^${}()|[\]\\]/g,'\\$&');
      const match=new RegExp('^'+prefix+' (\\d+)\\s*$').exec(log);
      if(!match || Number(match[1])===0) throw Error('Unexpected test output: '+phase);
      (counts[profile]??={})[phase]=Number(match[1]);
    }
  }
}
for(const [p,hash] of Object.entries(pins)) if(sha(p)!==hash) throw Error('Source changed during tests: '+p);
fs.writeFileSync(path.join(out,'COMPLETE.json'),JSON.stringify({counts,commands:commands.length,sources:files.length,
  hosted:true,main_executed:false,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
process.stdout.write('Complete '+JSON.stringify(counts)+'\n');
