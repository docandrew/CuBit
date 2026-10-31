import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{fileURLToPath}from'node:url';
export function audit(s){
 if(Object.keys(s.spark??{}).length!==3||!Object.values(s.spark).every(x=>x==='all'))throw Error('SPARK coverage');
 if(!Array.isArray(s.skip_proof)||!Array.isArray(s.skip_flow_proof)||s.skip_proof.length||s.skip_flow_proof.length)throw Error('Skipped analysis');
 if(s.flow?.length!==5||!s.flow.every(x=>x.severity==='info'))throw Error('Flow');
 if(s.proof?.length!==16||s.proof.filter(x=>x.rule==='VC_POSTCONDITION').length!==1||!s.proof.every(x=>x.severity==='info'&&x.how_proved==='prover'&&x.check_tree?.length&&x.check_tree.every(t=>Object.values(t.proof_attempts??{}).some(a=>a.result==='Valid'))))throw Error('Proof results');
}
export function check(root,here){
 const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex'),r=JSON.parse(fs.readFileSync(here+'/EVIDENCE.json'));
 if(r.version!==1||Object.keys(r.sources??{}).length!==26||r.results?.length!==2||Object.keys(r.sources).some(p=>p.includes('acpi_service_core')))throw Error('Source scope');
 for(const[rel,h]of Object.entries(r.sources)){const f=path.resolve(root,rel);if(!f.startsWith(root+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Source pin '+rel);}
 for(const[rel,h]of Object.entries(r.artifacts)){const f=path.resolve(here,rel);if(!f.startsWith(here+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Artifact pin '+rel);}
 const original=JSON.parse(fs.readFileSync(here+'/original-report.json'));
 const copies=Object.fromEntries(Object.entries(original.inputs).filter(([f])=>f.includes('/source/')).map(([f,h])=>[path.basename(f),h]));
 if(Object.keys(copies).length!==26||!Object.entries(r.sources).every(([f,h])=>copies[path.basename(f)]===h))throw Error('Snapshot linkage');
 const seen=new Set();for(const row of r.results){
  if(![1,2].includes(row.level)||seen.has(row.level)||!row.accepted||row.status!==0||row.signal||row.error||!r.artifacts[row.spark])throw Error('Acceptance');seen.add(row.level);
  if(!['--mode=prove','-j1','--report=all','--level='+row.level,'--prover=cvc5,z3','--timeout=5'].every(x=>row.args.includes(x))||row.args[row.args.indexOf('-u')+1]!=='acpi_ec_boot_resources.adb')throw Error('Command');
  const originalRow=original.results.find(x=>x.level===row.level);if(!originalRow||JSON.stringify(originalRow.args)!==JSON.stringify(row.args))throw Error('Original command linkage');
  const rel='level'+row.level+'/original-proof.gpr',project=row.args[row.args.indexOf('-P')+1],text=fs.readFileSync(here+'/'+rel,'utf8');
  if(original.inputs[project]!==r.artifacts[rel]||!['-gnat2022','-gnatwa','-gnatwe','-gnata','-gnato'].every(x=>text.includes(JSON.stringify(x)))||text.includes('-gnatp'))throw Error('Project switches');
  audit(JSON.parse(fs.readFileSync(here+'/'+row.spark)));
 }
 return {levels:[1,2],entities:3,flow_per_level:5,checks_per_level:16,posts_per_level:1,executed:false,scope:r.scope};
}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url)){
 const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..');if(process.argv.length>2){if(process.argv.length!==4||process.argv[2]!=='--source-root')throw Error('Usage: check-proof.mjs [--source-root DIR]');root=path.resolve(process.argv[3]);}console.log(JSON.stringify(check(root,here)));
}
