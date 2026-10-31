import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {fileURLToPath} from 'node:url';
export function audit(s){
 const selected=new Set(['ACPI_FADT.Sleep_Registers.Describe','ACPI_FADT.Sleep_Registers.Descriptions_Agree','ACPI_FADT.Sleep_Registers']);
 if(Object.keys(s.spark??{}).length!==selected.size||Object.entries(s.spark).some(([id,v])=>v!=='all'||!selected.delete(s.entities?.[id]?.name))||selected.size)throw Error('SPARK coverage');
 if(s.stop_reason!=='STOP_REASON_NONE'||s.progress!=='PROGRESS_PROOF'||!Array.isArray(s.pragma_assume)||s.pragma_assume.length||!Array.isArray(s.skip_proof)||!Array.isArray(s.skip_flow_proof)||s.skip_proof.length||s.skip_flow_proof.length)throw Error('Assume/skipped/incomplete analysis');
 if(s.flow?.length!==7||s.flow.some(r=>r.severity!=='info'))throw Error('Flow');
 if(s.proof?.length!==7||s.proof.filter(r=>r.rule==='VC_POSTCONDITION').length!==1||s.proof.some(r=>r.severity!=='info'||r.how_proved!=='prover'||!r.check_tree?.length||r.check_tree.some(t=>!Object.values(t.proof_attempts??{}).some(a=>a.result==='Valid'))))throw Error('Proof');
 const entities=Object.fromEntries(Object.entries(s.entities??{}).map(([id,value])=>[id.trim(),value]));
 const allowed=new Set(['ACPI_FADT.Registers.Describe','ACPI_FADT.Hardware_Reduced','ACPI_FADT.Sleep_Registers.Descriptions_Agree']);
 for(const row of s.assumptions??[]){if(!entities[row.claim?.arg]||row.assumptions.some(a=>!allowed.has(entities[a.arg]?.name)||!['CLAIM_POST','CLAIM_EFFECTS','CLAIM_AORTE'].includes(a.predicate)))throw Error('Unreviewed modular dependency');}
 return {entities:3,flow:7,checks:7,posts:1};
}
export function check(root,here){
 const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex'),r=JSON.parse(fs.readFileSync(here+'/EVIDENCE.json'));
 if(r.version!==1||Object.keys(r.sources??{}).length!==8||r.results?.length!==2||Object.keys(r.sources).some(p=>p.includes('acpi_service_core')))throw Error('Source scope');
 for(const [rel,h]of Object.entries(r.sources)){const f=path.resolve(root,rel);if(!f.startsWith(root+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h||/pragma\s+Assume\b/i.test(fs.readFileSync(f,'utf8')))throw Error('Source pin/Assume '+rel);}
 for(const [rel,h]of Object.entries(r.artifacts)){const f=path.resolve(here,rel);if(!f.startsWith(here+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Artifact pin '+rel);}
 const original=JSON.parse(fs.readFileSync(here+'/original-report.json'));if(!original.accepted)throw Error('Original acceptance');
 const copies=Object.fromEntries(Object.entries(original.inputs).filter(([f])=>f.includes('/source/')).map(([f,h])=>[path.basename(f),h]));if(Object.keys(copies).length!==8||Object.entries(r.sources).some(([f,h])=>copies[path.basename(f)]!==h))throw Error('Snapshot linkage');
 const seen=new Set();for(const row of r.results){if(![1,2].includes(row.level)||seen.has(row.level)||!row.accepted||row.status!==0||row.signal||row.error||!r.artifacts[row.spark])throw Error('Acceptance');seen.add(row.level);
 if(!['--mode=prove','-j1','--report=all','--level='+row.level,'--prover=cvc5,z3','--timeout=5'].every(x=>row.args.includes(x))||row.args[row.args.indexOf('-u')+1]!=='acpi_fadt-sleep_registers.adb')throw Error('Command');
 const old=original.commands.find(x=>x.level===row.level);if(!old||JSON.stringify(old)!==JSON.stringify(Object.fromEntries(Object.entries(row).filter(([k])=>!['accepted','spark'].includes(k)))))throw Error('Original command linkage');
 const project=row.args[row.args.indexOf('-P')+1],rel='level'+row.level+'/original-proof.gpr',text=fs.readFileSync(here+'/'+rel,'utf8');if(original.inputs[project]!==r.artifacts[rel]||!['-gnat2022','-gnatwa','-gnatwe','-gnata','-gnato'].every(x=>text.includes(JSON.stringify(x)))||text.includes('-gnatp'))throw Error('Project');audit(JSON.parse(fs.readFileSync(here+'/'+row.spark)));}
 return {levels:[1,2],entities:3,flow_per_level:7,checks_per_level:7,posts_per_level:1,executed:false,scope:r.scope};
}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url)){const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..');if(process.argv.length>2){if(process.argv.length!==4||process.argv[2]!=='--source-root')throw Error('Usage: check-proof.mjs [--source-root DIR]');root=path.resolve(process.argv[3]);}console.log(JSON.stringify(check(root,here)));}
