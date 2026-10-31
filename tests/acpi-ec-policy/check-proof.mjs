import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {fileURLToPath} from 'node:url';
export function audit(s){
 if(Object.keys(s.spark??{}).length!==13||!Object.values(s.spark).every(x=>x==='all'))throw Error('SPARK coverage');
 if(!Array.isArray(s.skip_proof)||!Array.isArray(s.skip_flow_proof)||s.skip_proof.length||s.skip_flow_proof.length)throw Error('Skipped analysis');
 if(s.flow?.length!==11||!s.flow.every(x=>x.severity==='info'))throw Error('Flow results');
 if(s.proof?.length!==10||s.proof.filter(x=>x.rule==='VC_POSTCONDITION').length!==5||!s.proof.every(x=>x.severity==='info'&&x.how_proved==='prover'&&x.check_tree?.length&&x.check_tree.every(t=>Object.values(t.proof_attempts??{}).some(a=>a.result==='Valid'))))throw Error('Proof results');
}
export function check(root,here){
 const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex'),record=JSON.parse(fs.readFileSync(here+'/EVIDENCE.json'));
 if(record.version!==1||Object.keys(record.sources??{}).length!==6||record.results?.length!==2)throw Error('Evidence coverage');
 for(const[rel,h]of Object.entries(record.sources)){const f=path.resolve(root,rel);if(!f.startsWith(root+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Source pin '+rel);}
 for(const[rel,h]of Object.entries(record.artifacts)){const f=path.resolve(here,rel);if(!f.startsWith(here+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Artifact pin '+rel);}
 const original=JSON.parse(fs.readFileSync(here+'/original-report.json'));
 const snapshots=Object.fromEntries(Object.entries(original.inputs).filter(([f])=>f.includes('/source/')).map(([f,h])=>[path.basename(f),h]));
 if(Object.keys(snapshots).length!==6||!Object.entries(record.sources).every(([f,h])=>snapshots[path.basename(f)]===h))throw Error('Captured source linkage');
 for(const level of [1,2]){const rel='level'+level+'/original-proof.gpr',text=fs.readFileSync(here+'/'+rel,'utf8'),command=original.results.find(r=>r.level===level)?.args??[],project=command[command.indexOf('-P')+1];if(!project||original.inputs[project]!==record.artifacts[rel]||!['-gnat2022','-gnatwa','-gnatwe','-gnata','-gnato'].every(x=>text.includes(JSON.stringify(x)))||text.includes('-gnatp'))throw Error('Proof recipe provenance');}
 const seen=new Set();for(const r of record.results){if(![1,2].includes(r.level)||seen.has(r.level)||!r.accepted||r.status!==0||r.signal||r.error||!record.artifacts[r.spark])throw Error('Selection');seen.add(r.level);if(!['--mode=prove','-j1','--report=all','--level='+r.level,'--prover=cvc5,z3','--timeout=5'].every(x=>r.args.includes(x))||r.args[r.args.indexOf('-u')+1]!=='acpi_ec_event_policy.adb')throw Error('Command');audit(JSON.parse(fs.readFileSync(here+'/'+r.spark)));}
 return {levels:[1,2],checks_per_level:10,posts_per_level:5,executed:false,scope:record.scope};
}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url)){
 const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..');if(process.argv.length>2){if(process.argv.length!==4||process.argv[2]!=='--source-root')throw Error('Usage: check-proof.mjs [--source-root DIR]');root=path.resolve(process.argv[3]);}console.log(JSON.stringify(check(root,here)));
}
