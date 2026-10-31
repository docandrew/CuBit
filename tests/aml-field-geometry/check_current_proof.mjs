import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
export function auditResult(s,expected){
 const reject=reason=>{throw Error(reason);};
 if(Object.keys(s.spark??{}).length!==expected.entities||!Object.values(s.spark).every(x=>x==='all'))reject('SPARK coverage');
 if(!Array.isArray(s.skip_proof)||!Array.isArray(s.skip_flow_proof)||s.skip_proof.length||s.skip_flow_proof.length)reject('Skipped analysis');
 if(s.flow?.length!==expected.flow||!s.flow.every(x=>x.severity==='info'))reject('Flow results');
 const proved=t=>Object.values(t.proof_attempts??{}).some(a=>a.result==='Valid');
 if(s.proof?.length!==expected.proof||!s.proof.every(x=>x.severity==='info'&&x.how_proved==='prover'&&x.check_tree?.length&&x.check_tree.every(proved)))reject('Proof results');
 if(s.proof.filter(x=>x.rule==='VC_POSTCONDITION').length!==expected.posts)reject('Postcondition coverage');
}
export function check(root,recordPath){
 const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
 const record=JSON.parse(fs.readFileSync(recordPath));
 if(record.schema!==2||record.source_count!==14||record.results?.length!==4)throw Error('Proof record');
 function verifyPins(pins){
  for(const[rel,h]of Object.entries(pins)){
   const f=path.resolve(root,rel);
   if(!f.startsWith(root+path.sep)||fs.realpathSync(f)!==f||sha(f)!==h)throw Error('Proof pin '+rel);
  }
 }
 if(Object.keys(record.source_sha256??{}).length!==record.source_count)throw Error('Source coverage');
 verifyPins(record.source_sha256);verifyPins(record.retained_sha256);
 const commands=JSON.parse(fs.readFileSync(root+'/'+record.commands));
 const seen=new Set();
 for(const r of record.results){
  const key=r.unit+':'+r.level;if(seen.has(key))throw Error('Duplicate proof selection');seen.add(key);
  const expected=r.unit==='aml_field_layout'?{entities:2,flow:4,proof:7,posts:1}:r.unit==='aml_field_plans'?{entities:10,flow:11,proof:8,posts:2}:null;
  if(!expected||![1,2].includes(r.level)||!record.retained_sha256[r.spark])throw Error('Unexpected proof selection');
  const c=commands.find(x=>x.label===r.unit+'-level'+r.level),a=c?.args??[];
  if(!c||c.status!==0||c.signal||c.error||a[a.indexOf('-u')+1]!==r.unit+'.adb'||!['--mode=prove','-j1','--report=all','--level='+r.level,'--prover=cvc5,z3','--timeout=5'].every(x=>a.includes(x)))throw Error('Proof command');
  auditResult(JSON.parse(fs.readFileSync(root+'/'+r.spark)),expected);
 }
 if(seen.size!==4)throw Error('Missing proof selection');
 return {levels:[1,2],checks_per_level:15,posts_per_level:3,executed:false,scope:'Selected geometry safety and stated contracts; dependency contracts/runtime models remain assumed. No whole interpreter, provider or hardware proof.'};
}
if(process.argv[1]&&path.resolve(process.argv[1])===fileURLToPath(import.meta.url)){
 const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..');
 if(process.argv.length>2){if(process.argv[2]!=='--source-root'||process.argv.length!==4)throw Error('Usage: check_current_proof.mjs [--source-root DIR]');root=path.resolve(process.argv[3]);}
 console.log(JSON.stringify(check(root,root+'/tests/aml-field-geometry/CURRENT-PLAN-PROOF.json')));
}
