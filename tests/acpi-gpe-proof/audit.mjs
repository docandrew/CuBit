import assert from 'node:assert/strict';
export function description(report) {
 const names=new Map(Object.entries(report.entities).map(([id,e])=>[Number(id),e.name]));
 const entity=id=>{assert.ok(names.has(Number(id)),'unknown entity');return names.get(Number(id));};
 const signature=x=>[entity(x.entity),x.file,x.line,x.col,x.rule].join(':');
 return {entities:[...names.values()].sort(),spark:Object.entries(report.spark).map(([id,mode])=>[entity(id),mode]).sort(),
  proof:report.proof.map(signature).sort(),flow:report.flow.map(signature).sort(),
  proof_shapes:report.proof.map(x=>({signature:signature(x),leaves:x.check_tree.map(n=>Object.keys(n.proof_attempts).sort())})).sort((a,b)=>a.signature.localeCompare(b.signature)),
  claims:report.assumptions.map(x=>({claim:entity(x.claim.arg)+':'+x.claim.predicate,dependencies:x.assumptions.map(a=>entity(a.arg)+':'+a.predicate).sort()})).sort((a,b)=>a.claim.localeCompare(b.claim))};
}
export function audit(report,inventory) {
 assert.equal(report.progress,'PROGRESS_PROOF');assert.equal(report.stop_reason,'STOP_REASON_NONE');
 for(const key of ['pragma_assume','skip_proof','skip_flow_proof'])assert.deepEqual(report[key],[]);
 assert.deepEqual(description(report),inventory.report);
 const names=new Map(Object.entries(report.entities).map(([id,e])=>[Number(id),e.name])),entity=id=>names.get(Number(id));
 assert.equal(names.size,16);assert.equal(report.proof.length,15);assert.equal(report.flow.length,21);
 const prefix='GPE_Flow_Instance.Controller.',imports=['Mask','Clear','Enable','Invoke'].map(n=>'GPE_Flow_Instance.'+n).sort();
 assert.deepEqual(Object.entries(report.spark).filter(([,m])=>m==='spec').map(([id])=>entity(id)).sort(),imports);
 assert.equal(Object.values(report.spark).filter(m=>m==='all').length,12);
 const operations=['Start','Service','Begin_Deferred','Complete_Deferred','Finish','Perform','Record_Completion'].map(n=>prefix+n).sort();
 assert.deepEqual(report.proof.filter(x=>x.rule==='VC_POSTCONDITION').map(x=>entity(x.entity)).sort(),operations);
 assert.deepEqual(report.flow.filter(x=>x.rule==='SUBPROGRAM_TERMINATION').map(x=>entity(x.entity)).sort(),[...operations,prefix+'Observe',prefix+'Valid'].sort());
 assert.equal(report.proof.filter(x=>x.rule==='VC_DEFAULT_INITIAL_CONDITION').length,1);
 assert.equal(report.proof.filter(x=>x.rule==='VC_ASSERT').length,1);
 let cvc5=0,trivial=0;
 for(const row of report.proof){
  assert.equal(row.severity,'info');assert.equal(row.how_proved,'prover');assert.ok(row.check_tree.length>0);
  for(const node of row.check_tree){assert.deepEqual(node.transformations,{});const keys=Object.keys(node.proof_attempts);assert.equal(keys.length,1);assert.ok(['CVC5','Trivial'].includes(keys[0]));assert.equal(node.proof_attempts[keys[0]].result,'Valid');if(keys[0]==='CVC5')cvc5++;else trivial++;}
 }
 for(const row of report.flow){assert.equal(row.severity,'info');assert.equal(row.how_proved,'flow');}
 // Abstract imported callbacks have no bodies here. Their effects/termination
 // and truthful hardware reports remain assumptions, never a closed proof.
 return {entities:16,full_spark_entities:12,assumed_import_specs:imports,proof:15,flow:21,termination:9,postconditions:7,default_initialization:1,client_default_assertion:1,valid_CVC5:cvc5,valid_Trivial:trivial,skips:0};
}
export function controls(report,inventory){
 const changes=[r=>r.proof.pop(),r=>r.flow.pop(),r=>r.proof.push(r.proof[0]),r=>{r.proof[0].severity='medium';},r=>{r.proof[0].how_proved='flow';},r=>{r.proof[0].check_tree=[];},r=>{const a=r.proof[0].check_tree[0].proof_attempts;a[Object.keys(a)[0]].result='Unknown';},r=>{r.proof[0].check_tree[0].transformations={hidden:{}};},r=>{r.proof[0].check_tree.pop();},r=>{r.flow[0].severity='warning';},r=>{delete r.spark[Object.keys(r.spark)[0]];},r=>{r.spark[Object.keys(r.spark)[0]]='spec';},r=>r.pragma_assume.push({}),r=>r.skip_proof.push({}),r=>r.skip_flow_proof.push({}),r=>{r.progress='PROGRESS_FLOW';},r=>{r.stop_reason='STOP_REASON_ERROR';},r=>r.assumptions.pop(),r=>{r.assumptions.find(x=>x.assumptions.length).assumptions[0].arg=99999;},r=>{r.proof[0].rule='VC_POSTCONDITION';}];
 for(const change of changes){const copy=structuredClone(report);change(copy);assert.throws(()=>audit(copy,inventory));}
 return changes.length;
}
