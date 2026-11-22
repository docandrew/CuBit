import assert from'node:assert/strict';
export function audit(report,inventory){
 assert.equal(report.progress,'PROGRESS_PROOF');assert.equal(report.stop_reason,'STOP_REASON_NONE');
 for(const key of ['pragma_assume','skip_proof','skip_flow_proof'])assert.deepEqual(report[key],[]);
 const names=new Map(Object.entries(report.entities).map(([id,e])=>[Number(id),e.name]));
 const entity=id=>{assert.ok(names.has(Number(id)),'unknown entity');return names.get(Number(id));};
 assert.deepEqual([...names.values()].sort(),inventory.entities);
 assert.deepEqual(Object.keys(report.spark).map(Number).sort((a,b)=>a-b),[...names.keys()].sort((a,b)=>a-b));
 for(const[id,mode]of Object.entries(report.spark)){entity(id);assert.equal(mode,'all');}
 const signature=x=>[entity(x.entity),x.file,x.line,x.col,x.rule].join(':');
 assert.deepEqual(report.proof.map(signature).sort(),inventory.proof);
 assert.deepEqual(report.flow.map(signature).sort(),inventory.flow);
 let cvc5=0,trivial=0;
 for(const row of report.proof){
  assert.equal(row.severity,'info');assert.equal(row.how_proved,'prover');assert.ok(row.check_tree.length>0);
  for(const node of row.check_tree){
   assert.deepEqual(node.transformations,{});const keys=Object.keys(node.proof_attempts);assert.equal(keys.length,1);
   assert.ok(['CVC5','Trivial'].includes(keys[0]));assert.equal(node.proof_attempts[keys[0]].result,'Valid');
   if(keys[0]==='CVC5')cvc5++;else trivial++;
  }
 }
 assert.deepEqual(report.proof.map(x=>({signature:signature(x),leaves:x.check_tree.map(n=>Object.keys(n.proof_attempts).sort())})).sort((a,b)=>a.signature.localeCompare(b.signature)),inventory.proof_shapes);
 for(const row of report.flow){assert.equal(row.severity,'info');assert.equal(row.how_proved,'flow');}
 const prefix='ACPI_EC_Query_Scans.';
 assert.deepEqual(report.proof.filter(x=>x.rule==='VC_POSTCONDITION').map(x=>entity(x.entity)).sort(),['Create','Extend','Fail','Record_Empty','Record_Event','Start_Iteration'].map(n=>prefix+n).sort());
 assert.deepEqual(report.flow.filter(x=>x.rule==='SUBPROGRAM_TERMINATION').map(x=>entity(x.entity)).sort(),['Can_Start','Create','Extend','Fail','Record_Empty','Record_Event','Start_Iteration','Valid','View'].map(n=>prefix+n).sort());
 assert.equal(report.proof.filter(x=>x.rule==='VC_DEFAULT_INITIAL_CONDITION').length,1);
 const claims=report.assumptions.map(x=>({claim:entity(x.claim.arg)+':'+x.claim.predicate,dependencies:x.assumptions.map(a=>entity(a.arg)+':'+a.predicate).sort()})).sort((a,b)=>a.claim.localeCompare(b.claim));
 assert.deepEqual(claims,inventory.claims);
 const claimKeys=new Set(claims.map(x=>x.claim));
 const effects=new Set(report.flow.filter(x=>x.rule==='GLOBAL_WRONG').map(x=>entity(x.entity)));
 for(const row of report.assumptions)for(const dep of row.assumptions){
  const name=entity(dep.arg);assert.ok(name.startsWith(prefix),'external proof dependency');
  if(dep.predicate==='CLAIM_EFFECTS')assert.ok(effects.has(name),'unproved effect dependency');
  else assert.ok(claimKeys.has(name+':'+dep.predicate),'unclosed dependency');
 }
 return{entities:names.size,proof:report.proof.length,flow:report.flow.length,valid_CVC5:cvc5,valid_Trivial:trivial,external_dependencies:0,skips:0};
}
export function controls(report,inventory){
 const changes=[r=>r.proof.pop(),r=>r.flow.pop(),r=>r.proof.push(r.proof[0]),r=>{r.proof[0].severity='medium';},r=>{r.proof[0].how_proved='flow';},r=>{r.proof[0].check_tree=[];},r=>{const a=r.proof[0].check_tree[0].proof_attempts;a[Object.keys(a)[0]].result='Unknown';},r=>{r.proof[0].check_tree[0].transformations={hidden:{}};},r=>{r.proof[0].check_tree.pop();},r=>{r.flow[0].severity='warning';},r=>{delete r.spark[Object.keys(r.spark)[0]];},r=>{r.spark[Object.keys(r.spark)[0]]='spec';},r=>r.pragma_assume.push({}),r=>r.skip_proof.push({}),r=>r.skip_flow_proof.push({}),r=>{r.progress='PROGRESS_FLOW';},r=>{r.stop_reason='STOP_REASON_ERROR';},r=>r.assumptions.pop(),r=>{r.assumptions.find(x=>x.assumptions.length).assumptions[0].arg=99999;},r=>{r.proof[0].rule='VC_ASSERT';}];
 for(const change of changes){const copy=structuredClone(report);change(copy);assert.throws(()=>audit(copy,inventory));}
 return changes.length;
}
