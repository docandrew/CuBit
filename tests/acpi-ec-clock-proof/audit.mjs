import assert from 'node:assert/strict';

const entities = {
  1: 'ACPI_EC_Clock_Policy.Slice',
  2: 'ACPI_EC_Clock_Policy.Decode',
  3: 'ACPI_EC_Clock_Policy',
};
const signature = c => [c.entity,c.file,c.line,c.col,c.rule].join(':');
const expectedProof = [
  '2:acpi_ec_clock_policy.adb:8:76:VC_RANGE_CHECK',
  '2:acpi_ec_clock_policy.ads:22:34:VC_DISCRIMINANT_CHECK',
  '2:acpi_ec_clock_policy.ads:22:71:VC_RANGE_CHECK',
  '2:acpi_ec_clock_policy.ads:18:16:VC_POSTCONDITION',
  '1:acpi_ec_clock_policy.adb:15:23:VC_OVERFLOW_CHECK',
  '1:acpi_ec_clock_policy.adb:14:17:VC_OVERFLOW_CHECK',
  '1:acpi_ec_clock_policy.ads:34:27:VC_OVERFLOW_CHECK',
  '1:acpi_ec_clock_policy.ads:35:78:VC_OVERFLOW_CHECK',
  '1:acpi_ec_clock_policy.ads:35:40:VC_OVERFLOW_CHECK',
  '1:acpi_ec_clock_policy.ads:33:16:VC_POSTCONDITION',
].sort();
const expectedFlow = [
  '1:acpi_ec_clock_policy.ads:30:13:SUBPROGRAM_TERMINATION',
  '1:acpi_ec_clock_policy.ads:32:11:GLOBAL_WRONG',
  '2:acpi_ec_clock_policy.ads:16:13:SUBPROGRAM_TERMINATION',
  '2:acpi_ec_clock_policy.ads:17:11:GLOBAL_WRONG',
].sort();
export function audit(report) {
  assert.equal(report.progress,'PROGRESS_PROOF');
  assert.equal(report.stop_reason,'STOP_REASON_NONE');
  for (const k of ['pragma_assume','skip_proof','skip_flow_proof']) assert.deepEqual(report[k],[]);
  assert.deepEqual(Object.keys(report.entities).map(Number).sort(),[1,2,3]);
  assert.deepEqual(Object.keys(report.spark).map(Number).sort(),[1,2,3]);
  for (const [id,entity] of Object.entries(report.entities)) {
    assert.equal(entity.name,entities[Number(id)]);
    assert.equal(report.spark[id],'all');
  }
  assert.deepEqual(report.proof.map(signature).sort(),expectedProof);
  assert.deepEqual(report.flow.map(signature).sort(),expectedFlow);
  for (const check of [...report.proof,...report.flow]) assert.equal(check.severity,'info');
  for (const check of report.flow) assert.equal(check.how_proved,'flow');
  for (const check of report.proof) {
    assert.equal(check.how_proved,'prover');
    assert.ok(check.check_tree.length>0);
    for (const branch of check.check_tree) {
      assert.equal(branch.proof_attempts.CVC5?.result,'Valid');
      assert.deepEqual(branch.transformations,{});
    }
  }
  const claims=[];
  for (const row of report.assumptions) {
    assert.deepEqual(row.assumptions,[]);
    claims.push(row.claim.arg+':'+row.claim.predicate);
  }
  assert.deepEqual(claims.sort(),[
    '1:CLAIM_AORTE','1:CLAIM_POST','2:CLAIM_AORTE','2:CLAIM_POST','3:CLAIM_AORTE',
  ]);
  return {proof:10,flow:4,entities:3,dependency_assumptions:0,skips:0};
}

export function negativeControls(report) {
  const changes = [
    r=>r.proof.pop(),
    r=>r.flow.pop(),
    r=>r.proof.push(r.proof[0]),
    r=>{r.proof[0].severity='medium';},
    r=>{r.proof[0].rule='VC_ASSERT';},
    r=>{r.proof[0].check_tree[0].proof_attempts.CVC5.result='Unknown';},
    r=>{r.proof[0].check_tree=[];},
    r=>{r.proof[0].check_tree[0].transformations={hidden:{}};},
    r=>{r.proof[0].how_proved='justified';},
    r=>{r.flow[0].severity='warning';},
    r=>{r.entities[' 1'].name='Other';},
    r=>{r.spark[' 1']='spec';},
    r=>{delete r.spark[' 3'];},
    r=>{r.assumptions[0].assumptions.push({arg:3,predicate:'CLAIM_AORTE'});},
    r=>r.assumptions.pop(),
    r=>r.pragma_assume.push({}),
    r=>r.skip_proof.push({}),
    r=>r.skip_flow_proof.push({}),
    r=>{r.progress='PROGRESS_FLOW';},
    r=>{r.stop_reason='STOP_REASON_ERROR';},
  ];
  for (const change of changes) {
    const altered=structuredClone(report); change(altered);
    assert.throws(()=>audit(altered));
  }
  return changes.length;
}
