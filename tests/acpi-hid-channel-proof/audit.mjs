function requireThat(ok,why){if(!ok)throw Error(why);}
function checked(report,name,proofCount,flowCount){
 requireThat(report.stop_reason==='STOP_REASON_NONE'&&report.progress==='PROGRESS_PROOF','proof progress/stop');
 requireThat(Array.isArray(report.proof)&&report.proof.length===proofCount,'proof count');
 requireThat(Array.isArray(report.flow)&&report.flow.length===flowCount,'flow count');
 requireThat(Object.keys(report.spark??{}).length===1&&report.spark[' 1']==='all','selected SPARK entity');
 requireThat(report.entities[' 1']?.name===name,'entity identity');
 requireThat(report.pragma_assume.length===0&&report.skip_proof.length===0&&report.skip_flow_proof.length===0,'assumptions/skips');
 for(const c of [...report.proof,...report.flow])requireThat(c.severity==='info'&&c.entity===1,'unproved/foreign check');
 for(const c of report.proof) requireThat(c.how_proved==='prover'&&c.check_tree?.length&&c.check_tree.every(t=>Object.values(t.proof_attempts??{}).some(a=>a.result==='Valid')),'proof attempt verdict');
}
export function audit(prepare,i2c,gpio){
 checked(prepare,'Channel_Proof_Model.Channels.Prepare',23,2);
 checked(i2c,'I2C_Channel_Badges.Valid',0,1);checked(gpio,'GPIO_Channel_Badges.Valid',0,1);
 requireThat(i2c.assumptions.length===0&&gpio.assumptions.length===0,'validity dependency assumptions');
 requireThat(prepare.entities[' 2']?.name==='GPIO_Channel_Badges.Valid'&&prepare.entities[' 3']?.name==='I2C_Channel_Badges.Valid','unexpected dependency identity');
 const wanted=['2:CLAIM_AORTE','2:CLAIM_EFFECTS','3:CLAIM_AORTE','3:CLAIM_EFFECTS'].sort().join(',');
 requireThat(prepare.assumptions.length===2,'claim count');
 const claims=[];
 for(const claim of prepare.assumptions){
  requireThat(claim.claim.arg===1,'claim owner');claims.push(claim.claim.predicate);
  requireThat(claim.assumptions.map(a=>a.arg+':'+a.predicate).sort().join(',')===wanted,'dependency closure');
 }
 requireThat(claims.sort().join(',')==='CLAIM_AORTE,CLAIM_POST','claim kinds');
 requireThat(prepare.proof.some(c=>c.rule==='VC_POSTCONDITION'),'missing functional postcondition');
}
