export function validate(data) {
 if(data.progress!=='PROGRESS_PROOF'||data.stop_reason!=='STOP_REASON_NONE')throw Error('proof did not complete');
 if(!Array.isArray(data.proof)||data.proof.length!==111||!Array.isArray(data.flow)||data.flow.length!==36)throw Error('proof coverage changed');
 for(const name of ['pragma_assume','skip_proof','skip_flow_proof'])if(!Array.isArray(data[name])||data[name].length)throw Error('assumed/skipped proof');
 if(Object.keys(data.spark??{}).length!==40||Object.values(data.spark).some(v=>v!=='all'))throw Error('incomplete SPARK coverage');
 if(data.proof.some(p=>p.severity!=='info'||p.how_proved!=='prover')||data.flow.some(p=>p.severity!=='info'))throw Error('unresolved proof/flow');
 const names=new Map(Object.entries(data.entities).map(([id,e])=>[Number(id),e.name]));
 for(const group of ['Small','Selection','Native'])for(const suffix of ['Decode','Decode.Refuse','Decode.Optional','Lemma_Text_Validity','Text_Matches']){
  const name='Battery_Format_Instances.'+group+'.'+suffix;
  if(!data.proof.some(p=>p.rule==='VC_POSTCONDITION'&&names.get(p.entity)===name))throw Error('missing contract proof '+name);
 }
 // Modular calls use proved local postconditions; these are not pragma Assume.
 const posts=new Set(data.proof.filter(p=>p.rule==='VC_POSTCONDITION').map(p=>p.entity));
 for(const item of data.assumptions??[])for(const dep of item.assumptions??[])if(dep.predicate==='CLAIM_POST'&&!posts.has(dep.arg))throw Error('postcondition dependency outside proved scope');
 return {proof:111,flow:36,entities:40,capacities:[8,16,128],pragma_assume:0,skipped:0,modular_postconditions_closed:true};
}
