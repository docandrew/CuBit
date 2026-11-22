import fs from 'node:fs';
export function audit(d){
 const command=JSON.parse(fs.readFileSync(d+'/command.json'));
 if(command.status!==0||command.signal||command.error)throw Error('Failed/nonterminal command');
 const unit=command.args.includes('gpio_client_proof_instance.ads')?'gpio_client_proof_instance':'gpio_control_wire';
 const s=JSON.parse(fs.readFileSync(d+'/obj/gnatprove/'+unit+'.spark'));
 let valid=0,trivial=0,nonvalid=0;
 function walk(v){if(!v||typeof v!=='object')return;if(v.proof_attempts){if(v.proof_attempts.CVC5?.result==='Valid')valid++;else if(v.proof_attempts.Trivial?.result==='Valid')trivial++;else nonvalid++;}for(const[k,x]of Object.entries(v))if(k!=='proof_attempts')walk(x);}
 for(const r of s.proof)walk(r.check_tree);
 const entities=new Map(Object.entries(s.entities).map(([id,e])=>[Number(id),e]));
 const ids=[...new Set(s.assumptions.flatMap(r=>r.assumptions.map(a=>a.arg)))];
 const dependencies=ids.map(id=>({id,entity:entities.get(id)??null}));
 if(dependencies.some(d=>!d.entity))throw Error('Unresolved dependency ID');
 const row={directory:d,unit,proof_checks:s.proof.length,functional_postconditions:s.proof.filter(r=>r.rule==='VC_POSTCONDITION').length,flow_checks:s.flow.length,SPARK_entities:Object.keys(s.spark).length,modes:[...new Set(Object.values(s.spark))],noninfo:[...s.proof,...s.flow].filter(r=>r.severity!=='info').length,valid_CVC5:valid,valid_Trivial:trivial,nonvalid,explicit_assume:s.pragma_assume.length,skip:s.skip_proof.length+s.skip_flow_proof.length,claim_rows:s.assumptions.length,dependencies,trusted_boundary:unit==='gpio_client_proof_instance'?'Imported Submit arbitrary acceptance; global effects/termination/no reentry/no owner-state aliasing and no accepted submission on False are trusted. Kernel envelope truth and process-global token allocation remain external.':'Pure wire codec only; no sender authentication, capability acquisition or transport delivery proof.'};
 fs.writeFileSync(d+'/AUDIT.json',JSON.stringify(row,null,2)+'\n');
 if(!row.proof_checks||!row.flow_checks||row.noninfo||nonvalid||!valid||row.explicit_assume||row.skip||row.modes.some(m=>m!=='all'))throw Error('Proof audit failed');
 if(unit==='gpio_client_proof_instance'&&row.functional_postconditions!==5)throw Error('Incomplete functional contract scope');
 fs.writeFileSync(d+'/AUDIT.json',JSON.stringify(row,null,2)+'\n');return row;
}
