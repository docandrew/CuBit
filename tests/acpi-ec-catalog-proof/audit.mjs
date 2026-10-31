import fs from'node:fs';
export function audit(s){
 const entities=new Map(Object.entries(s.entities).map(([id,e])=>[Number(id),e]));
 if(s.stop_reason!=='STOP_REASON_NONE'||s.progress!=='PROGRESS_PROOF')throw Error('Incomplete analysis');
 if(s.proof.length!==65||s.flow.length!==60||Object.keys(s.spark).length!==44||Object.values(s.spark).some(m=>m!=='all'))throw Error('Proof/flow/SPARK scope');
 if(s.pragma_assume.length||s.skip_proof.length||s.skip_flow_proof.length||[...s.proof,...s.flow].some(r=>r.severity!=='info'))throw Error('Unproved, skipped or assumed result');
 let valid=0,trivial=0;
 function walk(v){if(!v||typeof v!=='object')return;if(v.proof_attempts){if(v.proof_attempts.CVC5?.result==='Valid')valid++;else if(v.proof_attempts.Trivial?.result==='Valid')trivial++;else throw Error('Unproved leaf');}for(const[k,x]of Object.entries(v))if(k!=='proof_attempts')walk(x);}
 for(const r of s.proof){const before=valid+trivial;walk(r.check_tree);if(valid+trivial===before)throw Error('Empty check tree');}if(valid===0)throw Error('No solver results');
 const posts=(file)=>s.proof.filter(r=>r.file===file&&r.rule==='VC_POSTCONDITION').map(r=>entities.get(r.entity)?.name),catalog=posts('acpi_ec_registers.ads'),engine=posts('acpi_ec_transactions.ads');
 const required=['Install','Revoke','Submit','Submit_Query','Take_Query','Step','Request_Stop','Take_Completion'];
 if(catalog.length!==8||required.some(n=>!catalog.includes('EC_Catalog_Instance.Catalog.'+n))||engine.length!==4||['Start','Step','Request_Stop','Acknowledge'].some(n=>!engine.includes('EC_Catalog_Instance.Engine.'+n)))throw Error('Missing public contract');
 for(const file of ['acpi_ec_registers.ads','acpi_ec_transactions.ads'])if(s.proof.filter(r=>r.file===file&&r.rule==='VC_INITIAL_CONDITION').length!==1)throw Error('Missing initialization');
 if(s.proof.filter(r=>r.file==='ec_catalog_instance.ads'&&r.rule==='VC_ASSERT').length!==2)throw Error('Missing engine/catalog composition');
 const boundary=new Set(),dependencies=[],selected=new Set(Object.keys(s.spark).map(Number));
 for(const row of s.assumptions){if(!entities.has(row.claim.arg))throw Error('Unknown claim');for(const a of row.assumptions){const e=entities.get(a.arg);if(!e)throw Error('Unknown dependency');if(!selected.has(a.arg)){if(!/^EC_Proof_Boundary\.(Read|Write|Clock|Owned)$/.test(e.name))throw Error('Unselected dependency '+e.name);boundary.add(e.name);}dependencies.push({predicate:a.predicate,entity:e.name});}}
 if(boundary.size!==4)throw Error('Callback boundary changed');
 return {proof_checks:65,flow_checks:60,SPARK_entities:44,valid_CVC5:valid,valid_Trivial:trivial,catalog_posts:catalog,engine_posts:engine,initial_checks:2,composition_assertions:2,explicit_assume:0,skip:0,trusted_callbacks:[...boundary].sort(),dependencies:[...new Map(dependencies.map(d=>[d.entity+':'+d.predicate,d])).values()],scope:'Default32-slot catalog and actual transaction engine instance. Exact catalog Install/Revoke/Submit/Submit_Query transitions; other catalog operations preserve stated coherence. Imported callback effects/termination, no alias/reentry, truthful IO/ownership/time remain trusted. No whole AML/native/hardware proof.'};
}
if(process.argv[2]){const d=process.argv[2],cmd=JSON.parse(fs.readFileSync(d+'/command.json'));if(cmd.status!==0||cmd.signal||cmd.error)throw Error('Failed/nonterminal command');const row=audit(JSON.parse(fs.readFileSync(d+'/obj/gnatprove/ec_catalog_instance.spark')));fs.writeFileSync(d+'/AUDIT.json',JSON.stringify(row,null,2)+'\n');console.log(JSON.stringify({...row,dependencies:row.dependencies.length}));}
