import fs from 'node:fs';
for(const d of process.argv.slice(2)){
 const name=d.includes('/irq-proof-')?'irq_proof_instance':d.includes('/park-proof-')?'park_proof_instance':null;
 const units=name?[name]:['intel_adln_gpio','intel_gpio_irq'];const rows=[];
 for(const unit of units){const s=JSON.parse(fs.readFileSync(d+'/obj/gnatprove/'+unit+'.spark'));const entities=new Map(Object.entries(s.entities).map(([k,v])=>[Number(k),v]));const ids=[...new Set(s.assumptions.flatMap(r=>r.assumptions.map(a=>a.arg)))];const dependencies=ids.map(id=>({id,entity:entities.get(id)??null}));if(dependencies.some(r=>!r.entity))throw Error('Missing dependency entity');rows.push({unit,claim_rows:s.assumptions.length,dependencies});}
 fs.writeFileSync(d+'/DEPENDENCIES.json',JSON.stringify({rows,qualification:'Resolved entity identifiers do not prove imported contracts; GPIO_Proof_Boundary remains trusted. Pure dependency bodies are checked separately.'},null,2)+'\n');console.log(JSON.stringify({directory:d,units:rows.map(r=>({unit:r.unit,claim_rows:r.claim_rows,dependencies:r.dependencies.length}))}));
}
