import fs from 'node:fs';
for (const d of process.argv.slice(2)) {
  const command=JSON.parse(fs.readFileSync(d+'/command.json'));
  if(command.status!==0||command.signal||command.error)throw Error('Nonterminal/failed command');
  const rows=[];
  for (const name of ['intel_adln_gpio','intel_gpio_irq']) {
    const s=JSON.parse(fs.readFileSync(d+'/obj/gnatprove/'+name+'.spark'));
    let valid=0,trivial=0,nonvalid=0;
    function walk(v){if(!v||typeof v!=='object')return;if(v.proof_attempts){if(v.proof_attempts.CVC5?.result==='Valid')valid++;else if(v.proof_attempts.Trivial?.result==='Valid')trivial++;else nonvalid++;}for(const[k,x]of Object.entries(v))if(k!=='proof_attempts')walk(x);}
    for(const r of s.proof)walk(r.check_tree);
    const row={unit:name,proof_checks:s.proof.length,flow_checks:s.flow.length,entities:Object.keys(s.spark).length,modes:[...new Set(Object.values(s.spark))],noninfo:[...s.proof,...s.flow].filter(r=>r.severity!=='info').length,valid_CVC5:valid,valid_Trivial:trivial,nonvalid,explicit_assume:s.pragma_assume.length,skip:s.skip_proof.length+s.skip_flow_proof.length};
    if(row.noninfo||nonvalid||!valid||row.explicit_assume||row.skip||row.modes.some(v=>v!=='all'))throw Error('Bad proof '+name);
    rows.push(row);
  }
  fs.writeFileSync(d+'/AUDIT.json',JSON.stringify({directory:d,rows,scope:'Runtime safety and stated contracts of geometry and IRQ register-policy units; does not verify chipset documentation, firmware, hardware or authority.'},null,2)+'\n');
  console.log(JSON.stringify({directory:d,rows}));
}
