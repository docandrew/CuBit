import fs from 'node:fs';import crypto from 'node:crypto';
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const expected={'firmware_tables.spark':{proof:51,flow:10},'firmware_tables-mcfg.spark':{proof:55,flow:5}};
export function validate(result,reports,level){
 if(result.status!==0||!result.args.includes('--level='+level)||reports.length!==2)throw Error('incomplete run');
 for(const[p,h]of Object.entries(result.pins))if(hash(p)!==h)throw Error('input drift');
 if(new Set(reports.map(x=>x.name)).size!==2)throw Error('duplicate report');
 for(const {name,data:d}of reports){const n=expected[name];if(!n||d.progress!=='PROGRESS_PROOF'||d.stop_reason!=='STOP_REASON_NONE')throw Error('incomplete body');
  if(!Object.keys(d.spark??{}).length||Object.values(d.spark).some(v=>v!=='all'))throw Error('body not fully in SPARK');
  if(d.proof?.length!==n.proof||d.flow?.length!==n.flow)throw Error('missing checks');
  if([...d.proof,...d.flow].some(c=>c.severity!=='info'))throw Error('unproved check');
  for(const k of ['pragma_assume','skip_proof','skip_flow_proof'])if(!Array.isArray(d[k])||d[k].length)throw Error('assumed or skipped check');
 }
}
