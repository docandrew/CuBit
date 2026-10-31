import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';
import {audit,negativeControls} from './audit.mjs';
export const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
export const recipes=['run.sh','run.mjs','check.mjs','audit.mjs','sources.json'];
export function inputs(here,root){
 const sources=JSON.parse(fs.readFileSync(here+'/sources.json'));
 if(sources.version!==1||Object.keys(sources.sources).length!==3)throw Error('Source inventory');
 for(const [relative,h] of Object.entries(sources.sources)){
  const p=path.resolve(root,relative);
  if(!p.startsWith(root+'/')||fs.realpathSync(p)!==p||!fs.statSync(p).isFile()||sha(p)!==h)throw Error('Source pin '+relative);
 }
 return sources.sources;
}
export function validate(here,root){
 const sources=inputs(here,root),e=JSON.parse(fs.readFileSync(here+'/EVIDENCE.json'));
 if(JSON.stringify(e.sources)!==JSON.stringify(sources))throw Error('Evidence sources');
 for(const [rel,h]of Object.entries(e.artifacts))if(!rel.startsWith('cached/')||path.resolve(here,rel)!==here+'/'+rel||sha(here+'/'+rel)!==h)throw Error('Evidence artifact');
 const done=JSON.parse(fs.readFileSync(here+'/cached/COMPLETE.json'));
 if(JSON.stringify(done.sources)!==JSON.stringify(sources)||done.commands.length!==2)throw Error('Proof source linkage');
 for(const name of recipes)if(done.recipes[name]!==sha(here+'/'+name))throw Error('Recipe linkage');
 for(const level of [1,2]){
  const cmd=done.commands[level-1];
  if(cmd.status!==0||cmd.signal||cmd.error||!['-u','acpi_ec_clock_policy.adb','--level='+level,'--timeout=5','-j1','--report=all','--prover=cvc5','--checks-as-errors=on'].every(x=>cmd.args.includes(x)))throw Error('Proof command');
  const report=JSON.parse(fs.readFileSync(here+'/cached/level'+level+'.spark'));
  audit(report);if(negativeControls(report)!==20)throw Error('Negative controls');
 }
 return {accepted:true,cached_validation_only:true,levels:[1,2],checks_per_level:14,negative_controls:40,scope:e.scope};
}
