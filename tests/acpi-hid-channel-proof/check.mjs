import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {audit} from './audit.mjs';
export const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
export const recipes=['run.sh','run.mjs','check.mjs','audit.mjs','channel_proof_model.ads','policy_main.adb','sources.json'];
export function sourcePins(here){
 const root=path.resolve(here,'../..'),m=JSON.parse(fs.readFileSync(here+'/sources.json'));
 if(m.version!==1||Object.keys(m.sources??{}).length!==21)throw Error('source inventory');
 const names=new Set();
 for(const[relative,sha]of Object.entries(m.sources)){
  const p=path.resolve(root,relative);
  if(!p.startsWith(root+'/')||fs.realpathSync(p)!==p||!fs.lstatSync(p).isFile()||names.has(path.basename(p))||hash(p)!==sha)throw Error('source pin '+relative);
  names.add(path.basename(p));
 }
 return{root,sources:m.sources};
}
export function validate(here){
 const{sources}=sourcePins(here),e=JSON.parse(fs.readFileSync(here+'/EVIDENCE.json'));
 if(e.version!==1||JSON.stringify(sources)!==JSON.stringify(e.sources))throw Error('evidence source linkage');
 for(const[name,sha]of Object.entries(e.artifacts))if(!name.startsWith('cached/')||path.resolve(here,name)!==here+'/'+name||hash(here+'/'+name)!==sha)throw Error('artifact pin');
 const complete=JSON.parse(fs.readFileSync(here+'/cached/COMPLETE.json'));
 if(!complete.accepted||complete.commands.length!==10||complete.negative_controls!==24)throw Error('original completion');
 for(const name of recipes)if(complete.recipes[name]!==hash(here+'/'+name))throw Error('recipe drift');
 for(const[relative,sha]of Object.entries(sources))if(complete.source_copies[path.basename(relative)]!==sha)throw Error('original source-copy linkage');
 for(const level of [1,2]){
  const reports=['prepare','i2c-valid','gpio-valid'].map(name=>JSON.parse(fs.readFileSync(here+'/cached/level'+level+'-'+name+'.spark')));audit(...reports);
  for(const name of ['prepare','i2c-valid','gpio-valid']){
   const c=complete.commands.find(c=>c.level===level&&c.name===name);
   const selection={prepare:['channel_proof_model.ads','acpi_hid_i2c_channels.adb:6'],'i2c-valid':['i2c_channel_badges.ads','i2c_channel_badges.ads:26'],'gpio-valid':['gpio_channel_badges.ads','gpio_channel_badges.ads:20']}[name];
   if(!c?.args.includes(selection[0])||!c.args.includes('--limit-subp='+selection[1]))throw Error('proof selection');
   if(!c||c.status!==0||c.signal||c.error||!['--level='+level,'--timeout=5','-j1','--report=all','--prover=cvc5'].every(x=>c.args.includes(x)))throw Error('proof command');
  }
 }
 if(complete.hosted_checks_per_profile!==325)throw Error('hosted evidence');
 return{accepted:true,cached_validation_only:true,levels:[1,2],selected_checks_per_level:27,hosted_checks_per_profile:325,scope:e.scope};
}
