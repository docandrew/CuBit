import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../..'),e=JSON.parse(fs.readFileSync(here+'/observations.json')),sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex');
for(const r of e.source_inputs)if(!r.path.startsWith('userspace/')&&!r.path.startsWith('shared/')||sha(root+'/'+r.path)!==r.sha256)throw Error('Recorded source differs '+r.path);
if(e.rows.length!==30||e.summary.cubit_passed!==0||e.rows.some(r=>r.cubit?.passed))throw Error('Unexpected result classification');
console.log(JSON.stringify({recorded_sources_current:true,inputs:e.source_inputs.length,summary:e.summary,tests_executed:false,private_provenance_reopened:false,new_proof:false,native_executed:false}));
