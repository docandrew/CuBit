import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const suite=import.meta.dirname,root=path.resolve(suite,'../..');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('export TMPDIR before entering Nix');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const manifest=suite+'/inputs.sha256',manifestHash=hash(manifest),pins={};
for(const line of fs.readFileSync(manifest,'utf8').trim().split('\n')){if(!/^[a-f0-9]{64}  .+$/.test(line))throw Error('input format');const rel=line.slice(66);if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('relative input required');pins[root+'/'+rel]=line.slice(0,64);}
const tool='/nix/store/9xvbsfi448cx9fjk4ivla9fsq390a8gn-gnatprove-fsf-15_2025-06-10/bin/gnatprove',solvers=['/nix/store/kpamqxdkwynzxyzhqj4csy6h5qj0pz21-cvc5-1.3.4/bin/cvc5','/nix/store/c92vyfgscj1s8xnmgfiqvqk3ic35jcz1-z3-4.16.0/bin/z3'];
for(const p of [tool,...solvers])pins[p]=hash(p);
function verify(){if(hash(manifest)!==manifestHash)throw Error('manifest changed');for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);}
verify();if(process.argv[2]==='--validate-only'){console.log('Firmware-data codec proof inputs verified; no proof run.');process.exit(0);}
const out=process.env.ACPI_PROOF_OUTPUT??fs.mkdtempSync(process.env.TMPDIR+'/acpi-firmware-data-codec-proof-');if(!path.isAbsolute(out)||fs.existsSync(out+'/RESULT.json'))throw Error('fresh absolute proof output required');fs.mkdirSync(out,{recursive:true});
const args=['-P',suite+'/proof.gpr','-XACPI_PROOF_OUTPUT='+out,'-u','acpi_firmware_data_frames.adb','--mode=prove','--level=2','--timeout=5','--prover=cvc5,z3','-j1','--report=all'];
const r=spawnSync('timeout',['--kill-after=2s','180',tool,...args],{cwd:root,env:{...process.env,PATH:solvers.map(p=>path.dirname(p)).join(':')+':'+path.dirname(tool)+':'+process.env.PATH},encoding:'utf8',timeout:183000,maxBuffer:1048576});
fs.writeFileSync(out+'/prove.log',(r.stdout??'')+(r.stderr??''));verify();
const sf=out+'/obj/gnatprove/acpi_firmware_data_frames.spark',report=out+'/obj/gnatprove/gnatprove.out';
let accepted=false,audit={};
if(r.status===0&&fs.existsSync(sf)&&fs.existsSync(report)){
 const s=JSON.parse(fs.readFileSync(sf)),text=fs.readFileSync(report,'utf8');
 accepted=s.progress==='PROGRESS_PROOF'&&s.stop_reason==='STOP_REASON_NONE'&&s.proof.length===44&&s.flow.length===9&&s.pragma_assume.length===0&&s.skip_flow_proof.length===0&&s.skip_proof.length===0&&s.proof.every(p=>p.severity==='info'&&p.how_proved==='prover')&&s.flow.every(p=>p.severity==='info')&&/Total\s+53\s+9 \(17%\)\s+44 \(83%\)\s+\.\s+\./.test(text)&&text.includes('8 subprograms and packages out of 8 analyzed');
 audit={runtime_checks:s.proof.length,flow_initialization_and_termination:s.flow.length,total:53,pragma_assume:s.pragma_assume.length,skipped:s.skip_flow_proof.length+s.skip_proof.length,spark_sha256:hash(sf),report_sha256:hash(report)};
}
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({status:r.status,accepted,error:r.error?.message,args,pins,manifest_sha256:manifestHash,audit,scope:'Selected production request codec run-time checks, initialization, dependencies and termination. No functional round-trip postcondition, controller/lease/authority, imported-unit implementation, whole-service or hardware proof.',native:false},null,2)+'\n');
if(!accepted)throw Error('codec proof incomplete; inspect '+out);console.log('Firmware-data codec selected proof passed53obligations (44run-time+9flow/init/termination); evidence '+out);
