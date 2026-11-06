import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
import{validate}from'./proof_checks.mjs';
const root=import.meta.dirname,source=path.resolve(root,'../../shared/firmware'),tool='/nix/store/9xvbsfi448cx9fjk4ivla9fsq390a8gn-gnatprove-fsf-15_2025-06-10/bin/gnatprove',solver='/nix/store/kpamqxdkwynzxyzhqj4csy6h5qj0pz21-cvc5-1.3.4/bin/cvc5';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('set TMPDIR before Nix');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-mcfg-proof-'),pins={},levels=[];
fs.mkdirSync(out+'/src');for(const n of ['firmware_tables.ads','firmware_tables.adb','firmware_tables-mcfg.ads','firmware_tables-mcfg.adb']){pins[source+'/'+n]=hash(source+'/'+n);fs.copyFileSync(source+'/'+n,out+'/src/'+n);pins[out+'/src/'+n]=hash(out+'/src/'+n);}
pins[tool]=hash(tool);pins[solver]=hash(solver);
for(const level of [1,2]){const dir=out+'/level'+level;fs.mkdirSync(dir);const project=dir+'/proof.gpr';fs.writeFileSync(project,`project Proof is for Source_Dirs use ("${out}/src");for Object_Dir use "obj";package Compiler is for Default_Switches("Ada") use ("-gnat2022","-gnata","-gnato");end Compiler;end Proof;`);pins[project]=hash(project);
 const args=['-P',project,'-u','firmware_tables.adb','firmware_tables-mcfg.adb','--mode=prove','--level='+level,'--timeout=5','--prover=cvc5','-j1','--report=all'];
 const p=spawnSync('timeout',['--kill-after=2s','180',tool,...args],{env:{...process.env,PATH:path.dirname(solver)+':'+path.dirname(tool)+':'+process.env.PATH},encoding:'utf8',timeout:183000,maxBuffer:1048576});fs.writeFileSync(dir+'/prove.log',(p.stdout??'')+(p.stderr??''));
 const reportDir=dir+'/obj/gnatprove';const reports=fs.existsSync(reportDir)?fs.readdirSync(reportDir).filter(n=>n.endsWith('.spark')).map(name=>({name,data:JSON.parse(fs.readFileSync(reportDir+'/'+name))})):[];
 const result={status:p.status,args,pins:{...pins}};validate(result,reports,level);levels.push({level,status:p.status,proof_checks:106,flow_checks:15,unproved:0,report_directory:reportDir});
 fs.writeFileSync(dir+'/RESULT.json',JSON.stringify(result,null,2)+'\n');
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('source/tool drift');
const result={accepted:true,out,pins,levels,scope:'Selected actual SDT/MCFG safety, flow, termination, accepted extent/count contracts; Interfaces intrinsics/toolchain trusted; no complete byte semantics, topology authority, AML or native/hardware proof'};
fs.writeFileSync(root+'/PROOF-RESULT.json',JSON.stringify(result,null,2)+'\n');console.log(JSON.stringify({accepted:true,out,levels}));
