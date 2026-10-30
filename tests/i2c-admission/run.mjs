import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const here=path.dirname(new URL(import.meta.url).pathname),root=path.resolve(here,'../..'),mode=process.argv[2],output=process.argv[3];
if(!['tests','proof'].includes(mode)||!path.isAbsolute(output??'')||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Need tests|proof, absolute private output, pinned environment');
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const inventory=JSON.parse(fs.readFileSync(here+'/sources.json')),pins={};
for(const [r,h]of Object.entries(inventory.production)){const p=root+'/'+r;if(fs.realpathSync(p)!==p||sha(p)!==h)throw Error('Production drift');pins[p]=h;}
for(const n of [...inventory.mains.map(m=>m.name+'.adb'),'suite.gpr','proof.gpr','run.sh','run.mjs','sources.json','README.md'])pins[here+'/'+n]=sha(here+'/'+n);
fs.mkdirSync(output,{recursive:true});const out=fs.mkdtempSync(output+'/i2c-admission-'+mode+'-');console.log(out);
for(const [p,h]of Object.entries(pins)){fs.copyFileSync(p,out+'/'+path.basename(p),fs.constants.COPYFILE_EXCL);if(sha(out+'/'+path.basename(p))!==h)throw Error('Snapshot mismatch');}
const name=mode==='proof'?'gnatprove':'gprbuild',tool=fs.realpathSync(process.env.PATH.split(':').map(p=>p+'/'+name).find(fs.existsSync));if(!tool.startsWith('/nix/store/'))throw Error('Unpinned tool');
fs.writeFileSync(out+'/tool.json',JSON.stringify({path:tool,sha256:sha(tool)},null,2)+'\n');
const commands=[];
function run(label,args,seconds){const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:(seconds+3)*1000});const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576),{flag:'wx'});commands.push({label,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');if(r.status!==0||r.error||log.length>1048576)throw Error('Failed '+label);return log;}
if(mode==='tests')for(const profile of ['checked','release']){run(profile+'-build',[tool,'-p','-j1','-P','suite.gpr','-XPROFILE='+profile],180);for(const main of inventory.mains){const log=run(profile+'-'+main.name,[out+'/'+profile+'/bin/'+main.name],30);if(log.trim()!==main.label+' '+main.checks)throw Error('Unexpected test count');}}
else run('prove',[tool,'-P','proof.gpr','--mode=prove','--level=2','--timeout=5','--checks-as-errors=on','-j1'],180);
for(const [p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input changed');
fs.writeFileSync(out+'/inputs.json',JSON.stringify(pins,null,2)+'\n');fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({mode,commands:commands.length,checks_per_profile:mode==='tests'?inventory.checks_per_profile:null,proof_scope:mode==='proof'?['I2C_Channel_Badges','I2C_Transfer_Wire','I2C_Configure_Replies']:[],native_validated:false,hardware_validated:false},null,2)+'\n',{flag:'wx'});console.log('I2C admission '+mode+' complete; pinned inputs verified');
