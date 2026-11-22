import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const suite=import.meta.dirname,root=path.resolve(suite,'../..'),name=path.basename(suite),owner=name==='acpi-firmware-data-owner';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('set TMPDIR before entering Nix');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const manifest=suite+'/inputs.sha256',manifestHash=hash(manifest);
const pins=Object.fromEntries(fs.readFileSync(manifest,'utf8').trim().split('\n').map(l=>{if(!/^[a-f0-9]{64}  [^\n]+$/.test(l))throw Error('input format');return[l.slice(66),l.slice(0,64)];}));
const dirs=fs.readFileSync(suite+'/source-dirs.txt','utf8').trim().split('\n');
function verify(){
 if(hash(manifest)!==manifestHash)throw Error('manifest changed');
 for(const [rel,h]of Object.entries(pins)){if(path.isAbsolute(rel)||rel.split('/').includes('..'))throw Error('relative input required');const p=root+'/'+rel;if(fs.lstatSync(p).isSymbolicLink()||hash(p)!==h)throw Error('input changed '+rel);}
 const actual=dirs.flatMap(d=>fs.readdirSync(root+'/'+d).filter(n=>/\.ad[bs]$/.test(n)).map(n=>d+'/'+n)).sort();
 if(actual.join('\n')+'\n'!==fs.readFileSync(suite+'/source-files.txt','utf8'))throw Error('source inventory changed');
}
verify();
if(process.argv[2]==='--validate-only'){console.log(name+' inputs and closed source directories verified; no compile.');process.exit(0);}
const out=process.env.ACPI_TEST_OUTPUT??fs.mkdtempSync(process.env.TMPDIR+'/'+name+'-');
if(!path.isAbsolute(out)||fs.existsSync(out+'/RESULT.json'))throw Error('fresh absolute output required');fs.mkdirSync(out,{recursive:true});
const commands=[];
function run(label,args,seconds,marker){verify();const r=spawnSync('timeout',['--kill-after=2s',String(seconds),...args],{cwd:root,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);commands.push({label,status:r.status,error:r.error?.message});fs.writeFileSync(out+'/RESULT.json',JSON.stringify({root,suite:name,manifest:manifestHash,pins,commands,proved:false,native:false,hardware:false,scope:owner?'Actual AML Core/startup/physical lease and concrete firmware session; grants, hardware and transport modeled. Release only.':'Actual firmware-data pipeline, publication/scopes/history/session; hardware, grants and transport modeled. Checked and release.'},null,2)+'\n');verify();if(r.status!==0||marker&&!text.includes(marker))throw Error('failed '+label+'; see '+out);console.log(label+' passed'+(marker?' '+marker:''));}
for(const profile of owner?['release']:['checked','release']){
 run(profile+'-build',['gprbuild','-q','-p','-j1','-P',suite+'/'+profile+'.gpr','-XACPI_TEST_BUILD='+out],180);
 if(owner){for(let i=0;i<12;i++)run('locked-'+i,[out+'/'+profile+'/bin/locked_owner_tests',String(i)],30,'Locked owner checks');for(let i=0;i<6;i++)run('firmware-'+i,[out+'/'+profile+'/bin/firmware_owner_tests','0',String(i)],30,'Concrete-session startup owner checks');}
 else run(profile+'-pipeline',[out+'/'+profile+'/bin/publication_tests'],30,'Firmware data AML-provider-to-record checks 35851');
}
console.log('Hosted '+name+' passed. Evidence: '+out);
