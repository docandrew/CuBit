import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=import.meta.dirname,root=path.resolve(here,'../..');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Set required TMPDIR before Nix');
const configuration=JSON.parse(fs.readFileSync(here+'/sources.json'));
if(configuration.schema_version!==1)throw Error('Unsupported source catalog');
const args=process.argv.slice(2);let groups=configuration.groups;
if(args.length){if(args.length!==2||args[0]!=='--group')throw Error('Use --group controller|sender|owner-cleanup');groups=groups.filter(g=>g.name===args[1]);if(groups.length!==1)throw Error('Unknown group');}
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-startup-admission-'),pins={},runs=[],results=[];
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
for(const n of ['run.mjs','sources.json','README.md'])pins[here+'/'+n]=sha(here+'/'+n);
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('Input drift '+p);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,results,newProof:false,nativeExecuted:false,hardwareValidated:false,scope:configuration.scope},null,2)+'\n');}
function run(label,argv,seconds,cwd){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...argv],{cwd,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
 const output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal});save(false);if(r.status!==0||r.error||r.signal)throw Error('Failed '+label+'; '+out);verify();return output;
}
for(const g of groups){const dir=out+'/'+g.name,source=dir+'/source';fs.mkdirSync(source,{recursive:true});const names=new Set();
 for(const u of g.units){if(!['production','fixture'].includes(u.role)||path.basename(u.name)!==u.name||names.has(u.name))throw Error('Invalid source selection');names.add(u.name);const p=(u.role==='production'?root:here)+'/'+u.path;if(sha(p)!==u.sha256)throw Error('Catalog drift '+p);pins[p]=u.sha256;const target=source+'/'+u.name;fs.copyFileSync(p,target);pins[target]=sha(target);}
 const actual=fs.readdirSync(here+'/'+g.name).filter(n=>/\.ad[bs]$/.test(n)).sort();const expected=g.units.filter(u=>u.role==='fixture').map(u=>u.name).sort();if(JSON.stringify(actual)!==JSON.stringify(expected))throw Error('Fixture inventory drift');
 const profiles={};for(const mode of ['checked','release']){const project=dir+'/'+mode+'.gpr';fs.writeFileSync(project,`project ${mode} is for Source_Dirs use("source");for Object_Dir use"${mode}/obj";for Exec_Dir use"${mode}/bin";for Main use("${g.main}");package Compiler is for Default_Switches("Ada") use("-O1","-gnat2022",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;end ${mode};\n`);pins[project]=sha(project);
 run(g.name+'-'+mode+'-build',['gprbuild','-p','-j1','-P',project],180,dir);
 const outputs=g.arguments.map((arguments_,i)=>run(g.name+'-'+mode+'-'+i,[dir+'/'+mode+'/bin/'+g.main.replace(/\.adb$/,''),...arguments_],30,dir));
 let checks=0;for(const output of outputs){const match=output.match(new RegExp(g.pattern));if(!match)throw Error('Missing verdict '+g.name);checks+=Number(match[1]);}if(checks!==g.checksPerProfile)throw Error('Coverage changed '+g.name);
 const alis=fs.readdirSync(dir+'/'+mode+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'));if(!alis.length)throw Error('No compiled units');for(const n of alis){const flags=fs.readFileSync(dir+'/'+mode+'/obj/'+n,'utf8').split('\n');if(!flags.includes('A '+(mode==='checked'?'-gnata':'-gnatp'))||(mode==='checked'&&(!flags.includes('A -gnato')||flags.includes('A -gnatp'))))throw Error('Build-profile drift '+n);}
 profiles[mode]={outputs,checks,compiledUnits:alis.length};
 }
 if(JSON.stringify(profiles.checked.outputs)!==JSON.stringify(profiles.release.outputs))throw Error('Checked/release mismatch '+g.name);
 results.push({group:g.name,cases:g.cases,profiles});save(false);
}
verify();save(true);console.log(JSON.stringify({completed:true,out,groups:results.map(r=>({name:r.group,cases:r.cases,checksPerProfile:r.profiles.checked.checks})),totalChecks:results.reduce((n,r)=>n+r.profiles.checked.checks+r.profiles.release.checks,0)}));
