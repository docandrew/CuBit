import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';

// Run in the pinned Nix environment, with a 64 MiB stack and core dumps off.
const suite=import.meta.dirname, root=path.resolve(suite,'../..');
const output=process.argv[2];
if(!output||!path.isAbsolute(output)||fs.existsSync(output))throw Error('supply a fresh absolute output directory');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));
const pins={[suite+'/sources.json']:hash(suite+'/sources.json'),[suite+'/run.mjs']:hash(suite+'/run.mjs')};
fs.mkdirSync(output,{recursive:true});fs.mkdirSync(output+'/source');
const names=new Set();
for(const[rel,want]of Object.entries(catalog.inputs)){
 if(path.isAbsolute(rel)||rel.split('/').includes('..')||!(/^(userspace|shared|tests)\//.test(rel)))throw Error('invalid source path');
 const src=root+'/'+rel,n=path.basename(rel);
 if(names.has(n)||!fs.lstatSync(src).isFile()||hash(src)!==want)throw Error('source drift/duplicate '+rel);
 names.add(n);pins[src]=want;fs.copyFileSync(src,output+'/source/'+n);pins[output+'/source/'+n]=want;
}
const commands=[],expected=new Map([
 ['scoped_client_tests',304],['scoped_client_pu2c_tests',315],
 ['lease_scope_tests',251],['diagnostic_format_tests',105]]);
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input changed '+p);}
function receipt(accepted=false){fs.writeFileSync(output+'/RESULT.json',JSON.stringify({accepted,pins,commands,checks_each_profile:975,native:false,new_proof:false,hardware_validated:false,scope:catalog.scope},null,2)+'\n');}
for(const mode of ['checked','release']){
 const gp=output+'/'+mode+'.gpr';
 fs.writeFileSync(gp,`project ${mode} is for Source_Dirs use ("source");for Main use (${[...expected.keys()].map(n=>'"'+n+'.adb"').join(',')});for Object_Dir use "${mode}/obj";for Exec_Dir use "${mode}/bin";package Compiler is for Default_Switches ("Ada") use ("-O1","-gnat2022","-gnatwa","-gnatwe",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;end ${mode};\n`);
 pins[gp]=hash(gp);
 const runs=[['build',['gprbuild','-q','-p','-j1','-P',gp],180],...[...expected.keys()].map(n=>[n,[output+'/'+mode+'/bin/'+n],30])];
 for(const[name,args,seconds]of runs){
  verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd:output,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});
  const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(output+'/'+mode+'-'+name+'.log',text);
  const count=expected.get(name),marker=count===undefined||new RegExp('checks\\s*'+count+'(?:\\s|$)','i').test(text);
  commands.push({mode,name,status:r.status,error:r.error?.message,expected_checks:count,marker_matched:marker});receipt();verify();
  if(r.status!==0||!marker)throw Error(mode+' '+name+' failed; inspect saved log');
  console.log(mode+' '+name+' passed');
 }
}
receipt(true);console.log('Hosted Field diagnostics: 975 checks/profile; no native execution or proof.');
