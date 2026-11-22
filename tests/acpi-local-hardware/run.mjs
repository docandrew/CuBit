import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url));
let root=path.resolve(here,'../..'),validateOnly=false;
for(let i=2;i<process.argv.length;i++){
  if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);
  else if(process.argv[i]==='--validate-only')validateOnly=true;
  else throw Error('Usage: run.sh [--source-root DIR] [--validate-only]');
}
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),pins={},sources=new Map();
function inside(base,rel){
  if(path.isAbsolute(rel)||rel.split(/[\\/]/).includes('..'))throw Error('invalid input '+rel);
  const p=path.resolve(base,rel);if(!p.startsWith(base+path.sep))throw Error('outside root');
  let q=p;while(q!==base){if(fs.lstatSync(q).isSymbolicLink())throw Error('linked input '+q);q=path.dirname(q);}
  if(!fs.lstatSync(p).isFile())throw Error('not regular '+p);return p;
}
const catalog=JSON.parse(fs.readFileSync(here+'/sources.json'));
if(catalog.version!==1)throw Error('catalog version');
pins[here+'/sources.json']=sha(here+'/sources.json');
for(const [base,rows] of [[root,catalog.inputs],[here,catalog.helpers]])for(const r of rows){
  const p=inside(base,r.path);if(sha(p)!==r.sha256)throw Error('catalog drift '+p);pins[p]=r.sha256;
  if(/\.ad[bs]$/.test(p)&&r.kind!=='audit'){const n=path.basename(p);if(sources.has(n))throw Error('duplicate unit '+n);sources.set(n,p);}
}
if(validateOnly){console.log(JSON.stringify({inputs:catalog.inputs.length,helpers:catalog.helpers.length,adaFiles:sources.size,executed:false}));process.exit(0);}
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before entering Nix');
function tool(name,nix=true){
  const p=process.env.PATH.split(path.delimiter).map(d=>path.join(d,name)).find(p=>fs.existsSync(p));
  if(!p)throw Error('missing '+name);const real=fs.realpathSync(p);if(nix&&!real.startsWith('/nix/store/'))throw Error('use Nix '+name);
  pins[p]=sha(p);pins[real]=sha(real);return p;
}
const build=tool('gprbuild'),timeout=tool('timeout'),limit=tool('prlimit',false),iasl=tool('iasl'),acpica=tool('acpiexec');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-local-hardware-'),src=out+'/source',runs=[];
fs.mkdirSync(src);for(const [n,p] of sources){fs.copyFileSync(p,src+'/'+n);pins[src+'/'+n]=sha(src+'/'+n);}
function verify(){for(const [p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('source drift '+p);}
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({version:1,completed,root,out,pins,runs,nativeExecuted:false,hardwareValidated:false,proved:false},null,2)+'\n');}
function run(label,args,seconds=30,memory=24*1024**3){
  verify();const r=spawnSync(timeout,['--kill-after=2s',String(seconds),limit,'--as='+memory,...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1024**2});
  const output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);
  runs.push({label,args,status:r.status,signal:r.signal,error:r.error?.message,output});save(false);
  if(r.status!==0||r.signal||r.error)throw Error(label+' failed; '+out+'/'+label+'.log');verify();return output;
}
function project(name,mains,small=false){
  const p=out+'/'+name+'.gpr',checks=name==='checked'?['-gnata','-gnato']:['-gnatp'];
  fs.writeFileSync(p,`project ${name} is
 for Source_Dirs use ("source");
 for Main use (${mains.map(n=>'"'+n+'"').join(',')});
 for Object_Dir use "${name}/obj";
 for Exec_Dir use "${name}/bin";
 for Excluded_Source_Files use ("acpi_target_heap.adb");
 package Naming is for Body ("ACPI_Target_Heap") use "acpi_target_heap_host.adb"; end Naming;
 package Compiler is for Default_Switches ("Ada") use ("${small?'-O2':'-O0'}","-gnat2022",${checks.map(n=>'"'+n+'"').join(',')}); end Compiler;
end ${name};
`);pins[p]=sha(p);return p;
}
console.log('Hosted ACPI local hardware logs: '+out);
for(const profile of ['release','checked']){
  run(profile+'-build',[build,'-q','-p','-j1','-P',project(profile,['mux_tests.adb','nvs_mux_authority_tests.adb'],true)],180);
  for(const [main,modes] of [['mux_tests',['normal','ambiguous','no-pool','unknown','reentry']],['nvs_mux_authority_tests',['normal','ambiguous','empty','revoked','stale','unknown','bad-value','reentry','guard-denied']]])
    for(const mode of modes){const output=run(profile+'-'+main+'-'+mode,[out+'/'+profile+'/bin/'+main,mode]);if(!/checks\s+\d+\s*$/.test(output.trim()))throw Error('missing check marker');}
}
const asl=fs.readFileSync(inside(here,'pci-mux.asl'),'utf8');
for(const width of ['32','64']){
  const p=out+'/fixture'+width,text=asl.replace('"DSDT", 2,','"DSDT", '+(width==='32'?'1':'2')+',');
  if(width==='32'&&text===asl)throw Error('fixture width not changed');
  fs.writeFileSync(p+'.asl',text);pins[p+'.asl']=sha(p+'.asl');run('iasl-'+width,[iasl,'-tc','-p',p,p+'.asl']);pins[p+'.aml']=sha(p+'.aml');
  if(width==='64'){
    const bytes=[...fs.readFileSync(p+'.aml')].map(x=>'16#'+x.toString(16).padStart(2,'0').toUpperCase()+'#'),f=src+'/pci_mux_fixture.ads';
    fs.writeFileSync(f,'with Firmware_Tables;\npackage PCI_Mux_Fixture is\nData : constant Firmware_Tables.Bytes := ['+bytes.join(',')+'];\nend PCI_Mux_Fixture;\n');pins[f]=sha(f);
  }
  const output=run('acpica-'+width,[limit,'--cpu=5','--stack=2097152',acpica,'-fv','0x42','-b','evaluate ANS0; execute WRW0 0x12345678; execute PART 0x05; execute BRW0 0xCD; evaluate NANS; execute NRD0 0; execute NWR0 0x55; evaluate PANS; execute PRD0 0; execute PCI0.DEV0.LRD0 0; execute FRD0 0x66; execute FBK0 0x77; execute FIR0 0x88; execute DBW0 0x11; execute DBW0 0x12; execute DIW0 0x33; execute DIW0 0x34',p+'.aml'],5,128*1024**2);
  const values=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString(16));
  const expected=width==='32'?['5a','5','cd','42','42','55','42','42','42','66','77','88','11','12','33','34']:['5a','12345678','5','cd','42','42','55','42','42','42','66','77','88','11','12','33','34'];
  if(JSON.stringify(values)!==JSON.stringify(expected)||(width==='32'&&!/\[Buffer\] Length 08[^\n]*0000:\s+78 56 34 12 00 00 00 00/.test(output)))throw Error('ACPICA result mismatch '+width);
  runs.at(-1).typedValues={integers:values,wide:width==='32'?'Buffer08:7856341200000000':'Integer:12345678'};
  runs.at(-1).diagnostics=output.split('\n').filter(s=>/Error|Warning|Outstanding.*allocation/i.test(s));save(false);
}
run('core-build',[build,'-q','-p','-j1','-P',project('core',['pci_mux_core_tests.adb'])],180);
for(const width of ['32','64'])for(const mode of ['enabled','disabled','legacy-only','bad-declaration','refused','unknown','nvs-unknown','readonly','write-unknown','write-fault','pci-unknown','bank-completion-unknown','index-completion-unknown']){
  const output=run('core-'+width+'-'+mode,[out+'/core/bin/pci_mux_core_tests',width,mode]);
  if(!new RegExp('^PCI MUX CORE width='+width+' mode='+mode+' checks\\s+\\d+\\s*$').test(output.trim()))throw Error('missing Core marker');
}
const coreALIs=fs.readdirSync(out+'/core/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__')).sort();if(JSON.stringify(coreALIs)!==JSON.stringify(catalog.expectedCoreALIs))throw Error('Core ALI inventory');for(const n of coreALIs){const lines=fs.readFileSync(out+'/core/obj/'+n,'utf8').split('\n');if(!lines.includes('A -gnatp')||lines.includes('A -gnata'))throw Error('Core ALI flags');}verify();save(true);console.log('Passed; record '+out+'/RESULT.json');
