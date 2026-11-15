import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const suite=import.meta.dirname,tree=path.resolve(suite,'../..'),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-local-hid-events-'),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),pins={},commands=[],counts={};
const dirs=[suite,...['userspace/services/acpi','userspace/services/acpi/lib','userspace/services/acpi/backend','userspace/services/acpi/native-clock','userspace/services/acpi/power','userspace/services/acpi/lid','userspace/services/acpi/hid-i2c','userspace/services/acpi/request-driver','userspace/services/acpi/resource-consumer','userspace/services/acpi/resource-data','shared/acpi','shared/firmware'].map(p=>tree+'/'+p)];
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));pins[suite+'/sources.json']=hash(suite+'/sources.json');
for(const[base,rows]of [[tree,catalog.inputs],[suite,catalog.helpers]])for(const row of rows){
 if(path.isAbsolute(row.path)||row.path.split('/').includes('..'))throw Error('relative source');
 const p=base+'/'+row.path;if(!fs.lstatSync(p).isFile()||fs.lstatSync(p).isSymbolicLink()||hash(p)!==row.sha256)throw Error('baseline '+p);pins[p]=row.sha256;
}
const frozenDirs=dirs.map((dir,index)=>{const target=out+'/input-'+index;fs.mkdirSync(target);
 for(const n of fs.readdirSync(dir))if(/\.(ad[bs]|asl)$/.test(n)){if(!pins[dir+'/'+n])throw Error('unlisted Ada/ASL source');fs.copyFileSync(dir+'/'+n,target+'/'+n);pins[target+'/'+n]=hash(target+'/'+n);}
 return target;});
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,commands,counts,new_proof:false,native:false,hardware:false,scope:'Complete bounded HID event inventory on actual request/Core/runtime/discovery/resolver; modeled clock/provider/health/fact callbacks. Hosted only.'},null,2)+'\n');}
function run(label,args,limit){const p=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(limit),'prlimit','--as=25769803776',...args],{encoding:'utf8',timeout:limit*1000+3000,maxBuffer:1048576,cwd:out});fs.writeFileSync(out+'/'+label+'.log',(p.stdout??'')+(p.stderr??''));commands.push({label,args,status:p.status,error:p.error?.message??null,signal:p.signal});save(false);if(p.status!==0||p.error||p.signal)throw Error(label+' failed '+out);return p.stdout??'';}
const project=out+'/test.gpr';fs.writeFileSync(project,`project Test is for Source_Dirs use (${frozenDirs.map(p=>JSON.stringify(p)).join(',')});for Object_Dir use "obj";for Exec_Dir use ".";for Main use ("hid_events_tests.adb");for Excluded_Source_Files use ("acpi_service.ads");package Naming is for Spec("ACPI_Service") use "acpi_service_host.ads";end Naming;package Compiler is for Default_Switches("Ada") use ("-O0","-gnat2022","-gnatwa","-gnatwe","-gnatp");end Compiler;end Test;`);
run('build',['gprbuild','-q','-p','-j1','-P',project],180);

for(const width of [1,2])for(const mode of ['normal','empty','small','limit','budget','missing','self','source-lost','stopped','root-query']){
 const prefix=out+'/'+mode+width,src=prefix+'.asl';const text=fs.readFileSync(suite+'/hid.asl','utf8').replace('"DSDT", 2','"DSDT", '+width)
 .replace('/*CID0*/',mode==='empty'?'TEST0001':'PNP0C50').replace('/*CID1*/',mode==='empty'?'TEST0002':'PNP0C50')
 .replace('/*SELF*/',mode==='self'?'Notify (TPD0, 0x80)':'').replace('/*GPIO*/',mode==='missing'?'\\\\MISS':'\\\\GPIO')
 .replace('/*EXTRA*/',mode==='limit'?'Device (TPD2) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F) }':'');
 fs.writeFileSync(src,text);pins[src]=hash(src);run('compile-'+mode+'-'+width,['iasl','-p',prefix,src],5);pins[prefix+'.aml']=hash(prefix+'.aml');
 const output=run('run-'+mode+'-'+width,[out+'/hid_events_tests',prefix+'.aml',mode],30);
 if(!new RegExp('^HID events '+mode+' checks\\s+\\d+\\n$').test(output))throw Error('coverage '+output);counts[mode+'-'+width]=Number(output.match(/checks\s+(\d+)/)[1]);
}
const reference=run('acpica-reference',[process.execPath,suite+'/reference.mjs'],30);if(!reference.includes('Local HID ACPICA references passed; '))throw Error('reference marker');
const artifacts={binaries:[out+'/hid_events_tests'],alis:[]};for(const p of artifacts.binaries)pins[p]=hash(p);pins[project]=hash(project);
for(const n of fs.readdirSync(out+'/obj').filter(n=>n.endsWith('.ali')&&!n.startsWith('b__'))){
 const p=out+'/obj/'+n,lines=fs.readFileSync(p,'utf8').split('\n');for(const flag of ['-gnatp','-gnatwa','-gnatwe'])if(!lines.includes('A '+flag))throw Error('profile '+n);
 if(lines.includes('A -gnata'))throw Error('assertion-enabled Core');pins[p]=hash(p);artifacts.alis.push(n);
}
fs.writeFileSync(out+'/ARTIFACTS.json',JSON.stringify(artifacts,null,2)+'\n');
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);save(true);console.log(JSON.stringify({accepted:true,out,counts,native:false,new_proof:false}));
