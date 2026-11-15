import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const suite=import.meta.dirname,tree=path.resolve(suite,'../..'),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-status-cache-runtime-'),hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),pins={},commands=[],counts={};
const dirs=[suite,...['userspace/services/acpi','userspace/services/acpi/lib','userspace/services/acpi/backend','userspace/services/acpi/native-clock','userspace/services/acpi/power','userspace/services/acpi/lid','userspace/services/acpi/request-driver','userspace/services/acpi/resource-consumer','userspace/services/acpi/resource-data','shared/acpi','shared/firmware'].map(p=>tree+'/'+p)];
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const catalog=JSON.parse(fs.readFileSync(suite+'/sources.json'));pins[suite+'/sources.json']=hash(suite+'/sources.json');
for(const[base,rows]of [[tree,catalog.inputs],[suite,catalog.helpers]])for(const row of rows){
 if(path.isAbsolute(row.path)||row.path.split('/').includes('..'))throw Error('relative source');
 const p=base+'/'+row.path;if(!fs.lstatSync(p).isFile()||fs.lstatSync(p).isSymbolicLink()||hash(p)!==row.sha256)throw Error('baseline '+p);pins[p]=row.sha256;
}
const frozenDirs=dirs.map((dir,index)=>{const target=out+'/input-'+index;fs.mkdirSync(target);
 for(const n of fs.readdirSync(dir))if(/\.(ad[bs]|asl)$/.test(n)){if(!pins[dir+'/'+n])throw Error('unlisted Ada/ASL input');fs.copyFileSync(dir+'/'+n,target+'/'+n);pins[target+'/'+n]=hash(target+'/'+n);}
 return target;});
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,commands,counts,new_proof:false,native:false,hardware:false,scope:'Actual request-owned lid/AC-source cache status gates with same-Core runtime and modeled provider/clock callbacks; hosted only.'},null,2)+'\n');}
function run(label,args,limit){const p=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(limit),'prlimit','--as=25769803776',...args],{encoding:'utf8',timeout:limit*1000+3000,maxBuffer:1048576,cwd:out});fs.writeFileSync(out+'/'+label+'.log',(p.stdout??'')+(p.stderr??''));commands.push({label,args,status:p.status});save(false);if(p.status!==0||p.error||p.signal)throw Error(label+' failed '+out);return p.stdout??'';}
const project=out+'/test.gpr';fs.writeFileSync(project,`project Test is for Source_Dirs use (${frozenDirs.map(p=>JSON.stringify(p)).join(',')});for Object_Dir use "obj";for Exec_Dir use ".";for Main use ("cache_tests.adb");for Excluded_Source_Files use ("acpi_service.ads");package Naming is for Spec("ACPI_Service") use "acpi_service_host.ads";end Naming;package Compiler is for Default_Switches("Ada") use ("-O0","-gnat2022","-gnatwa","-gnatwe","-gnatp");end Compiler;end Test;`);
run('build',['gprbuild','-q','-p','-j1','-P',project],180);
for(const width of [1,2])for(const mode of ['ready','absent','disabled','broken','wrong','default','zero']){
 const prefix=out+'/'+mode+width,src=prefix+'.asl';let source=fs.readFileSync(suite+'/cache.asl','utf8').replace('"DSDT", 2','"DSDT", '+width);
 const value=({ready:'0x0F',absent:'Zero',disabled:'0x0D',broken:'0x07',wrong:'Buffer () {1}',zero:'0x0F'})[mode];
 for(const [tag,counter]of [['LID_STATUS','LSCT'],['POWER_STATUS','PSCT']])source=source.replace('/*'+tag+'*/',mode==='default'?'':'Method (_STA, 0) { Increment (\\'+counter+') If (LEqual (\\'+counter+', One)) { Return (0x0F) } Return ('+value+') }');
 if(mode==='wrong')source=source.replaceAll('Return (Buffer () {1})','CopyObject (Buffer () {1}, SVAL) Return (SVAL)');
 fs.writeFileSync(src,source);pins[src]=hash(src);run('compile-'+mode+'-'+width,['iasl','-p',prefix,src],5);pins[prefix+'.aml']=hash(prefix+'.aml');
 const stdout=run('run-'+mode+'-'+width,[out+'/cache_tests',prefix+'.aml',mode],30);
 if(!new RegExp('^Status cache '+mode+' checks\\s+\\d+\\n$').test(stdout))throw Error('coverage '+stdout);counts[mode+'-'+width]=Number(stdout.match(/checks\s+(\d+)/)[1]);
}
const reference=run('acpica-reference',[process.execPath,suite+'/reference.mjs'],30);if(!reference.includes('Status-cache ACPICA references passed; '))throw Error('reference marker');
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);save(true);console.log(JSON.stringify({accepted:true,out,counts,native:false,new_proof:false}));
