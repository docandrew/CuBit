import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('pinned TMPDIR required');
const suite=import.meta.dirname,tree=path.resolve(suite,'../..'),root=process.env.ACPI_RESULT_DIR??fs.mkdtempSync(process.env.TMPDIR+'/battery-legacy-receipt-'),base=process.env.ACPI_CORE_ROOT??tree,sources=tree+'/userspace/services/acpi/power',hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('pinned TMPDIR required');
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-battery-legacy-'),pins={},commands=[],counts={};
for(const dir of [sources,suite,...['userspace/services/acpi','userspace/services/acpi/lib','userspace/services/acpi/resource-consumer','userspace/services/acpi/resource-data','shared/firmware','shared/acpi'].map(p=>base+'/'+p)])for(const n of fs.readdirSync(dir))if(/\.(ad[bs]|asl)$/.test(n))pins[dir+'/'+n]=hash(dir+'/'+n);
function run(label,args,limit){args=args[0]==='acpiexec'?['prlimit','--as=134217728','--stack=2097152',...args]:args;const p=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(limit),...args],{cwd:out,encoding:'utf8',timeout:limit*1000+3000,maxBuffer:1048576}),log=(p.stdout??'')+(p.stderr??'');fs.writeFileSync(out+'/'+label+'.log',log.slice(0,1048576));commands.push({label,args,status:p.status,signal:p.signal,error:p.error?.message??null});fs.writeFileSync(root+'/RESULT.json',JSON.stringify({accepted:false,out,pins,commands,counts,new_proof:false,native:false},null,2)+'\n');if(p.status!==0||p.error||p.signal||log.length>1048576)throw Error(label+' failed '+out);return log;}
for(const tool of ['gprbuild','iasl','acpiexec']){const p=process.env.PATH.split(':').map(d=>d+'/'+tool).find(fs.existsSync);if(!p||!fs.realpathSync(p).startsWith('/nix/store/'))throw Error('unpinned '+tool);pins[fs.realpathSync(p)]=hash(fs.realpathSync(p));}
function project(name,dirs,main,mode){const dir=out+'/'+name;fs.mkdirSync(dir);const file=dir+'/test.gpr';fs.writeFileSync(file,`project Test is for Source_Dirs use (${dirs.map(d=>JSON.stringify(d)).join(',')});for Object_Dir use "obj";for Exec_Dir use ".";for Main use ("${main}.adb");package Compiler is for Default_Switches("Ada") use ("-O0","-gnat2022","-gnatwa","-gnatwe",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'});end Compiler;end Test;`);return{dir,file};}
for(const mode of ['checked','release']){const p=project('pure-'+mode,[sources,suite],'legacy_tests',mode);run('pure-'+mode+'-build',['gprbuild','-q','-p','-j1','-P',p.file],180);const log=run('pure-'+mode+'-run',[p.dir+'/legacy_tests'],30);counts['pure-'+mode]=Number(/Legacy battery format checks\s+(\d+)/.exec(log)?.[1]);if(!counts['pure-'+mode])throw Error('missing pure check count');}
// Whole AML Core receives release code generation only. Checked testing above
// is confined to the pure format decoder, per the shared resource policy.
const p=project('reader-release',[sources,suite,...['userspace/services/acpi','userspace/services/acpi/lib','userspace/services/acpi/resource-consumer','userspace/services/acpi/resource-data','shared/firmware','shared/acpi'].map(p=>base+'/'+p)],'reader_tests','release');
run('reader-build',['gprbuild','-q','-p','-j1','-P',p.file],180);
for(const width of [1,2]){
 const source=out+'/bif'+width+'.asl';fs.writeFileSync(source,fs.readFileSync(suite+'/bif.asl','utf8').replace('"DSDT", 2','"DSDT", '+width));pins[source]=hash(source);
 const prefix=out+'/bif'+width;run('compile-'+width,['iasl','-p',prefix,source],5);pins[prefix+'.aml']=hash(prefix+'.aml');
 const log=run('reader-'+width,[p.dir+'/reader_tests',prefix+'.aml'],30);counts['reader-'+width]=Number(/Legacy battery reader checks\s+(\d+)/.exec(log)?.[1]);if(!counts['reader-'+width])throw Error('missing reader check count');
 const reference=run('reference-'+width,['acpiexec','-di','-dr','-b','execute \\BAT0._BIF',prefix+'.aml'],5);
 fs.writeFileSync(root+'/REFERENCE-'+width+'.log',reference);
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);
if(counts['pure-checked']!==counts['pure-release']||counts['reader-1']!==counts['reader-2'])throw Error('profile/width count divergence');
fs.writeFileSync(root+'/RESULT.json',JSON.stringify({accepted:true,out,pins,commands,counts,new_proof:false,native:false,hardware:false,scope:'Pure format checked/release and actual-Core legacy reader release in both AML widths; ACPICA observations awaiting exact comparison'},null,2)+'\n');console.log(JSON.stringify({accepted:true,out,counts}));
