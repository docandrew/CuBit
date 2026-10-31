import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';import{fileURLToPath}from'node:url';
const here=path.dirname(fileURLToPath(import.meta.url));let root=path.resolve(here,'../..'),unitRoot=null;
for(let i=2;i<process.argv.length;i++){if(process.argv[i]==='--source-root'&&process.argv[i+1])root=path.resolve(process.argv[++i]);else if(process.argv[i]==='--unit-root'&&process.argv[i+1])unitRoot=path.resolve(process.argv[++i]);else throw Error('Usage: semantic.sh [--source-root DIR] [--unit-root DIR]');}unitRoot??=root;
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('TMPDIR');
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex'),out=fs.mkdtempSync(process.env.TMPDIR+'/ec-progress-semantic-');console.log(out);fs.mkdirSync(out+'/source');
const pins={[here+'/semantic.mjs']:sha(here+'/semantic.mjs'),[here+'/semantic.sh']:sha(here+'/semantic.sh')},sources=[],origin={};
function copy(f){const n=path.basename(f),dest=out+'/source/'+n;if(fs.existsSync(dest)||fs.realpathSync(f)!==f)throw Error('Duplicate/symlink '+f);fs.copyFileSync(f,dest);pins[f]=sha(f);pins[dest]=sha(dest);sources.push(n);origin[n]=f;}
let runtimeSpecs=0;for(const dir of ['userspace/runtime/gnat','shared/capabilities'])for(const n of fs.readdirSync(root+'/'+dir).filter(n=>n.endsWith('.ads'))){copy(root+'/'+dir+'/'+n);runtimeSpecs++;}
for(const stem of ['acpi_ec_types','acpi_ec_transactions','acpi_ec_registers','acpi_ec_endpoint','acpi_ec_event_types','acpi_ec_event_publisher'])for(const ext of ['ads','adb']){const f=unitRoot+'/userspace/services/acpi/ec/'+stem+'.'+ext;if(fs.existsSync(f))copy(f);}
for(const stem of ['acpi_ec_native_calls','acpi_ec_backend_progress'])for(const ext of ['ads','adb'])copy(unitRoot+'/userspace/services/acpi/ec/native/'+stem+'.'+ext);
copy(root+'/userspace/services/acpi-backend/acpi_region_protocol.ads');copy(here+'/native_semantic_tests.adb');
if(runtimeSpecs<150||Object.values(origin).some(f=>f.includes('/fixtures/')))throw Error('Runtime scope');
const tool=fs.realpathSync(process.env.PATH.split(':').map(d=>d+'/gprbuild').find(fs.existsSync));if(!tool.startsWith('/nix/store/'))throw Error('Nix required');pins[tool]=sha(tool);const commands=[];
function verify(){for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Drift '+f);}
for(const profile of ['checked','release']){
 const flags=['-gnatc','-gnat2022','-gnatwa','-gnatwe',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])],project=out+'/'+profile+'.gpr';
 fs.writeFileSync(project,`project ${profile} is\n for Source_Dirs use ("source");\n for Source_Files use (${sources.map(n=>JSON.stringify(n)).join(',')});\n for Object_Dir use "${profile}";\n package Compiler is\n for Default_Switches ("Ada") use (${flags.map(n=>JSON.stringify(n)).join(',')});\n end Compiler;\nend ${profile};\n`);pins[project]=sha(project);verify();
 const args=[tool,'-p','-j1','-c','-u','-P',project,'native_semantic_tests.adb'];const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','180',...args],{cwd:out,encoding:'utf8',maxBuffer:1048576,timeout:183000}),log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+profile+'.log',log.slice(0,1048576));commands.push({profile,args,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');verify();
 if(r.status!==0||r.signal||r.error||Buffer.byteLength(log)>1048576)throw Error('Semantic '+profile);
 const outputs=fs.readdirSync(out+'/'+profile);if(outputs.some(n=>n.endsWith('.o')||n.endsWith('.bexch')))throw Error('Unexpected object/link output');
 const alis=outputs.filter(n=>n.endsWith('.ali'));if(!alis.length)throw Error('No semantic evidence');for(const n of alis){const lines=fs.readFileSync(out+'/'+profile+'/'+n,'utf8').split('\n');for(const flag of flags.filter(x=>x.startsWith('-gnat')))if(!lines.includes('A '+flag))throw Error('ALI switch '+n);}
}
verify();fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({actual_runtime_specs:true,runtime_spec_count:runtimeSpecs,source_count:sources.length,default_saved_reply_callbacks_instantiated:true,backend_dispatcher_instantiated:true,full_width_sender_and_consumer_types:true,zero_object_output:true,native_executed:false,new_proof:false,commands,inputs:pins,origins:origin},null,2)+'\n');console.log('Actual-runtime dispatcher semantic profiles accepted');
