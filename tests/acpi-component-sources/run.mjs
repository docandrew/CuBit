import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';

const options={};
for(let n=2;n<process.argv.length;n+=2){const key=process.argv[n];if(!['--source-root','--output','--manifest-tool'].includes(key)||!process.argv[n+1]||options[key])throw Error('Usage: node run.mjs --source-root ROOT --output FRESH_DIR --manifest-tool CCL_MANIFEST');options[key]=path.resolve(process.argv[n+1]);}
const root=options['--source-root'],out=options['--output'],tool=options['--manifest-tool'];
if(!root||!out||!tool||fs.existsSync(out))throw Error('supply all options and a fresh output directory');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const pins={[tool]:hash(tool),[import.meta.filename]:hash(import.meta.filename)},commands=[],components=[];
fs.mkdirSync(out,{recursive:true});
function pin(p){if(!fs.lstatSync(p).isFile())throw Error('not a regular input '+p);const h=hash(p);if(pins[p]&&pins[p]!==h)throw Error('input changed '+p);pins[p]=h;return fs.readFileSync(p);}
function verify(){for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('input drift '+p);}
function receipt(accepted=false){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,pins,commands,components,native_codegen:false,native_executed:false,new_proof:false,scope:'Actual component-declared source directories and their native search order, plus current CuBit/runtime ABI specs and fresh per-program generated Startup. Main semantic compilation only; host GNAT standard library, not target runtime execution.'},null,2)+'\n');}
function run(label,args,cwd,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),...args],{cwd,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);commands.push({label,args,cwd,status:r.status,error:r.error?.message,log_sha256:hash(out+'/'+label+'.log')});receipt();verify();if(r.status!==0||r.error)throw Error(label+' failed; see retained log');return text;}
const compiler=process.env.PATH.split(':').map(d=>d+'/gcc').find(p=>fs.existsSync(p));
if(!compiler||!fs.realpathSync(compiler).startsWith('/nix/store/'))throw Error('use the pinned Nix GNAT environment');pin(compiler);
const runtimeProject=root+'/userspace/runtime/user_runtime.gpr',runtimeText=pin(runtimeProject).toString();
const runtimeMatch=/for\s+Source_Dirs\s+use\s*\(([^;]+)\);/.exec(runtimeText);if(!runtimeMatch)throw Error('runtime source schema');
fs.mkdirSync(out+'/runtime-api');
for(const match of runtimeMatch[1].matchAll(/"([^"]+)"/g)){
 const dir=path.resolve(root+'/userspace/runtime',match[1]);if(!dir.startsWith(root+'/'))throw Error('runtime source outside root');
 for(const n of fs.readdirSync(dir).filter(n=>n.endsWith('.ads')&&(dir!==root+'/userspace/runtime/gnat'||/^(cubit|cualloc)/.test(n)))){const bytes=pin(dir+'/'+n),dest=out+'/runtime-api/'+n;if(fs.existsSync(dest)&&!fs.readFileSync(dest).equals(bytes))throw Error('runtime ABI collision '+n);fs.writeFileSync(dest,bytes);pin(dest);}
}
const catalog=root+'/userspace/ccl/catalogs/native-runtime-services.ccl',schema=root+'/userspace/ccl/interfaces/executable-manifest.ccl';pin(catalog);pin(schema);
for(const[name,component]of [['backend','userspace/services/acpi/backend/native/component.ccl'],['frontend','userspace/services/acpi/native/component.ccl']]){
 const text=pin(root+'/'+component).toString(),m=/sources\s*=>\s*\[([^\]]*)\]/.exec(text);
 if(!m||text.includes('libraries =>'))throw Error('unsupported component source schema');
 const dirs=[path.posix.dirname(component),...[...m[1].matchAll(/"([^"]+)"/g)].map(x=>x[1])];
 const main=/main\s*=>\s*\(Main.File\s*"([^"]+)"\)/.exec(text)?.[1]??'main.adb';
 const snapshot=out+'/'+name;fs.mkdirSync(snapshot);const selected=new Map(),shadows=[];
 for(const dir of dirs){if(path.isAbsolute(dir)||dir.split('/').includes('..'))throw Error('invalid component source path');fs.mkdirSync(snapshot+'/'+dir,{recursive:true});
  for(const n of fs.readdirSync(root+'/'+dir).filter(n=>/\.ad[bs]$/.test(n))){const rel=dir+'/'+n,bytes=pin(root+'/'+rel);fs.writeFileSync(snapshot+'/'+rel,bytes);pin(snapshot+'/'+rel);
   if(selected.has(n)){if(n!=='acpi_service.ads')throw Error('unreviewed source shadow '+n);shadows.push({name:n,selected:selected.get(n),hidden:rel});}else selected.set(n,rel);
  }
 }
 if(selected.get('acpi_service.ads')!=='userspace/services/acpi/native/acpi_service.ads')throw Error('native service shadow precedence changed');
 const manifest=root+'/'+path.posix.dirname(component)+'/manifest.ccl';pin(manifest);fs.mkdirSync(snapshot+'/generated');
 run(name+'-generate',[tool,catalog,manifest,'--schema',schema,'--startup-output',snapshot+'/generated/startup.ads'],root,30);
 for(const n of fs.readdirSync(snapshot+'/generated').filter(n=>/\.ad[bs]$/.test(n)))pin(snapshot+'/generated/'+n);
 const item={name,component,component_sha256:hash(root+'/'+component),dirs,main,selection:Object.fromEntries(selected),shadows,profiles:[]};components.push(item);
 for(const mode of ['checked','release']){const obj=snapshot+'/'+mode;fs.mkdirSync(obj);const args=[compiler,'-c','-gnatc','-gnatA','-gnat2022','-gnatwa','-gnatwe','-gnatw.X',...(mode==='checked'?['-gnata','-gnato']:['-gnatp']),...dirs.map(d=>'-I'+snapshot+'/'+d),'-I'+out+'/runtime-api','-I'+snapshot+'/generated',snapshot+'/'+path.posix.dirname(component)+'/'+main,'-o',obj+'/'+main.slice(0,-4)+'.o'];
  run(name+'-'+mode,args,obj,180);const files=fs.readdirSync(obj),alis=files.filter(n=>n.endsWith('.ali'));if(alis.length!==1||files.some(n=>n.endsWith('.o')))throw Error('semantic output scope');
  const ali=fs.readFileSync(obj+'/'+alis[0],'utf8');for(const flag of ['-gnatc','-gnatwa','-gnatwe',...(mode==='checked'?['-gnata','-gnato']:['-gnatp'])])if(!ali.split('\n').includes('A '+flag))throw Error('wrong semantic profile');
  item.profiles.push({mode,alis:alis.length,objects:0,ali_sha256:hash(obj+'/'+alis[0])});receipt();
 }
}
verify();receipt(true);console.log('Exact-component semantic gate: both programs, checked/release, 4 ALIs and zero objects.');
