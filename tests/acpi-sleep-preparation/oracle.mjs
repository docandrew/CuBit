import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
const result=process.argv[2];if(process.argv.length!==3||!result?.startsWith('/'))throw Error('Usage: node oracle.mjs ABS_RESULT_JSON');
const host=JSON.parse(fs.readFileSync(result)),out=host.out;
const w=fs.mkdtempSync(out+'/oracle-');
if(!host.completed||host.observations.length!==20||host.observations.some(r=>!r.passed))throw Error('Hosted prerequisite');
for(const[p,h]of Object.entries(host.pins))if(hash(p)!==h)throw Error('Hosted drift');
const tool='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec';
if(hash(tool)!==host.pins[tool])throw Error('Oracle tool drift');
const pins={[result]:hash(result),[import.meta.filename]:hash(import.meta.filename),[tool]:hash(tool)},runs=[],observations=[];
for(const mode of ['normal','integer','package','reference','metadata','failure','absent','wrong-kind','alias'])for(const width of[1,2]){
 let fixture=out+'/'+mode+width+'.aml';if(hash(fixture)!==host.pins[fixture])throw Error('Fixture drift');pins[fixture]=hash(fixture);
 if(mode==='metadata'){
   const source=out+'/metadata'+width+'.asl';if(hash(source)!==host.pins[source])throw Error('ASL drift');pins[source]=hash(source);
   const prefix=w+'/ignored-metadata'+width;
   const text=fs.readFileSync(source,'utf8').replace(/\}\s*$/,'Method(WPTS,1){_PTS(Arg0) Return(Zero)}}');
   fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');
   const iasl='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl';
   if(hash(iasl)!==host.pins[iasl])throw Error('ASL tool drift');pins[iasl]=hash(iasl);
   const built=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',iasl,'-oa','-p',prefix,prefix+'.asl'],{encoding:'utf8',timeout:8000,maxBuffer:1048576});
   fs.writeFileSync(prefix+'-compile.log',(built.stdout??'')+(built.stderr??''));
   runs.push({label:'iasl-ignored-metadata',width,status:built.status,signal:built.signal,error:built.error?.message??null});
   if(built.status!==0||built.error||built.signal)throw Error('Ignored-return wrapper compilation');
   fixture=prefix+'.aml';pins[fixture]=hash(fixture);
 }
 const commands=mode==='normal'?Array.from({length:5},(_,i)=>'execute _PTS '+(i+1)+';evaluate SEEN;evaluate CALL').join(';'):(mode==='metadata'?'execute WPTS 5;evaluate SEEN;evaluate CALL':'execute _PTS 5;evaluate SEEN;evaluate CALL');
 const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',tool,'-l','-di','-dr','-b',commands,fixture],{encoding:'utf8',timeout:8000,maxBuffer:1048576});
 const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(w+'/ORACLE-'+mode+width+'.log',text);runs.push({mode,width,status:r.status,signal:r.signal,error:r.error?.message??null});
 if(r.status!==0||r.error||r.signal)throw Error('ACPICA execution '+mode+width);
 const values=[...text.matchAll(/Evaluation of \\(SEEN|CALL) returned object[^\n]*\n\s*\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>({name:m[1],value:BigInt('0x'+m[2]).toString()}));
 const expected=mode==='normal'?Array.from({length:5},(_,i)=>[{name:'SEEN',value:String(i+1)},{name:'CALL',value:String(i+1)}]).flat():[{name:'SEEN',value:['absent','wrong-kind'].includes(mode)?'0':'5'},{name:'CALL',value:['absent','wrong-kind'].includes(mode)?'0':'1'}];
 if(JSON.stringify(values)!==JSON.stringify(expected))throw Error('ACPICA effect disagreement '+mode+width);
 const failures=[...text.matchAll(/Evaluation of (\S+) failed with status (\S+)/g)].map(m=>({method:m[1].replace(/^\\+/, '\\'),status:m[2]}));
 if(mode==='failure'){
   if(failures.length!==1||failures[0].method!=='\\_PTS'||failures[0].status!=='AE_AML_DIVIDE_BY_ZERO')throw Error('Expected division failure');
 }else if(mode==='absent'){
   if(failures.length!==1||failures[0].method!=='\\_PTS'||failures[0].status!=='AE_NOT_FOUND')throw Error('Expected missing root');
 }else if(failures.length||/Aborting method|Could not resolve symbol/.test(text))throw Error('Unexpected AML failure '+mode+width);
 observations.push({mode,width,values,failures,ignoredReturnWrapper:mode==='metadata'});
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('Oracle drift');
fs.writeFileSync(w+'/ORACLE-RESULT.json',JSON.stringify({accepted:true,pins,runs,observations,scope:'Independent ACPICA PTS effects/arguments and explicit errors; skips host-specific operation-budget loop. Invalid reserved-name fixtures are deliberate checksum-correct negative AML.',newProof:false,nativeExecuted:false,hardwareValidated:false},null,2)+'\n');
console.log(JSON.stringify({accepted:true,out:w,ACPICARuns:observations.length}));
