import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,fixture=d+'/sample.asl',out=fs.mkdtempSync(process.env.TMPDIR+'/local-sample-reference-'),runs=[],pins={};
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[fixture]=hash(fixture);
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA references for actual sensor/source method returns and relative Notify delivery count; not the owner state machine or hardware.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
for(const width of [1,2]){
 const notices=[];
 for(const self of [false,true]){
  const prefix=out+'/fixture-'+width+'-'+self,source=fs.readFileSync(fixture,'utf8').replace('"DSDT", 2','"DSDT", '+width).replace('/*SELF*/',self?'One':'Zero').replace('/*HID*/','PNP0C0D');
  fs.writeFileSync(prefix+'.asl',source);pins[prefix+'.asl']=hash(prefix+'.asl');run('compile-'+width+'-'+self,['iasl','-p',prefix,prefix+'.asl']);pins[prefix+'.aml']=hash(prefix+'.aml');
  const output=run('reference-'+width+'-'+self,['acpiexec','-di','-dr','-b','evaluate LID0._LID; evaluate LID0._LID; evaluate AC00._PSR; evaluate LCNT',prefix+'.aml']);
  const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());if(JSON.stringify(integers)!=='["1","1","1","2"]')throw Error('typed reference');
  const count=(output.match(/Received a Device Notify on \[LID0\].*Value 0x80/g)??[]).length;notices.push(count);runs.at(-1).typedValues={integers,notifyHandlerLines:count};
 }
 if(notices[0]===0||notices[1]!==2*notices[0])throw Error('Notify reference multiplicity');
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Local sample ACPICA references passed; '+out);
