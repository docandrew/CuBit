import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,fixture=d+'/family.asl',out=fs.mkdtempSync(process.env.TMPDIR+'/local-family-reference-'),runs=[],pins={};
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[fixture]=hash(fixture);
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA references for both sensor/source fixtures and relative Notify-handler counts; no event-policy or hardware claim.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
for(const family of ['lid','power'])for(const width of [1,2]){
 const notices=[];
 for(const self of [false,true]){
  const prefix=out+'/'+family+'-'+width+'-'+self,target=family==='lid'?'LID0':'AC00',member=family==='lid'?'LID0._LID':'AC00._PSR',external=family==='lid'?'AC00._PSR':'LID0._LID';
  const subject='Increment (\\MCNT) If (LAnd (\\SELF, LEqual (\\MCNT, One))) { Notify (\\'+target+', 0x80) }',emit='Notify (\\'+target+', 0x80)';
  const text=fs.readFileSync(fixture,'utf8').replace('"DSDT", 2','"DSDT", '+width).replace('/*SELF*/',self?'One':'Zero').replace('/*LID_HID*/','PNP0C0D').replace('/*POWER_HID*/','ACPI0003').replace('/*LID_BODY*/',family==='lid'?subject:emit).replace('/*POWER_BODY*/',family==='power'?subject:emit);
  fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');run('compile-'+family+'-'+width+'-'+self,['iasl','-p',prefix,prefix+'.asl']);pins[prefix+'.aml']=hash(prefix+'.aml');
  const output=run('reference-'+family+'-'+width+'-'+self,['acpiexec','-di','-dr','-b','evaluate '+member+'; evaluate '+member+'; evaluate '+external+'; evaluate MCNT',prefix+'.aml']);
  const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());if(JSON.stringify(integers)!=='["1","1","1","2"]')throw Error('typed reference');
  const count=(output.match(new RegExp('Received a Device Notify on \\['+target+'\\].*Value 0x80','g'))??[]).length;notices.push(count);runs.at(-1).typedValues={integers,notifyHandlerLines:count};
 }
 if(notices[0]===0||notices[1]!==2*notices[0])throw Error('Notify handler multiplicity');
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Local family ACPICA references passed; '+out);
