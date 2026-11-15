import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const d=import.meta.dirname,fixture=d+'/hid.asl';
const out=fs.mkdtempSync(process.env.TMPDIR+'/local-hid-reference-'),pins={},runs=[];
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[fixture]=hash(fixture);
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA STA/DSM-register and CRS fixture equivalence plus self-Notify; not ownership/event policy or hardware validation.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');
 fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
const helper=`Method (REG0,0) { Return (\\TPD0._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, One, Zero)) }
Method (REG1,0) { Return (\\TPD1._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, One, Zero)) }`;
for(const width of [1,2])for(const mode of ['normal','empty','self']){
 const prefix=out+'/'+mode+width,text=fs.readFileSync(fixture,'utf8').replace('"DSDT", 2','"DSDT", '+width)
 .replace('/*CID0*/',mode==='empty'?'TEST0001':'PNP0C50').replace('/*CID1*/',mode==='empty'?'TEST0002':'PNP0C50')
 .replace('/*SELF*/',mode==='self'?'Notify (TPD0, 0x80)':'').replace('/*GPIO*/','\\\\GPIO').replace('/*EXTRA*/',helper);
 fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');run('compile-'+mode+'-'+width,['iasl','-p',prefix,prefix+'.asl']);pins[prefix+'.aml']=hash(prefix+'.aml');
 const output=run('reference-'+mode+'-'+width,['acpiexec','-di','-dr','-b','evaluate TPD0._STA; evaluate REG0; evaluate REG1; evaluate TPD0._CRS; evaluate TPD1._CRS',prefix+'.aml']);
 if(/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(output))throw Error('reference evaluation');
 const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
 if(JSON.stringify(integers)!=='["15","32","32"]')throw Error('typed register/STA');
 const buffers=[];let current=null;
 for(const line of output.split('\n')){
  const begin=/\[Buffer\] Length ([0-9A-Fa-f]+)/.exec(line);
  if(begin){if(current)buffers.push(current);current={length:parseInt(begin[1],16),bytes:[]};continue;}
  const row=/^\s*[0-9A-Fa-f]{4}:\s+((?:[0-9A-Fa-f]{2}(?:\s+|$))+)/.exec(line);
  if(current&&row)current.bytes.push(...row[1].trim().split(/\s+/));
 }
 if(current)buffers.push(current);
 if(buffers.length!==2||buffers.some(b=>b.length!==b.bytes.length||b.length===0)||JSON.stringify(buffers[0])!==JSON.stringify(buffers[1]))throw Error('whole CRS equivalence '+out);
 if(mode==='self'&&!/Received a Device Notify on \[TPD0\].*Value 0x80/.test(output))throw Error('self Notify');
 runs.at(-1).typedValues={integers,buffers};
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Local HID ACPICA references passed; '+out);
