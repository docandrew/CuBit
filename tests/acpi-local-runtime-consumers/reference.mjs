import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,out=fs.mkdtempSync(process.env.TMPDIR+'/runtime-consumer-reference-'),runs=[],pins={};
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[d+'/runtime.asl']=hash(d+'/runtime.asl');
function save(completed){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed,out,pins,runs,scope:'ACPICA fixture reference values, repair disabled; not owner policy, hardware access or event authentication.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');
 fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
for(const width of [1,2]){
 const stem=out+'/fixture'+width;let s=fs.readFileSync(d+'/runtime.asl','utf8').replace('"DSDT", 2','"DSDT", '+width);
 s=s.replace(/}\s*$/,`    Method (OREG, 0) { Return (\\TPD0._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, One, Package () {})) }\n}\n`);
 fs.writeFileSync(stem+'.asl',s);pins[stem+'.asl']=hash(stem+'.asl');run('compile-'+width,['iasl','-p',stem,stem+'.asl']);pins[stem+'.aml']=hash(stem+'.aml');
 const output=run('reference-'+width,['acpiexec','-dr','-b','evaluate LID0._LID; evaluate AC00._PSR; evaluate BAT0._BST; execute EVT0; evaluate LID0._LID; evaluate AC00._PSR; evaluate BAT0._BST; evaluate TPD0._STA; execute OREG',stem+'.aml']);
 const values=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
 const expected=[1,1,1,100,3000,11000,0,0,1,100,2000,11000,15,32].map(String);
 if(JSON.stringify(values)!==JSON.stringify(expected))throw Error('typed reference mismatch '+out);runs.at(-1).typedValues=values;
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Consumer ACPICA values passed both widths; '+out);
