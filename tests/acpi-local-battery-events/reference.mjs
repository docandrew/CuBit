import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,fixture=d+'/battery.asl',out=fs.mkdtempSync(process.env.TMPDIR+'/local-battery-reference-'),runs=[],pins={};
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[fixture]=hash(fixture);
const x=[0,1,42000,40000,1,11000,2000,1000,0xFFFFFFFF,100000,0xFFFFFFFF,10,500,5,1,10],b=[1,42000,40000,1,11000,2000,1000,1,10],bst=[2,10,20000,11000],xs=['CUBIT','S1','LION',''],bs=['CUBIT','S2','LION',''];
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA literal BIX/BIF/BST, absence, presence-change and self-Notify fixture values. Not an inventory/cleanup/fallback policy oracle.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
for(const width of [1,2])for(const mode of ['mixed','absent','presence-changed','self']){
 const prefix=out+'/'+mode+width,text=fs.readFileSync(fixture,'utf8').replace('"DSDT", 2','"DSDT", '+width)
  .replace('/*SELF*/',mode==='self'?'One':'Zero').replace('/*CHNG*/',mode==='presence-changed'?'One':'Zero').replace('/*BAD_BIX*/','')
  .replace('/*HID0*/','PNP0C0A').replace('/*HID1*/','PNP0C0A').replace('/*STA1*/',mode==='absent'?'0x0F':'0x1F').replace('/*EXTRA*/','');
 fs.writeFileSync(prefix+'.asl',text);pins[prefix+'.asl']=hash(prefix+'.asl');run('compile-'+mode+'-'+width,['iasl','-p',prefix,prefix+'.asl']);pins[prefix+'.aml']=hash(prefix+'.aml');
 const command=mode==='mixed'?'evaluate BAT0._BIX; evaluate BAT0._BST; evaluate BAT1._BIF; evaluate BAT1._BST':mode==='absent'?'evaluate BAT1._STA':mode==='presence-changed'?'evaluate BAT0._STA; evaluate BAT0._BIX; evaluate BAT0._STA':'evaluate BAT0._BST; evaluate BAT0._BST; evaluate BCNT';
 const output=run('reference-'+mode+'-'+width,['acpiexec','-di','-dr','-b',command,prefix+'.aml']);
 const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString()),strings=[...output.matchAll(/\[String\] Length [0-9A-Fa-f]+ = "([^"\r\n]*)"/g)].map(m=>m[1]);
 const expected=(mode==='mixed'?[...x,...bst,...b,...bst]:mode==='absent'?[15]:mode==='presence-changed'?[31,...x,15]:[...bst,...bst,2]).map(String),texts=mode==='mixed'?[...xs,...bs]:mode==='presence-changed'?xs:[];
 if(JSON.stringify(integers)!==JSON.stringify(expected)||JSON.stringify(strings)!==JSON.stringify(texts))throw Error('typed battery reference '+out);
 if(mode==='self'&&!/Received a Device Notify on \[BAT0\].*Value 0x80/.test(output))throw Error('Notify reference');runs.at(-1).typedValues={integers,strings};
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Local battery ACPICA references passed; '+out);
