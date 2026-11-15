import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,fixture=d+'/cache.asl',out=fs.mkdtempSync(process.env.TMPDIR+'/status-cache-reference-'),runs=[],pins={};
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');pins[fixture]=hash(fixture);
function save(accepted){fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,out,pins,runs,scope:'Repair-disabled ACPICA references; an oracle-only method resets fixture counters after loader initialization before explicit first and later status observations. No cache-policy or hardware validation.'},null,2)+'\n');}
function run(label,args){const r=spawnSync('timeout',['--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152',...args],{cwd:out,encoding:'utf8',timeout:8000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});save(false);if(r.status!==0||r.error||r.signal)throw Error(label);return output;}
for(const width of [1,2])for(const mode of ['ready','absent','disabled','broken','wrong']){
 const value=({ready:'0x0F',absent:'Zero',disabled:'0x0D',broken:'0x07',wrong:'Buffer () {1}'})[mode],prefix=out+'/'+mode+width;
 let s=fs.readFileSync(fixture,'utf8').replace('"DSDT", 2','"DSDT", '+width);
 for(const[tag,counter]of [['LID_STATUS','LSCT'],['POWER_STATUS','PSCT']])s=s.replace('/*'+tag+'*/','Method (_STA, 0) { Increment (\\'+counter+') If (LEqual (\\'+counter+', One)) { Return (0x0F) } Return ('+value+') }');
 s=s.replace(/}\s*$/,'Method (RSET, 0) { Store (Zero, \\LSCT) Store (Zero, \\PSCT) }\n}\n');
 if(mode==='wrong')s=s.replaceAll('Return (Buffer () {1})','CopyObject (Buffer () {1}, SVAL) Return (SVAL)');
 fs.writeFileSync(prefix+'.asl',s);pins[prefix+'.asl']=hash(prefix+'.asl');run('compile-'+mode+'-'+width,['iasl','-p',prefix,prefix+'.asl']);pins[prefix+'.aml']=hash(prefix+'.aml');
 const output=run('reference-'+mode+'-'+width,['acpiexec','-di','-dr','-b','execute RSET; evaluate LID0._STA; evaluate LID0._STA; evaluate AC00._STA; evaluate AC00._STA',prefix+'.aml']);
 const integers=[...output.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
 const expected=mode==='wrong'?['15','15']:['15',String({ready:15,absent:0,disabled:13,broken:7}[mode]),'15',String({ready:15,absent:0,disabled:13,broken:7}[mode])];
 if(JSON.stringify(integers)!==JSON.stringify(expected))throw Error('integer reference '+out);
 if(mode==='wrong'&&[...output.matchAll(/\[Buffer\] Length 01[^\n]*0000:\s+01/g)].length!==2)throw Error('buffer reference '+out);
 runs.at(-1).typedValues={integers,buffers:mode==='wrong'?['01','01']:[]};
}
for(const[p,h]of Object.entries(pins))if(hash(p)!==h)throw Error('drift');save(true);console.log('Status-cache ACPICA references passed; '+out);
