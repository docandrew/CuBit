import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {spawnSync} from 'node:child_process';
const here=import.meta.dirname,sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),arg=process.argv[2];
if(process.argv.length!==3||!path.isAbsolute(arg)||fs.realpathSync(arg)!==arg)throw Error('Usage: node general-oracle.mjs ABS_RESULT_JSON');
const r=JSON.parse(fs.readFileSync(arg)),catalog=JSON.parse(fs.readFileSync(here+'/sources.json')),tool=catalog.tools.acpiexec,commands='execute _GPE._L09; evaluate DREG',expected=['1','2','34'],runs=[];
if(!r.accepted||r.nativeExecuted||arg!==r.out+'/RESULT.json'||Object.keys(r.counts).length!==36)throw Error('Current hosted prerequisite missing');
const self=catalog.helpers.find(x=>x.path==='general-oracle.mjs');if(!self||sha(import.meta.filename)!==self.sha256||sha(tool.path)!==tool.sha256||r.pins[tool.path]!==tool.sha256)throw Error('Oracle/tool pin mismatch');
const out=fs.mkdtempSync(r.out+'/general-oracle-');
for(const width of [1,2])for(const mode of ['general','general-no-ec']){
 const fixture=r.out+'/'+mode+width+'.aml';if(sha(fixture)!==r.pins[fixture]||sha(tool.path)!==tool.sha256)throw Error('Fixture/tool drift');
 const p=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5','prlimit','--as=134217728','--stack=2097152','--core=0',tool.path,'-l','-di','-dr','-b',commands,fixture],{encoding:'utf8',timeout:8000,maxBuffer:1048576}),text=(p.stdout??'')+(p.stderr??''),log=out+'/'+mode+'-'+width+'.log';
 fs.writeFileSync(log,text);const integers=[...text.matchAll(/\[Integer\]\s*=\s*([0-9A-Fa-f]+)/g)].map(m=>BigInt('0x'+m[1]).toString());
 const pass=p.status===0&&!p.signal&&!p.error&&!/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(text)&&JSON.stringify(integers)===JSON.stringify(expected);
 runs.push({mode,width,status:p.status,signal:p.signal,error:p.error?.message??null,integers,expected,pass,log,sha256:sha(log),fixture:{path:fixture,sha256:sha(fixture)}});
}
const result={accepted:runs.length===4&&runs.every(x=>x.pass),out,source:{path:import.meta.filename,sha256:sha(import.meta.filename)},tool,runs,commands,scope:'Independent repair-disabled ACPICA confirms level-handler package result1/2 and descriptor34, with/without EC at AML32/64. No host routing, IRQ, physical ordering, native launch, clean teardown or proof claim.',newProof:false,nativeExecuted:false,hardwareValidated:false};
fs.writeFileSync(out+'/RESULT.json',JSON.stringify(result,null,2)+'\n');console.log(JSON.stringify({accepted:result.accepted,out,runs:runs.length}));if(!result.accepted)process.exit(1);
