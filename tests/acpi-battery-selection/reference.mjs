import fs from 'node:fs';import crypto from 'node:crypto';
const root=process.argv[2];if(!root)throw Error('absolute result directory required');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),run=JSON.parse(fs.readFileSync(root+'/RESULT.json')),values=[
 {name:'\\EXT0._BIX',numbers:[0,1,42000,40000,1,11000,2000,1000,0xFFFFFFFF,100000,0xFFFFFFFF,10,500,5,1,10]},
 {name:'\\BAT0._BIF',numbers:[1,42000,40000,1,11000,2000,1000,1,10]}],strings=['CUBIT','SER1','LION',''],observations=[];
for(const width of [1,2]){const log=run.out+'/reference-'+width+'.log',text=fs.readFileSync(log,'utf8');
 for(let i=0;i<values.length;i++){const expected=values[i],marker='Evaluation of '+expected.name+' returned object';if(text.split(marker).length!==2)throw Error('missing/repeated evaluation '+expected.name);let body=text.split(marker)[1];if(i+1<values.length)body=body.split('Evaluation of '+values[i+1].name+' returned object')[0];
  if(!new RegExp('\\[Package\\] Contains '+(expected.numbers.length+strings.length)+' Elements:').test(body))throw Error('package shape');
  const numbers=[...body.matchAll(/^\s*\[Integer\] = ([0-9A-Fa-f]{16})\s*$/gm)].map(m=>BigInt('0x'+m[1]));
  const texts=[...body.matchAll(/^\s*\[String\] Length ([0-9A-Fa-f]+) = "([^"\r\n]*)"\s*$/gm)].map(m=>{if(parseInt(m[1],16)!==m[2].length)throw Error('text length');return m[2];});
  if(numbers.length!==expected.numbers.length||numbers.some((n,j)=>n!==BigInt(expected.numbers[j]))||texts.length!==4||texts.some((s,j)=>s!==strings[j]))throw Error('typed field mismatch '+expected.name);
  observations.push({width,name:expected.name,typed_elements:numbers.length+texts.length,log,sha256:hash(log)});
 }
}
fs.writeFileSync(root+'/REFERENCE-RESULT.json',JSON.stringify({accepted:true,typed_elements:66,repair_disabled:true,observations,scope:'Two valid firmware information packages in both AML widths, not a selector-policy/uncertainty/cleanup oracle; cache diagnostics retained in raw logs'},null,2)+'\n');console.log('Typed BIX/BIF reference elements matched: 66');
