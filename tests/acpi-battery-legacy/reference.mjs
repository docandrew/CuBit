import fs from 'node:fs';import crypto from 'node:crypto';
const root=process.argv[2]??import.meta.dirname,hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex'),expectedNumbers=[1,42000,40000,1,11000,2000,1000,1,10].map(BigInt),expectedStrings=['CUBIT','SER1','LION',''],results=[];
for(const width of [1,2]){const p=root+'/REFERENCE-'+width+'.log',text=fs.readFileSync(p,'utf8'),marker='Evaluation of \\BAT0._BIF returned object';if(text.split(marker).length!==2)throw Error('missing or repeated BIF evaluation');
 const body=text.split(marker)[1];if(!/^\s*\[Package\] Contains 13 Elements:/m.test(body))throw Error('wrong package count');
 const numbers=[...body.matchAll(/^\s*\[Integer\] = ([0-9A-Fa-f]{16})\s*$/gm)].map(m=>BigInt('0x'+m[1]));
 const strings=[...body.matchAll(/^\s*\[String\] Length ([0-9A-Fa-f]+) = "([^"\r\n]*)"\s*$/gm)].map(m=>{if(parseInt(m[1],16)!==m[2].length)throw Error('string length mismatch');return m[2];});
 if(numbers.length!==9||strings.length!==4||numbers.some((v,i)=>v!==expectedNumbers[i])||strings.some((v,i)=>v!==expectedStrings[i]))throw Error('BIF reference mismatch');
 results.push({width,log:p,sha256:hash(p),typed_elements:13,numbers:numbers.map(n=>n.toString()),strings,cache_allocations:Number(/ACPI Error: (\d+).*Outstanding cache allocations/.exec(text)?.[1]??0)});
}
fs.writeFileSync(root+'/REFERENCE-RESULT.json',JSON.stringify({accepted:true,typed_elements:26,repair_disabled:true,results,scope:'Normal ACPICA package type/count and nine integers/four complete ASCII strings; valid literal scenario only, not malformed-input or ownership oracle'},null,2)+'\n');console.log('ACPICA exact typed elements matched: 26');
