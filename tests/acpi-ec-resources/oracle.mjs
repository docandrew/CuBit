export function readReference(text){
 const names=['ECIO','ECFI','ECFM','ECMM','ECGI','ECXX','ECNO','ECIR','ECLM','ECAI','ECEX','ECDP'];
 const buffers={};
 for(const section of text.split(/^Evaluating /m).slice(1)){
  const newline=section.indexOf('\n'),name=section.slice(0,newline).trim();
  if(!name.endsWith('._CRS'))continue;
  const device=name.slice(1,5);if(!names.includes(device)||buffers[device])throw Error('Unexpected/duplicate reference '+name);
  const length=/\[Buffer\] Length ([0-9A-F]+) =/.exec(section);if(!length)throw Error('Missing buffer '+name);
  const expected=Number.parseInt(length[1],16),bytes=[];
  for(const row of section.matchAll(/(?:^|\n|=)\s*([0-9A-F]{4}): ([^\n]+)/g)){
   if(Number.parseInt(row[1],16)!==bytes.length)throw Error('Reference offset');
   const fields=row[2].split('//')[0].trim().split(/\s+/);
   if(!fields.length||fields.length>16||fields.some(s=>!/[0-9A-F]{2}/.test(s)||s.length!==2))throw Error('Reference bytes');
   bytes.push(...fields.map(s=>Number.parseInt(s,16)));
  }
  if(bytes.length!==expected)throw Error('Incomplete reference '+device);
  buffers[device]=Buffer.from(bytes).toString('hex').toUpperCase();
 }
 if(names.some(n=>!buffers[n])||Object.keys(buffers).length!==names.length)throw Error('Reference set');
 const integers=[...text.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16));
 if(JSON.stringify(integers)!==JSON.stringify([0x090CD041,1,9,17,1]))throw Error('Reference integers '+JSON.stringify(integers));
 const references=[...text.matchAll(/Name ([A-Z0-9_]{4}) Device/g)].map(m=>m[1]);
 if(JSON.stringify(references)!==JSON.stringify(['GPEB']))throw Error('Reference device');
 return {buffers,integers,references,shutdown_cache_note:text.includes('Outstanding cache allocations')};
}
