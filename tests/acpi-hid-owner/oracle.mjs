import fs from 'node:fs';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const sha=f=>crypto.createHash('sha256').update(fs.readFileSync(f)).digest('hex'),aml=process.argv[2],tool='/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec';
if(!aml||process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Fixture and TMPDIR required');
const out=fs.mkdtempSync(process.env.TMPDIR+'/hid-owner-oracle-'),pins={[aml]:sha(aml),[tool]:sha(tool),[process.argv[1]]:sha(process.argv[1])};
const args=[tool,'-b','evaluate TPD0._CID; evaluate TPD0._STA; evaluate TPD0._CRS',aml],r=spawnSync('nice',['-n','19','timeout','--kill-after=2s','5',...args],{encoding:'utf8',maxBuffer:1048576,timeout:8000});
const log=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/acpica.log',log.slice(0,1048576));for(const[f,h]of Object.entries(pins))if(sha(f)!==h)throw Error('Input drift');
const section=log.slice(log.indexOf('Evaluating \\TPD0._CRS')),m=/\[Buffer\] Length ([0-9A-F]+) =/.exec(section);const data=[];
for(const row of section.matchAll(/(?:^|\n|=)\s*([0-9A-F]{4}): ([^\n]+)/g)){
 if(Number.parseInt(row[1],16)!==data.length)throw Error('Discontinuous buffer');const hex=row[2].split('//')[0].trim();for(const v of hex.split(/\s+/)){if(!/^[0-9A-F]{2}$/.test(v))throw Error('Bad byte');data.push(Number.parseInt(v,16));}
}
const integers=[...log.matchAll(/^\s*\[Integer\] = ([0-9A-F]+)/gm)].map(m=>Number.parseInt(m[1],16)),diagnostics=log.split('\n').filter(s=>/ACPI Error/.test(s));
const recognized_teardown=diagnostics.length===1&&/^ACPI Error: 14 \(0xE\) Outstanding cache allocations \(20260408\/uttrack-902\)$/.test(diagnostics[0]);
const bytes=Buffer.from(data),semantic_match=r.status===0&&!r.error&&!r.signal&&Buffer.byteLength(log)<=1048576&&!/Evaluation of .* failed|Aborting method|Could not resolve symbol/.test(log)&&/"PNP0C50"/.test(log)&&JSON.stringify(integers)==='[15]'&&m&&Number.parseInt(m[1],16)===data.length&&data.length>40&&bytes.includes(Buffer.from('\\I2C0\0'))&&bytes.includes(Buffer.from('\\GPIO\0'))&&bytes.subarray(-2).equals(Buffer.from([0x79,0]));
const accepted=Boolean(semantic_match&&(diagnostics.length===0||recognized_teardown));
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({accepted,semantic_match:Boolean(semantic_match),clean_tool_log:diagnostics.length===0,diagnostics,recognized_teardown,integers,resource_hex:bytes.toString('hex'),args,status:r.status,signal:r.signal,error:r.error?.message??null,inputs:pins,scope:'Independent exact-fixture CID/STA/CRS observations only; not DSM, service bridge, controller resolution, provider authority or native/hardware acceptance. Exit diagnostic explicitly qualified.'},null,2)+'\n');console.log({out,accepted,integers,bytes:data.length,diagnostics});if(!accepted)process.exit(1);
