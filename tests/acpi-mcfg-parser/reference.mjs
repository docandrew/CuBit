import fs from 'node:fs';import crypto from 'node:crypto';import{spawnSync}from'node:child_process';
const root=import.meta.dirname,suite=root,test=JSON.parse(fs.readFileSync(suite+'/TEST-RESULT.json')),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-mcfg-oracle-'),commands=[];
if(!test.accepted)throw Error('tests incomplete');
const hash=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
for(const mode of ['checked','release']){const fixture=test.out+'/'+mode+'/fixture.dat',before=hash(fixture),prefix=out+'/mcfg-'+mode;
 const p=spawnSync('timeout',['--kill-after=2s','5','iasl','-d','-p',prefix,fixture],{encoding:'utf8',timeout:8000,maxBuffer:1048576});fs.writeFileSync(prefix+'.log',(p.stdout??'')+(p.stderr??''));
 if(p.status!==0||hash(fixture)!==before)throw Error('ACPICA disassembly failed or modified source');
 const dsl=fs.readFileSync(prefix+'.dsl','utf8');
 const expected=[['Reserved','0000000000000000'],['Base Address','123456789ABCDEF0'],['Segment Group Number','1234'],['Start Bus Number','40'],['End Bus Number','20'],['Reserved','89ABCDEF']];
 for(const [field,value] of expected) if(!dsl.split('\n').some(line=>line.trim().endsWith(field+' : '+value))) throw Error('ACPICA field mismatch '+field);
 commands.push({mode,status:p.status,fixture,fixture_sha256:before,disassembly:prefix+'.dsl',lines:dsl.split('\n').filter(l=>/Base Address|Segment|Bus|Reserved/.test(l))});
}
if(commands[0].fixture_sha256!==commands[1].fixture_sha256)throw Error('assertion profiles emitted different fixtures');
fs.writeFileSync(root+'/ORACLE-OBSERVATIONS.json',JSON.stringify({accepted:true,out,field_checks:12,commands,scope:'ACPICA raw MCFG field disassembly of the exact emitted hosted-test bytes; no AML execution or hardware authority'},null,2)+'\n');console.log(JSON.stringify({out,commands}));
