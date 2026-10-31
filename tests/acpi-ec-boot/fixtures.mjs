import fs from 'node:fs';
const dir=process.argv[2],text=fs.readFileSync(dir+'/ecdt-template.asl','utf8');
if(!text.includes('Namepath : ""'))throw Error('Template changed');
const named=text.replace('Namepath : ""','Namepath : "\\_SB.PCI0.LPCB.EC0"');
fs.writeFileSync(dir+'/io.asl',named);
fs.writeFileSync(dir+'/memory.asl',named.replaceAll('Space ID : 01','Space ID : 00').replace('0000000000000066','FEDCBA9876543267').replace('0000000000000062','0123456789ABCDE3').replace('UID : 00000000','UID : FEEDBEEF').replace('GPE Number : 09','GPE Number : FF'));
