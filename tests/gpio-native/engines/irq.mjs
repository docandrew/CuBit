import fs from 'node:fs';import {spawnSync} from 'node:child_process';
const stage=process.env.GPIO_CASE_ROOT,out=fs.mkdtempSync(stage+'/run-'),commands=[],counts={};
fs.cpSync(stage+'/source',out+'/source',{recursive:true});console.log(out);
function run(label,args,seconds){const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...args],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576});const text=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',text);commands.push({label,status:r.status,signal:r.signal,error:r.error?.message??null});fs.writeFileSync(out+'/commands.json',JSON.stringify(commands,null,2)+'\n');if(r.status!==0||r.signal||r.error)throw Error(label);return text;}
for(const profile of ['checked','release']){
 const flags=['-gnat2022','-gnatwa','-gnatwe','-O0',...(profile==='checked'?['-gnata','-gnato']:['-gnatp'])];
 fs.writeFileSync(out+'/test.gpr','project Test is for Source_Dirs use ("source");for Main use ("notice_tests.adb","dispatcher_tests.adb","irq_turn_tests.adb");for Object_Dir use "'+profile+'/obj";for Exec_Dir use "'+profile+'/bin";package Compiler is for Default_Switches ("Ada") use ('+flags.map(f=>'"'+f+'"').join(',')+');end Compiler;end Test;');
 run(profile+'-build',['gprbuild','-q','-p','-j1','-P',out+'/test.gpr'],180);counts[profile]={};
 for(const [main,expected]of [['notice_tests',264],['dispatcher_tests',85],['irq_turn_tests',39]]){
  const text=run(profile+'-'+main,[out+'/'+profile+'/bin/'+main],30),match=/^GPIO (?:IRQ notice checks|dispatcher checks|IRQ turn PASS) (\d+)\n$/.exec(text);
  if(!match||Number(match[1])!==expected)throw Error('Unexpected verdict '+text);counts[profile][main]=expected;
 }
}
fs.writeFileSync(out+'/COMPLETE.json',JSON.stringify({counts,commands:commands.length,new_proof:false,native_executed:false,hardware_validated:false},null,2)+'\n');
