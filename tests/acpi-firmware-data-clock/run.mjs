import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export TMPDIR before Nix');
const catalogPath=import.meta.dirname+'/sources.json',catalog=JSON.parse(fs.readFileSync(catalogPath)),root=path.resolve(import.meta.dirname,'../..');
for(const row of catalog.inputs){const h=crypto.createHash('sha256').update(fs.readFileSync(root+'/'+row.path)).digest('hex');if(h!==row.sha256)throw Error('source mismatch '+row.path);}
const d=import.meta.dirname,tree=root,base=root,rel='userspace/services/acpi/field-client/transport/acpi_firmware_data_frontend_native.adb';
const out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-firmware-clock-domain-'),pins={},runs=[];
const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
pins[catalogPath]=sha(catalogPath);for(const row of catalog.inputs)pins[root+'/'+row.path]=row.sha256;
function put(n,s){const p=out+'/changed/'+n;fs.mkdirSync(path.dirname(p),{recursive:true});fs.writeFileSync(p,s);pins[p]=sha(p);}
for(const n of ['acpi_firmware_data_frontend_native.ads','acpi_firmware_data_frontend_native.adb']){const p=tree+'/userspace/services/acpi/field-client/transport/'+n;pins[p]=sha(p);put(n,fs.readFileSync(p));}
put('cubit.ads','package CuBit with Pure is end CuBit;');
put('cubit-handles.ads',`with Interfaces; package CuBit.Handles is
 package ABI is type Kind is (No_Capability,Endpoint); end ABI;
 type Endpoint is new Interfaces.Unsigned_64; No_Endpoint : constant Endpoint := 0;
 type Rights is record Read : Boolean := True; end record;
end CuBit.Handles;`);
put('cubit-messages.ads',`with Interfaces; package CuBit.Messages is
 type MessageTag is record label : Interfaces.Unsigned_32 := 0; length,protocol,ipc : Interfaces.Unsigned_8 := 0; reserved : Interfaces.Unsigned_16 := 0; end record;
 type MessageWords is array(Natural range 0 .. 3) of Interfaces.Unsigned_64;
 type Message is record tag : MessageTag; words : MessageWords := [others=>0]; end record;
 NULL_MESSAGE : constant Message := (others=><>);
 Kernel_Reply_First : constant Interfaces.Unsigned_32 := 16#FFFF_0000#;
 function Flag_Byte(T : MessageTag) return Interfaces.Unsigned_8 is (T.protocol);
end CuBit.Messages;`);
put('clock_model.ads',`with Interfaces; package Clock_Model is
 Kernel_MS,HPET_US,Expected_Deadline : Interfaces.Unsigned_64 := 0;
 Micro_Available,Fail_Call,Carry : Boolean := False;
 Millisecond_Reads,Microsecond_Reads,Calls,Closes : Natural := 0;
 procedure Check(OK : Boolean);
end Clock_Model;`);
put('clock_model.adb',`package body Clock_Model is
 procedure Check(OK : Boolean) is begin if not OK then raise Program_Error with "boundary"; end if; end Check;
end Clock_Model;`);
put('cubit-monotonic.ads',`with Interfaces; package CuBit.Monotonic is
 type Reading(Available : Boolean := False) is record case Available is when True=>Microseconds : Interfaces.Unsigned_64; when False=>null; end case; end record;
 function Read return Reading; function Milliseconds return Interfaces.Unsigned_64;
end CuBit.Monotonic;`);
put('cubit-monotonic.adb',`with Clock_Model; package body CuBit.Monotonic is
 function Read return Reading is begin Clock_Model.Microsecond_Reads:=Clock_Model.Microsecond_Reads+1;
 if Clock_Model.Micro_Available then return (True,Clock_Model.HPET_US); else return (Available=>False); end if; end Read;
 function Milliseconds return Interfaces.Unsigned_64 is begin Clock_Model.Millisecond_Reads:=Clock_Model.Millisecond_Reads+1; return Clock_Model.Kernel_MS; end Milliseconds;
end CuBit.Monotonic;`);
put('acpi_firmware_data_replies.ads',`with Interfaces; package ACPI_Firmware_Data_Replies is
 package IPC is
 type Time_Instant is new Interfaces.Unsigned_64; No_Finite_Deadline : constant Time_Instant := Time_Instant'Last;
 type Words is array(Natural range 0 .. 3) of Interfaces.Unsigned_64;
 type Packet is record Label : Interfaces.Unsigned_32 := 0; Length : Interfaces.Unsigned_8 := 0; Flags : Interfaces.Unsigned_8 := 0; Reserved : Interfaces.Unsigned_16 := 0; Data : Words := [others=>0]; end record;
 type Delivery is (Not_Dispatched,Completion_Unknown,Reply_Received);
 end IPC;
 package F is
 Microseconds_Per_Millisecond : constant := 1000;
 type Decoded is record Valid : Boolean; end record;
 function Decode(Request : IPC.Packet; Reply : Boolean) return Decoded;
 end F;
end ACPI_Firmware_Data_Replies;`);
put('acpi_firmware_data_replies.adb',`package body ACPI_Firmware_Data_Replies is
 package body F is
 function Decode(Request : IPC.Packet; Reply : Boolean) return Decoded is
 pragma Unreferenced(Reply); use type Interfaces.Unsigned_8;
 begin return (Valid=>Request.Length/=0); end Decode;
 end F;
end ACPI_Firmware_Data_Replies;`);
put('acpi_register_roles.ads','with Interfaces; package ACPI_Register_Roles is Frontend_Tag : constant Interfaces.Unsigned_64 := 91; end ACPI_Register_Roles;');
put('acpi_native_startup_timing.ads',`package ACPI_Native_Startup_Timing is
 package P is type Phase is (Firmware_Data_RPC); end P; type Token is null record;
 procedure Enter(Phase : P.Phase; Item : out Token); procedure Leave(Item : in out Token);
end ACPI_Native_Startup_Timing;`);
put('acpi_native_startup_timing.adb',`package body ACPI_Native_Startup_Timing is
 procedure Enter(Phase : P.Phase; Item : out Token) is pragma Unreferenced(Phase); begin Item:=(null record); end Enter;
 procedure Leave(Item : in out Token) is pragma Unreferenced(Item); begin null; end Leave;
end ACPI_Native_Startup_Timing;`);
put('cubit-handles-ipc.ads',`with Interfaces; with CuBit.Messages; package CuBit.Handles.IPC is
 type Close_Result(Closed : Boolean := False) is record case Closed is when True=>null; when False=>Why : Natural:=0; end case; end record;
 type Carried_Kind is (Absent,Present,Refused);
 type Carried(Kind : Carried_Kind := Absent) is record case Kind is when Present=>Word : Interfaces.Unsigned_64:=0; when others=>null; end case; end record;
 type Description(Current : Boolean := False) is record case Current is when True=>Kind : ABI.Kind := ABI.Endpoint; Held : Rights; Authority_Tag : Interfaces.Unsigned_64:=91; when False=>null; end case; end record;
 function Describe(Item : Endpoint) return Description;
 function Call_Accepting(Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Interfaces.Unsigned_64; Received : out Carried) return CuBit.Messages.MessageTag;
end CuBit.Handles.IPC;`);
put('cubit-handles-ipc.adb',`with Clock_Model; package body CuBit.Handles.IPC is
 use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
 function Describe(Item : Endpoint) return Description is begin Clock_Model.Check(Item=77); return (Current=>True,others=><>); end Describe;
 function Call_Accepting(Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Interfaces.Unsigned_64; Received : out Carried) return CuBit.Messages.MessageTag is
 begin Clock_Model.Check(Target=77 and Deadline=Clock_Model.Expected_Deadline);
 Clock_Model.Calls:=Clock_Model.Calls+1;
 Received:=(if Clock_Model.Carry then (Present,16#1_0000_0007#) else (Kind=>Absent));
 Msg.tag.label:=(if Clock_Model.Fail_Call then CuBit.Messages.Kernel_Reply_First else Msg.tag.label);
 return Msg.tag; end Call_Accepting;
end CuBit.Handles.IPC;`);
put('clock_tests.adb',`with Ada.Text_IO; with Interfaces; with Clock_Model; with ACPI_Firmware_Data_Frontend_Native; with CuBit.Handles.IPC;
procedure Clock_Tests is
 package C renames Clock_Model; package H renames CuBit.Handles.IPC;
 function Close(Item : H.Carried) return H.Close_Result is
 pragma Unreferenced(Item);
 begin C.Closes:=C.Closes+1; return (Closed=>True); end Close;
 package Native is new ACPI_Firmware_Data_Frontend_Native(Close);
 package I renames Native.IPC;
 use type Interfaces.Unsigned_64, I.Time_Instant, I.Delivery, H.Carried_Kind;
 Checks : Natural:=0;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with "clock regression" & Checks'Image; end if; end Check;
 Now : I.Time_Instant; Available : Boolean;
 Samples : constant array(Positive range 1 .. 7) of Interfaces.Unsigned_64 := [0,1,999,1000,16#1_0000_3039#,Interfaces.Unsigned_64'Last-1,Interfaces.Unsigned_64'Last];
begin
 for Available_Case in 1 .. 2 loop
 for Sample of Samples loop
 C.Kernel_MS:=Sample; C.HPET_US:=9_000_000_000; C.Micro_Available:=Available_Case=1;
 C.Microsecond_Reads:=0; C.Millisecond_Reads:=0;
 Native.Clock(Now,Available);
 Check(Available=(Sample/=Interfaces.Unsigned_64'Last));
 Check(Now=(if Available then I.Time_Instant(Sample) else 0));
 Check(C.Millisecond_Reads=1 and C.Microsecond_Reads=0);
 end loop; end loop;
 for Mode in 1 .. 11 loop
 declare
 Channel : Native.Channel; Accepted,Closed : Boolean;
 Request : I.Packet := (Label=>1,Length=>4,others=><>); Reply : I.Packet;
 Received : H.Carried; Delivered : I.Delivery;
 Deadline : I.Time_Instant:=110;
 begin
 C.Kernel_MS:=100; C.HPET_US:=9_000_000_000; C.Micro_Available:=False;
 C.Calls:=0; C.Microsecond_Reads:=0; C.Millisecond_Reads:=0; C.Fail_Call:=False; C.Carry:=False;
 Native.Initialize(Channel,77,Accepted); Check(Accepted);
 case Mode is
 when 2=>C.Kernel_MS:=110;
 when 3=>C.Kernel_MS:=111;
 when 4=>C.Kernel_MS:=Interfaces.Unsigned_64'Last;
 when 5=>Deadline:=0;
 when 6=>Deadline:=I.No_Finite_Deadline;
 when 7=>Request.Length:=0;
 when 8=>C.Kernel_MS:=0; Deadline:=10;
 when 9=>C.Kernel_MS:=16#1_0000_0007#; Deadline:=I.Time_Instant(C.Kernel_MS+10);
 when 10=>C.Fail_Call:=True; C.Carry:=True;
 when 11=>C.Carry:=True;
 when others=>null;
 end case;
 C.Expected_Deadline:=Interfaces.Unsigned_64(Deadline);
 Native.Exchange(Channel,Deadline,Request,Reply,Received,Delivered);
 if Mode in 2 .. 7 then Check(C.Calls=0 and Delivered=I.Not_Dispatched);
 elsif Mode=10 then Check(C.Calls=1 and Delivered=I.Completion_Unknown);
 else Check(C.Calls=1 and Delivered=I.Reply_Received); end if;
 Check(C.Microsecond_Reads=0);
 if Mode in 10 | 11 then Check(Received.Kind=H.Present and Native.Present(Received));
 Native.Dispose(Received,Closed); Check(Closed and Received.Kind=H.Absent); end if;
 end; end loop;
 Ada.Text_IO.Put_Line("Firmware clock checks" & Checks'Image);
end Clock_Tests;`);
fs.cpSync(out+'/changed',out+'/baseline',{recursive:true});const original=d+'/fixtures/before/acpi_firmware_data_frontend_native.adb';pins[original]=sha(original);fs.copyFileSync(original,out+'/baseline/acpi_firmware_data_frontend_native.adb');
for(const n of fs.readdirSync(out+'/baseline'))pins[out+'/baseline/'+n]=sha(out+'/baseline/'+n);
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('drift '+p);}
function run(label,argv,seconds,expected=0){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...argv],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:false,out,pins,runs},null,2)+'\n');if(r.status!==expected||r.error||r.signal)throw Error(label+' '+out);verify();return output;}
for(const [variant,mode]of [['baseline','checked'],['changed','checked'],['changed','release']]){
 const name=variant+'-'+mode,project=name.replaceAll('-','_'),gpr=out+'/'+project+'.gpr';
 fs.writeFileSync(gpr,`project ${project} is\nfor Source_Dirs use ("${variant}"); for Object_Dir use "${name}/obj"; for Exec_Dir use "${name}/bin"; for Main use ("clock_tests.adb");\npackage Compiler is for Default_Switches("Ada") use ("-O1","-gnat2022",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'}); end Compiler; end ${project};\n`);pins[gpr]=sha(gpr);
 run(name+'-build',['gprbuild','-p','-j1','-P',gpr],180);
 const output=run(name+'-test',[out+'/'+name+'/bin/clock_tests'],30,variant==='baseline'?1:0);
 if(variant==='baseline'&&!output.includes('clock regression 2'))throw Error('baseline failed unexpectedly');
}
const output=runs.find(r=>r.label==='changed-checked-test').output;
if(output!=='Firmware clock checks 79\n'||output!==runs.find(r=>r.label==='changed-release-test').output||!/^Firmware clock checks [0-9]+\n$/.test(output))throw Error('verdict');
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:true,out,pins,runs,baselineExpectedFailure:true,checksPerProfile:Number(output.match(/[0-9]+/)[0]),scope:'Actual native firmware-data frontend with modeled clocks, endpoint, timing and frame decoding; deadline/clock epoch behavior and handle preservation only. Native SDK separately checks real dependencies.',newProof:false,nativeExecuted:false,hardwareValidated:false},null,2)+'\n');
console.log(JSON.stringify({out,baselineExpectedFailure:true,checks:output.trim()}));
