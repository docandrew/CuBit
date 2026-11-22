import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
if(process.env.TMPDIR!=='/home/doc/cubit-build-tmp')throw Error('Export required TMPDIR before Nix');
const catalogPath=import.meta.dirname+'/sources.json',catalog=JSON.parse(fs.readFileSync(catalogPath)),root=path.resolve(import.meta.dirname,'../..');
for(const row of catalog.inputs){const h=crypto.createHash('sha256').update(fs.readFileSync(root+'/'+row.path)).digest('hex');if(h!==row.sha256)throw Error('source mismatch '+row.path);}
const d=import.meta.dirname,tree=root,out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-ec-retained-observations-'),source=out+'/source',pins={},runs=[];
fs.mkdirSync(source);pins[catalogPath]=crypto.createHash('sha256').update(fs.readFileSync(catalogPath)).digest('hex');for(const row of catalog.inputs)pins[root+'/'+row.path]=row.sha256;const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
function put(n,s){fs.writeFileSync(source+'/'+n,s);pins[source+'/'+n]=sha(source+'/'+n);}
for(const rel of [...['acpi_ec_types.ads','acpi_ec_transactions.ads','acpi_ec_transactions.adb','acpi_local_ec_commands.ads','acpi_local_ec_commands.adb'].map(n=>'userspace/services/acpi/ec/'+n),...['acpi_native_diagnostic_output.ads','acpi_native_diagnostic_output.adb'].map(n=>'userspace/services/acpi/native/'+n)]){const p=tree+'/'+rel;pins[p]=sha(p);put(path.basename(p),fs.readFileSync(p));}
put('cubit.ads','package CuBit with Pure is end CuBit;');
put('output_model.ads',`package Output_Model is Logger_Calls,Serial_Calls : Natural:=0; Last_Quiet,Last_Success : Boolean:=False; end Output_Model;`);
put('cubit-messages.ads','package CuBit.Messages is procedure debugPrint(Text : String); end CuBit.Messages;');
put('cubit-messages.adb',`with Output_Model; package body CuBit.Messages is
 procedure debugPrint(Text : String) is begin
 if Text/="copied transport=SERIAL_ONLY" & ASCII.LF then raise Program_Error; end if;
 Output_Model.Serial_Calls:=Output_Model.Serial_Calls+1;
 end debugPrint;
end CuBit.Messages;`);
put('acpi_native_log.ads','package ACPI_Native_Log is procedure Copied(Text : String; Peer_Quiet : Boolean; Success : Boolean:=False); end ACPI_Native_Log;');
put('acpi_native_log.adb',`with Output_Model; package body ACPI_Native_Log is
 procedure Copied(Text : String; Peer_Quiet : Boolean; Success : Boolean:=False) is begin if Text/="copied" then raise Program_Error; end if; Output_Model.Logger_Calls:=Output_Model.Logger_Calls+1; Output_Model.Last_Quiet:=Peer_Quiet; Output_Model.Last_Success:=Success; end Copied;
end ACPI_Native_Log;`);
put('observation_tests.adb',`with Ada.Text_IO; with ACPI_Local_EC_Commands; with ACPI_EC_Types; with ACPI_Native_Diagnostic_Output; with Output_Model;
procedure Observation_Tests is
 package T renames ACPI_EC_Types;
 use type T.Byte,T.Microseconds,T.Work_Budget,T.Outcome,T.Engine_Phase,T.Transaction_Observation,T.Read_Register,T.Write_Register,T.Operation;
 Checks,Active_Mode : Natural:=0;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with "mode" & Active_Mode'Image & " check" & Checks'Image; end if; end Check;
 procedure Scenario(Mode : Positive) is
 Reads,Writes,Clocks,Leaves : Natural:=0;
 Guard,Data_Read : Boolean:=False;
 Now : T.Microseconds:=0;
 procedure Read(Register : T.Read_Register; Value : out T.Byte; Result : out T.Access_Result) is
 begin
 Reads:=Reads+1; Result:=T.Completed; Value:=0;
 if Mode=9 then Result:=T.Refused; return;
 elsif Mode=10 then Result:=T.Indeterminate; return; end if;
 if Register=T.Data_Register then Value:=55; Data_Read:=True; return; end if;
 if Mode=1 then Value:=T.Burst_Mode or T.Event_Pending;
 elsif Mode in 2 | 3 then Value:=T.Input_Full or T.Event_Pending;
 elsif Mode=5 and Writes>=2 and not Data_Read then Value:=T.Output_Full;
 elsif Mode=8 then
 if Writes=0 then Value:=T.Event_Pending;
 elsif not Data_Read then Value:=T.Output_Full; end if;
 end if;
 end Read;
 procedure Write(Register : T.Write_Register; Value : T.Byte; Result : out T.Access_Result) is
 begin
 Writes:=Writes+1; Check(Guard);
 if Register=T.Command_Register then Check(Value=(if Mode=6 then T.Write_Command elsif Mode=8 then T.Query_Command else T.Read_Command)); end if;
 Result:=(if Mode=14 then T.Refused else T.Completed);
 end Write;
 procedure Clock(Value : out T.Microseconds; Available : out Boolean) is
 begin Clocks:=Clocks+1; Available:=Mode/=11;
 if Mode=12 then Value:=(if Clocks=1 then 10 else 9);
 else Value:=Now; Now:=Now+1; end if;
 end Clock;
 procedure Enter_Command(For_Query,For_Write : Boolean; Deadline : T.Microseconds; Accepted : out Boolean) is
 pragma Unreferenced(For_Query,For_Write,Deadline);
 begin Guard:=True; Accepted:=True; end Enter_Command;
 procedure Leave_Command(Released : out Boolean) is begin Leaves:=Leaves+1; Guard:=False; Released:=True; end Leave_Command;
 function Command_Quiet return Boolean is (not Guard);
 function Backing_Owned return Boolean is (Guard);
 package C is new ACPI_Local_EC_Commands(Read,Write,Clock,Enter_Command,Leave_Command,Command_Quiet,Backing_Owned);
 use type C.Start_Result,C.Phase;
 Result : C.Start_Result; Token : C.Token_ID; Accepted : Boolean;
 Completion : C.Completion;
 Item,Copy : T.Transaction_Observation;
 R,W,K : Natural;
 begin
 Active_Mode:=Mode;
 C.Start((if Mode in 7 | 8 then T.Query_Event elsif Mode=6 then T.Write_Cell else T.Read_Cell),17,42,
 (if Mode in 2 | 3 then 3 else 30),(if Mode=13 then 1 else 100),Result,Token); Check(Result=C.Started);
 if Mode=15 then C.Request_Stop(Accepted); Check(Accepted); end if;
 for Turn in 1 .. 64 loop
 R:=Reads; W:=Writes; K:=Clocks; Item:=C.Engine_Observation; Copy:=C.Engine_Observation;
 Check(Item=Copy and Reads=R and Writes=W and Clocks=K);
 exit when C.Observe.Stage/=C.Running;
 C.Advance;
 end loop;
 Check(C.Observe.Stage/=C.Running);
 Item:=C.Engine_Observation;
 Check(Item.Deadline=(if Mode in 2 | 3 then 3 else 30));
 Check(Item.Stage=T.Dormant and Item.Last_Active/=T.Dormant);
 if Mode=1 then Check(Item.Status=T.Burst_Active and Item.Last_Status=(T.Burst_Mode or T.Event_Pending) and Item.Recovery);
 elsif Mode in 2 | 3 then Check(Item.Status=T.Deadline_Reached and Item.Last_Active=T.Initial_Status and Item.Last_Sample=3 and Item.Recovery);
 elsif Mode=4 then Check(Item.Status=T.Deadline_Reached and Item.Effects_Possible and Item.Recovery and Item.Last_Active=T.Wait_Output);
 elsif Mode in 5 | 6 | 8 then Check(Item.Status=T.Succeeded and not Item.Recovery);
 elsif Mode=7 then Check(Item.Status=T.No_Event and not Item.Effects_Possible);
 elsif Mode in 9 | 14 then Check(Item.Status=T.Access_Refused);
 elsif Mode=10 then Check(Item.Status=T.Access_Unknown and Item.Recovery and not Item.Status_Known and Item.Status_Attempted);
 elsif Mode=11 then Check(Item.Status=T.Clock_Unavailable and not Item.Sample_Available and Reads=0 and Writes=0);
 elsif Mode=12 then Check(Item.Status=T.Clock_Reversed and Item.Last_Sample=9 and Item.Last_Time=10);
 elsif Mode=13 then Check(Item.Status=T.Budget_Exhausted and Item.Remaining=0 and Writes=0);
 elsif Mode=15 then Check(Item.Status=T.Stopped and Reads=0 and Writes=0 and Item.Stopping);
 end if;
 R:=Reads; W:=Writes; K:=Clocks; Copy:=C.Engine_Observation;
 Check(Item=Copy and Reads=R and Writes=W and Clocks=K);
 C.Collect(Completion); Check(Completion.Present);
 if Item.Recovery then Check(not C.Quiescent and Guard and Leaves=0 and C.Observe.Stage=C.Retained);
 else Check(C.Quiescent and not Guard and Leaves=1); end if;
 R:=Reads; W:=Writes; K:=Clocks;
 Copy:=C.Engine_Observation; C.Reconcile;
 Check(Reads=R and Writes=W and Clocks=K);
 end Scenario;
begin
 for Mode in 1 .. 15 loop Scenario(Mode); end loop;
 for Peer_Quiet in Boolean loop for Success in Boolean loop
 Output_Model.Logger_Calls:=0; Output_Model.Serial_Calls:=0;
 ACPI_Native_Diagnostic_Output.Write("copied",Peer_Quiet,Success);
 Check(Output_Model.Logger_Calls=1);
 Check(Output_Model.Serial_Calls=0);
 Check(Output_Model.Last_Quiet=Peer_Quiet and Output_Model.Last_Success=Success);
 end loop; end loop;
 Ada.Text_IO.Put_Line("EC observation checks" & Checks'Image);
end Observation_Tests;`);
// Test the COMPLETE actual ACPI logger separately, with a publisher model
// that detects attempts to open at an unsafe boundary. No runtime source edit.
function lp(n,s){const p=out+'/logger-source/'+n;fs.mkdirSync(path.dirname(p),{recursive:true});fs.writeFileSync(p,s);pins[p]=sha(p);}
for(const n of ['acpi_native_log.ads','acpi_native_log.adb']){const p=tree+'/userspace/services/acpi/native/'+n;pins[p]=sha(p);lp(n,fs.readFileSync(p));}
lp('cubit.ads','package CuBit with Pure is end CuBit;');
lp('cubit-control_events.ads','package CuBit.Control_Events is type Event is null record; end CuBit.Control_Events;');
lp('cubit-process_ids.ads',`with Interfaces; package CuBit.Process_IDs is type Process_ID is new Interfaces.Unsigned_64; No_Process : constant Process_ID:=0; function From_Word(W : Interfaces.Unsigned_64) return Process_ID is (Process_ID(W)); function Is_Process(P : Process_ID) return Boolean is (P/=0); end CuBit.Process_IDs;`);
lp('cubit-handles.ads',`with Interfaces; package CuBit.Handles is package ABI is type Kind is (No_Capability,Endpoint); end ABI; type Endpoint is new Interfaces.Unsigned_64; end CuBit.Handles;`);
lp('cubit-handles-ipc.ads',`with Interfaces; package CuBit.Handles.IPC is type Description(Current : Boolean:=False) is record case Current is when True=>Kind : ABI.Kind:=ABI.Endpoint; Object : Interfaces.Unsigned_64:=16#2_0000_0009#; when False=>null; end case; end record; function Describe(Item : Endpoint) return Description; end CuBit.Handles.IPC;`);
lp('cubit-handles-ipc.adb',`package body CuBit.Handles.IPC is function Describe(Item : Endpoint) return Description is pragma Unreferenced(Item); begin return (Current=>True,others=><>); end Describe; end CuBit.Handles.IPC;`);
lp('startup.ads','with CuBit.Handles; package Startup is function Logstore return CuBit.Handles.Endpoint is (77); end Startup;');
lp('logger_model.ads',`package Logger_Model is Opens,Emits,Debugs,Flushes,Disconnects : Natural:=0; Forbid_Open,Shed,Related : Boolean:=False; Open_Allowed : Boolean:=True; end Logger_Model;`);
lp('cubit-messages.ads','package CuBit.Messages is procedure debugPrint(Text : String); end CuBit.Messages;');
lp('cubit-messages.adb',`with Logger_Model; package body CuBit.Messages is procedure debugPrint(Text : String) is begin if Text'Length=0 or else Text(Text'Last)/=ASCII.LF then raise Program_Error; end if; Logger_Model.Debugs:=Logger_Model.Debugs+1; end debugPrint; end CuBit.Messages;`);
lp('cubit-log_records.ads',`package CuBit.Log_Records is type Severity is (Information,Warning,Critical); type Log_Record is null record; type Decoded(Success : Boolean:=False) is record case Success is when True=>Value : Log_Record; when False=>null; end case; end record; function Make(Text : String; Level : Severity) return Decoded; end CuBit.Log_Records;`);
lp('cubit-log_records.adb',`package body CuBit.Log_Records is function Make(Text : String; Level : Severity) return Decoded is pragma Unreferenced(Level); begin return (if Text'Length<=512 then (Success=>True,Value=>(null record)) else (Success=>False)); end Make; end CuBit.Log_Records;`);
lp('acpi_log_lifetime.ads',`with CuBit.Process_IDs; with CuBit.Control_Events; package ACPI_Log_Lifetime is function Related(Peer : CuBit.Process_IDs.Process_ID; Item : CuBit.Control_Events.Event) return Boolean; end ACPI_Log_Lifetime;`);
lp('acpi_log_lifetime.adb',`with Logger_Model; package body ACPI_Log_Lifetime is function Related(Peer : CuBit.Process_IDs.Process_ID; Item : CuBit.Control_Events.Event) return Boolean is pragma Unreferenced(Peer,Item); begin return Logger_Model.Related; end Related; end ACPI_Log_Lifetime;`);
lp('cubit-logging.ads',`with Interfaces; with CuBit.Handles; with CuBit.Log_Records; package CuBit.Logging is
 package Logstore_Handles is function Handle(Item : CuBit.Handles.Endpoint) return CuBit.Handles.Endpoint is (Item); end Logstore_Handles;
 type State is (Uninitialized,Ready,Disabled); type Publisher is limited record Stage : State:=Uninitialized; end record;
 function Bound_To(Target : CuBit.Handles.Endpoint) return Publisher;
 procedure Emit(Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Submitted : out Boolean);
 procedure Flush(Item : in out Publisher; Drained : out Boolean; Wait_Ms : Natural);
 procedure Disconnect(Item : in out Publisher; Done : out Boolean);
end CuBit.Logging;`);
lp('cubit-logging.adb',`with Logger_Model; package body CuBit.Logging is
 function Bound_To(Target : CuBit.Handles.Endpoint) return Publisher is pragma Unreferenced(Target); begin return (Stage=>Uninitialized); end Bound_To;
 procedure Emit(Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Submitted : out Boolean) is pragma Unreferenced(Value); begin
 Logger_Model.Emits:=Logger_Model.Emits+1;
 if Item.Stage=Uninitialized then if Logger_Model.Forbid_Open then raise Program_Error with "unsafe open"; end if; Logger_Model.Opens:=Logger_Model.Opens+1; Item.Stage:=(if Logger_Model.Open_Allowed then Ready else Disabled); end if;
 Submitted:=Item.Stage=Ready and not Logger_Model.Shed;
 end Emit;
 procedure Flush(Item : in out Publisher; Drained : out Boolean; Wait_Ms : Natural) is pragma Unreferenced(Item,Wait_Ms); begin Logger_Model.Flushes:=Logger_Model.Flushes+1; Drained:=True; end Flush;
 procedure Disconnect(Item : in out Publisher; Done : out Boolean) is begin Logger_Model.Disconnects:=Logger_Model.Disconnects+1; Item.Stage:=Disabled; Done:=True; end Disconnect;
end CuBit.Logging;`);
lp('logger_tests.adb',`with Ada.Command_Line; with Ada.Text_IO; with Interfaces; with ACPI_Native_Log; with Logger_Model;
procedure Logger_Tests is package N renames ACPI_Native_Log; package M renames Logger_Model; use type Interfaces.Unsigned_64;
 Checks : Natural:=0; procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
 Mode : constant String:=Ada.Command_Line.Argument(1);
begin
 M.Forbid_Open:=True; N.Copied("before",False); Check(M.Opens=0 and M.Emits=0 and N.Copied_Dropped=1);
 M.Forbid_Open:=False;
 if Mode="disabled" then M.Open_Allowed:=False; end if;
 if Mode="copied-first" then N.Copied("start",True); else N.Info("start"); end if;
 Check(M.Opens=1 and M.Emits=1);
 M.Forbid_Open:=True; N.Copied("retained",False); Check(M.Opens=1 and M.Emits=2 and M.Flushes=0);
 if Mode="disabled" then Check(N.Copied_Dropped=2);
 else
 Check(N.Copied_Dropped=1);
 M.Shed:=True; N.Copied("full",False); Check(M.Opens=1 and M.Emits=3 and N.Copied_Dropped=2);
 M.Shed:=False; N.Copied(String'[1 .. 513=>'x'],False); Check(M.Emits=3 and N.Copied_Dropped=3);
 M.Related:=True; N.Grant_Notice((null record)); Check(M.Disconnects=1);
 N.Copied("closed",False); Check(M.Emits=3 and N.Copied_Dropped=4 and M.Opens=1);
 end if;
 Check(M.Flushes=0);
 Ada.Text_IO.Put_Line("Copied logger checks" & Checks'Image);
end Logger_Tests;`);
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('source drift '+p);}
function run(label,argv,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...argv],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:false,out,pins,runs},null,2)+'\n');if(r.status!==0||r.error||r.signal)throw Error(label+' '+out);verify();return output;}
for(const mode of ['checked','release']){
 const p=out+'/'+mode+'.gpr';fs.writeFileSync(p,`project ${mode} is for Source_Dirs use ("source"); for Object_Dir use "${mode}/obj"; for Exec_Dir use "${mode}/bin"; for Main use ("observation_tests.adb"); package Compiler is for Default_Switches("Ada") use ("-O1","-gnat2022",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'}); end Compiler; end ${mode};\n`);pins[p]=sha(p);
 run(mode+'-build',['gprbuild','-p','-j1','-P',p],180);run(mode+'-test',[out+'/'+mode+'/bin/observation_tests'],30);
 const logger=out+'/logger_'+mode+'.gpr';fs.writeFileSync(logger,`project Logger_${mode} is for Source_Dirs use ("logger-source"); for Object_Dir use "logger-${mode}/obj"; for Exec_Dir use "logger-${mode}/bin"; for Main use ("logger_tests.adb"); package Compiler is for Default_Switches("Ada") use ("-O1","-gnat2022",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'}); end Compiler; end Logger_${mode};\n`);pins[logger]=sha(logger);
 run(mode+'-logger-build',['gprbuild','-p','-j1','-P',logger],180);
 for(const name of ['normal','copied-first','disabled'])run(mode+'-logger-'+name,[out+'/logger-'+mode+'/bin/logger_tests',name],30);
}
const outputs=runs.filter(r=>r.label.endsWith('-test')).map(r=>r.output);if(outputs[0]!==outputs[1]||!/^EC observation checks [0-9]+\n$/.test(outputs[0]))throw Error('verdict');
const loggerOutputs=runs.filter(r=>r.label.includes('-logger-')&&!r.label.endsWith('-build')).map(r=>r.output);
if(outputs[0]!=='EC observation checks 257\n'||JSON.stringify(loggerOutputs.slice(0,3))!==JSON.stringify(['Copied logger checks 9\n','Copied logger checks 9\n','Copied logger checks 5\n'])||loggerOutputs.length!==6||loggerOutputs.slice(0,3).join()!==loggerOutputs.slice(3).join())throw Error('logger verdict');
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:true,out,pins,runs,cases:15,checksPerProfile:Number(outputs[0].match(/[0-9]+/)[0]),loggerCases:3,loggerChecksPerProfile:loggerOutputs.slice(0,3).reduce((n,s)=>n+Number(s.match(/[0-9]+/)[0]),0),newProof:false,nativeExecuted:false,hardwareValidated:false,scope:'Actual EC transaction engine/command owner, observation getters and diagnostic sink plus complete ACPI logger. Modeled register/clock/guard and runtime publisher callbacks; bounded ring behavior additionally audited in current runtime sources.'},null,2)+'\n');console.log(JSON.stringify({out,cases:15,checks:outputs[0].trim(),logger:loggerOutputs.slice(0,3)}));
