import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {spawnSync} from 'node:child_process';
const d=import.meta.dirname,tree=path.resolve(d,'../..'),out=fs.mkdtempSync(process.env.TMPDIR+'/acpi-region-cleanup-admission-'),source=out+'/source',pins={},runs=[];
fs.mkdirSync(source);const sha=p=>crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex');
function put(n,s){fs.writeFileSync(source+'/'+n,s);pins[source+'/'+n]=sha(source+'/'+n);}
for(const stem of ['acpi_bootstrap-with_region_cleanup','acpi_requests-with_region_cleanup','acpi_bootstrap-with_local_maintenance','acpi_requests-with_local_maintenance'])for(const ext of ['ads','adb']){const p=tree+'/userspace/services/acpi/'+stem+'.'+ext;pins[p]=sha(p);put(path.basename(p),fs.readFileSync(p));}
for(const n of ['acpi_device_policy.ads','acpi_device_policy.adb']){const p=tree+'/userspace/services/acpi/'+n;pins[p]=sha(p);put(n,fs.readFileSync(p));}
put('acpi_service.ads',`package ACPI_Service is type State is record Results,Notifications : Natural:=0; end record;
 function Pending_Notifications(Service : State) return Natural is (Service.Notifications);
end ACPI_Service;`);
put('acpi_bootstrap.ads',`with ACPI_Service; with ACPI_Device_Policy; package ACPI_Bootstrap is
 type Phase is (Receiving,Complete,Failed);
 type Device_Phase is (Devices_Not_Started,Devices_Preparing,Devices_Awaiting_Regions,Devices_Walking,Devices_Completed,Devices_Completed_With_Errors,Devices_Incomplete,Devices_Failed);
 type Device_Report is record Stage : Device_Phase:=Devices_Not_Started; Regions_Attempted : Boolean:=False; Region_Status : ACPI_Device_Policy.Region_Outcome:=ACPI_Device_Policy.Regions_Unavailable; end record;
 type State is record Stage : Phase:=Receiving; Device_Data : Device_Report; Notify_Enabled : Boolean:=False; Core : aliased ACPI_Service.State; end record;
 function Valid(Session : State) return Boolean is (Session.Core.Results<=1 and Session.Core.Notifications<=1);
 function Retained_Results_Pending(Session : State) return Boolean is (Session.Core.Results/=0);
end ACPI_Bootstrap;`);
put('acpi_requests.ads',`with ACPI_Service; with ACPI_Bootstrap; with Interfaces; package ACPI_Requests is
 use type Interfaces.Unsigned_64;
 type State is record Boot : ACPI_Bootstrap.State; Notifications : Boolean:=False; Version : Interfaces.Unsigned_64:=5; end record;
 function Valid(Server : State) return Boolean is (ACPI_Bootstrap.Valid(Server.Boot));
 function Revision(Server : State) return Interfaces.Unsigned_64 is (Server.Version);
 function Notifications_Pending(Server : State) return Boolean is (Server.Notifications);
end ACPI_Requests;`);
put('admission_tests.adb',`with Ada.Text_IO; with Interfaces; with ACPI_Service; with ACPI_Bootstrap; with ACPI_Requests; with ACPI_Requests.With_Region_Cleanup; with ACPI_Requests.With_Local_Maintenance; with ACPI_Device_Policy;
procedure Admission_Tests is
 package B renames ACPI_Bootstrap; package R renames ACPI_Requests; package P renames ACPI_Device_Policy;
 use type B.Phase,B.Device_Phase,P.Region_Outcome,Interfaces.Unsigned_64;
 Old_Calls,New_Calls,Checks : Natural:=0;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
 procedure Old_Step(Service : aliased in out ACPI_Service.State; Completed : out Boolean) is pragma Unreferenced(Service); begin Old_Calls:=Old_Calls+1; Completed:=True; end Old_Step;
 procedure New_Step(Service : aliased in out ACPI_Service.State; Completed : out Boolean) is pragma Unreferenced(Service); begin New_Calls:=New_Calls+1; Completed:=True; end New_Step;
 package Runtime is new R.With_Local_Maintenance(Old_Step);
 package Cleanup is new R.With_Region_Cleanup(New_Step);
 use type Runtime.Controller.Admission,Cleanup.Controller.Admission;
begin
 for Boot in B.Phase loop for Device in B.Device_Phase loop for Region in P.Region_Outcome loop
 for Attempted in Boolean loop for Enabled in Boolean loop for Core_Notify in 0 .. 1 loop
 for Results in 0 .. 1 loop for Server_Notify in Boolean loop
 declare
 S : R.State; Done : Boolean:=True; Old : Runtime.Controller.Admission; Newer : Cleanup.Controller.Admission;
 Old_Before : constant Natural:=Old_Calls; New_Before : constant Natural:=New_Calls;
 Ready : constant Boolean:=Boot=B.Complete and Attempted and Device in B.Devices_Completed | B.Devices_Completed_With_Errors | B.Devices_Incomplete | B.Devices_Failed;
 Expected : constant Boolean:=Ready and not Server_Notify and not (Enabled and Core_Notify/=0) and Results=0;
 Old_Expected : constant Boolean:=Boot=B.Complete and Device=B.Devices_Completed and not Server_Notify and not (Enabled and Core_Notify/=0) and Results=0;
 begin
 S.Boot.Stage:=Boot; S.Boot.Device_Data:=(Stage=>Device,Regions_Attempted=>Attempted,Region_Status=>Region);
 S.Boot.Notify_Enabled:=Enabled; S.Boot.Core:=(Results=>Results,Notifications=>Core_Notify); S.Notifications:=Server_Notify;
 Cleanup.Advance(S,Newer,Done);
 Check(Done=Expected and New_Calls=New_Before+(if Expected then 1 else 0));
 if Server_Notify then Check(Newer=Cleanup.Controller.Notifications_Pending);
 elsif not Ready then Check(Newer=Cleanup.Controller.Not_Ready);
 elsif Enabled and Core_Notify/=0 then Check(Newer=Cleanup.Controller.Notifications_Pending);
 elsif Results/=0 then Check(Newer=Cleanup.Controller.Results_Pending);
 else Check(Newer=Cleanup.Controller.Admitted); end if;
 Runtime.Advance(S,Old,Done);
 Check(Done=Old_Expected and Old_Calls=Old_Before+(if Old_Expected then 1 else 0));
 Check(S.Boot.Stage=Boot and S.Boot.Device_Data.Stage=Device and S.Boot.Device_Data.Region_Status=Region);
 Check(S.Boot.Core.Results=Results and S.Boot.Core.Notifications=Core_Notify and R.Revision(S)=5);
 end;
 end loop; end loop; end loop; end loop; end loop; end loop; end loop; end loop;
 Ada.Text_IO.Put_Line("Region cleanup admission checks" & Checks'Image);
end Admission_Tests;`);
function verify(){for(const[p,h]of Object.entries(pins))if(sha(p)!==h)throw Error('input drift');}
function run(label,argv,seconds){verify();const r=spawnSync('nice',['-n','19','timeout','--kill-after=2s',String(seconds),'prlimit','--as=25769803776',...argv],{cwd:out,encoding:'utf8',timeout:(seconds+3)*1000,maxBuffer:1048576}),output=(r.stdout??'')+(r.stderr??'');fs.writeFileSync(out+'/'+label+'.log',output);runs.push({label,status:r.status,error:r.error?.message??null,signal:r.signal,output});fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:false,out,pins,runs},null,2)+'\n');if(r.status!==0||r.error||r.signal)throw Error(label+' '+out);verify();return output;}
const outputs=[];for(const mode of ['checked','release']){const p=out+'/'+mode+'.gpr';fs.writeFileSync(p,`project ${mode} is for Source_Dirs use ("source"); for Object_Dir use "${mode}/obj"; for Exec_Dir use "${mode}/bin"; for Main use ("admission_tests.adb"); package Compiler is for Default_Switches("Ada") use ("-O1","-gnat2022",${mode==='checked'?'"-gnata","-gnato"':'"-gnatp"'}); end Compiler; end ${mode};\n`);pins[p]=sha(p);run(mode+'-build',['gprbuild','-p','-j1','-P',p],180);outputs.push(run(mode+'-test',[out+'/'+mode+'/bin/admission_tests'],30));}
if(outputs[0]!==outputs[1]||!/^Region cleanup admission checks [0-9]+\n$/.test(outputs[0]))throw Error('verdict');
fs.writeFileSync(out+'/RESULT.json',JSON.stringify({completed:true,out,pins,runs,checksPerProfile:Number(outputs[0].match(/[0-9]+/)[0]),newProof:false,nativeExecuted:false,hardwareValidated:false,scope:'Actual region-cleanup and unchanged runtime-maintenance generic bodies with modeled bootstrap/request/Core parent state. Exhaustive policy product, no actual AML/region effect or proof.'},null,2)+'\n');console.log(JSON.stringify({out,checks:outputs[0].trim()}));
