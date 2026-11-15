import fs from 'node:fs';const d=import.meta.dirname;function put(n,s){fs.writeFileSync(d+'/'+n,s);}
put('probe_model.ads',`with Interfaces;package Probe_Model is
 type Test_Mode is (Good,No_Window,Unsafe,No_Clock,No_Deadline,Nonfinite,No_Intel,No_Class,Class_Refused,LGMR_Refused,Wide_Identity,Disabled,Expired,Shed);
 Selected:Test_Mode:=Good;Headers,Reads,Logs,Dropped:Natural:=0;
 Last_Text:String(1..2048):=[others=>' '];Last:Natural:=0;
 Scope:constant Interfaces.Unsigned_64:=16#ABCD_0000_0000_0001#;
 Epoch:constant Interfaces.Unsigned_64:=16#1234_5678_9876_4321#;
 ID:constant Interfaces.Unsigned_64:=16#FEDC_BA98_7654_3210#;
end Probe_Model;`);
put('acpi_one_service.ads',`with Interfaces;package ACPI_One_Service is
 Field_Scope:constant Interfaces.Unsigned_64:=16#ABCD_0000_0000_0001#;
 package Root is package Fields is
 function Control_Idle return Boolean;
 type Observation is record Present:Boolean:=False;end record;
 function Uncertain return Observation;
 package EC_Commands is function Quiescent return Boolean;end EC_Commands;
 end Fields;end Root;
end ACPI_One_Service;`);
put('acpi_one_service.adb',`with Probe_Model;package body ACPI_One_Service is
 use type Probe_Model.Test_Mode;
 package body Root is package body Fields is
 function Control_Idle return Boolean is(Probe_Model.Selected/=Probe_Model.Unsafe);
 function Uncertain return Observation is((Present=>False));
 package body EC_Commands is function Quiescent return Boolean is(True);end EC_Commands;
 end Fields;end Root;end ACPI_One_Service;`);
put('acpi_native_delays.ads',`with Interfaces;package ACPI_Native_Delays is
 procedure Inspect_Window(Deadline:out Interfaces.Unsigned_64;Available:out Boolean);
end ACPI_Native_Delays;`);
put('acpi_native_delays.adb',`with Probe_Model;package body ACPI_Native_Delays is
 use type Probe_Model.Test_Mode;
 procedure Inspect_Window(Deadline:out Interfaces.Unsigned_64;Available:out Boolean) is
 begin Deadline:=1000;Available:=Probe_Model.Selected/=Probe_Model.No_Clock;
 if Probe_Model.Selected=Probe_Model.No_Deadline then Deadline:=0;
 elsif Probe_Model.Selected=Probe_Model.Nonfinite then Deadline:=Interfaces.Unsigned_64'Last;end if;end;
end ACPI_Native_Delays;`);
put('acpi_native_log.ads',`package ACPI_Native_Log is procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False);end ACPI_Native_Log;`);
put('acpi_native_log.adb',`with Probe_Model;package body ACPI_Native_Log is
 use type Probe_Model.Test_Mode;
 procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is pragma Unreferenced(Success);
 begin if Peer_Quiet then raise Program_Error with "probe may not open logging";end if;
 Probe_Model.Logs:=Probe_Model.Logs+1;
 if Probe_Model.Selected=Probe_Model.Shed then Probe_Model.Dropped:=Probe_Model.Dropped+1;return;end if;
 Probe_Model.Last_Text(Probe_Model.Last+1..Probe_Model.Last+Text'Length):=Text;Probe_Model.Last:=Probe_Model.Last+Text'Length;end;
end ACPI_Native_Log;`);
put('acpi_native_pci_windows.ads',`with Interfaces;with ACPI_PCI_Protocol;with ACPI_Local_PCI_Results;package ACPI_Native_PCI_Windows is
 subtype Word is Interfaces.Unsigned_64;subtype Local_Window is ACPI_Local_PCI_Results.Window_Description;
 procedure Prepare_Local(Scope:Word;Key:ACPI_PCI_Protocol.P.Bus_Key;Setup_Deadline_Milliseconds:ACPI_PCI_Protocol.Wire.Time_Instant;Allow_Setup:Boolean;Result:out Local_Window);
 procedure Read_Local(Scope,Epoch,ID:Word;Region:ACPI_PCI_Protocol.P.Region;Offset:Word;Width:ACPI_PCI_Protocol.P.T.Access_Width;Deadline_Microseconds:Word;Guard:not null access function return Boolean;Result:out ACPI_Local_PCI_Results.Result);
end ACPI_Native_PCI_Windows;`);
put('acpi_native_pci_windows.adb',`with Probe_Model;package body ACPI_Native_PCI_Windows is
 package Q renames ACPI_PCI_Protocol;package P renames Q.P;package R renames ACPI_Local_PCI_Results;
 use type Word,Q.Wire.Time_Instant,P.Segment_Number,P.Bus_Number,P.Device_Number,P.Function_Number,P.T.Access_Width,Probe_Model.Test_Mode;
 procedure Prepare_Local(Scope:Word;Key:P.Bus_Key;Setup_Deadline_Milliseconds:Q.Wire.Time_Instant;Allow_Setup:Boolean;Result:out Local_Window) is
 begin if Scope/=Probe_Model.Scope or Key.Segment/=0 or Key.Bus/=0 or Setup_Deadline_Milliseconds/=Q.Wire.No_Finite_Deadline or Allow_Setup then raise Program_Error with "unexpected setup or key";end if;
 Probe_Model.Headers:=Probe_Model.Headers+1;Result:=(others=><>);
 if Probe_Model.Selected=Probe_Model.No_Window then Result.Status:=Q.Not_Ready;else Result.Status:=Q.Ready;Result.Epoch:=Probe_Model.Epoch;Result.ID:=Probe_Model.ID;end if;end;
 procedure Read_Local(Scope,Epoch,ID:Word;Region:P.Region;Offset:Word;Width:P.T.Access_Width;Deadline_Microseconds:Word;Guard:not null access function return Boolean;Result:out R.Result) is
 begin if Scope/=Probe_Model.Scope or Epoch/=Probe_Model.Epoch or ID/=Probe_Model.ID or Region.Device.Segment/=0 or Region.Device.Bus/=0 or Region.Device.Device/=31 or Region.Device.Func/=0 or Region.Base/=0 or Region.Length/=256 or Width/=P.T.Dword_Access or Deadline_Microseconds/=1000 or not Guard.all then raise Program_Error with "unexpected read or narrowed identity/deadline";end if;
 Probe_Model.Reads:=Probe_Model.Reads+1;Result:=(Status=>R.Read_Complete,Value=>0);
 if Probe_Model.Selected=Probe_Model.Expired then Result.Status:=R.Deadline_Expired;return;end if;
 case Offset is
 when 0=>Result.Value:=(if Probe_Model.Selected=Probe_Model.No_Intel then 16#5481_1022# elsif Probe_Model.Selected=Probe_Model.Wide_Identity then Word'Last else 16#5481_8086#);
 when 8=>if Probe_Model.Selected=Probe_Model.Class_Refused then Result.Status:=R.Guard_Refused;else Result.Value:=(if Probe_Model.Selected=Probe_Model.No_Class then 16#0300_0000# else 16#0601_0000#);end if;
 when 16#98#=>if Probe_Model.Selected=Probe_Model.LGMR_Refused then Result.Status:=R.Deadline_Expired;else Result.Value:=(if Probe_Model.Selected=Probe_Model.Disabled then 16#FE0B_0000# else 16#FE0B_0001#);end if;
 when others=>raise Program_Error with "unrequested register";end case;end;
end ACPI_Native_PCI_Windows;`);
put('probe_tests.adb',`with Ada.Command_Line;with Ada.Text_IO;with Ada.Strings.Fixed;with Probe_Model;with ACPI_Native_LPC_Probe;
procedure Probe_Tests is
 use type Probe_Model.Test_Mode;
 Mode:constant Probe_Model.Test_Mode:=Probe_Model.Test_Mode'Value(Ada.Command_Line.Argument(1));Checks:Natural:=0;
 procedure Check(OK:Boolean;Why:String) is begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end;
 Expected_Headers:constant Natural:=(if Mode in Probe_Model.Unsafe|Probe_Model.No_Clock|Probe_Model.No_Deadline|Probe_Model.Nonfinite then 0 else 1);
 Expected_Reads:constant Natural:=(case Mode is when Probe_Model.Good|Probe_Model.Disabled|Probe_Model.Shed|Probe_Model.LGMR_Refused=>3,when Probe_Model.No_Class|Probe_Model.Class_Refused=>2,when Probe_Model.No_Intel|Probe_Model.Wide_Identity|Probe_Model.Expired=>1,when others=>0);
begin Probe_Model.Selected:=Mode;ACPI_Native_LPC_Probe.Capture;ACPI_Native_LPC_Probe.Report;
 Check(Probe_Model.Headers=Expected_Headers and Probe_Model.Reads=Expected_Reads,"bounded existing-only read sequence");
 Check(Probe_Model.Logs=2,"two established-ring attempts only");
 if Mode=Probe_Model.Shed then Check(Probe_Model.Dropped=2 and Probe_Model.Last=0,"shed does not block capture");
 else declare Text:constant String:=Probe_Model.Last_Text(1..Probe_Model.Last);begin
 Check(Ada.Strings.Fixed.Index(Text,"source=EXISTING_READ_ONLY_ECAM setup=0")>0 and Ada.Strings.Fixed.Index(Text,"admission=NONE")>0,"no acquisition/admission implied");
 if Mode in Probe_Model.Good|Probe_Model.Disabled then
 Check(Ada.Strings.Fixed.Index(Text,"status=CAPTURED")>0 and Ada.Strings.Fixed.Index(Text,"decoded-base=0x00000000FE0B0000")>0,"exact captured decode");
 Check(Ada.Strings.Fixed.Index(Text,(if Mode=Probe_Model.Disabled then "enabled=0" else "enabled=1"))>0,"enable bit observed, never programmed");
 elsif Mode in Probe_Model.Unsafe|Probe_Model.No_Clock|Probe_Model.No_Deadline|Probe_Model.Nonfinite then Check(Ada.Strings.Fixed.Index(Text,"status=BOUNDARY_UNAVAILABLE")>0,"unsafe boundary refuses before header");
 elsif Mode=Probe_Model.No_Window then Check(Ada.Strings.Fixed.Index(Text,"status=WINDOW_UNAVAILABLE")>0,"missing window never creates one");
 elsif Mode in Probe_Model.No_Intel|Probe_Model.No_Class then Check(Ada.Strings.Fixed.Index(Text,"status=DIFFERENT_DEVICE")>0,"wrong device never reads LGMR");
 elsif Mode=Probe_Model.LGMR_Refused then Check(Ada.Strings.Fixed.Index(Text,"status=REGISTER_UNAVAILABLE")>0,"register failure explicit");
 else Check(Ada.Strings.Fixed.Index(Text,"status=IDENTITY_UNAVAILABLE")>0,"invalid identity/read explicit");end if;end;end if;
 Probe_Model.Selected:=Probe_Model.Good;ACPI_Native_LPC_Probe.Capture;ACPI_Native_LPC_Probe.Report;
 Check(Probe_Model.Headers=Expected_Headers and Probe_Model.Reads=Expected_Reads and Probe_Model.Logs=2,"no retry or repeated publication after any outcome");
 Ada.Text_IO.Put_Line("LPC probe checks"&Checks'Image);end Probe_Tests;`);
console.log(d);
