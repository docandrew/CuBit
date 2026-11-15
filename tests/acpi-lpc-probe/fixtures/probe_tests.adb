with Ada.Command_Line;with Ada.Text_IO;with Ada.Strings.Fixed;with Probe_Model;with ACPI_Native_LPC_Probe;
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
 Ada.Text_IO.Put_Line("LPC probe checks"&Checks'Image);end Probe_Tests;