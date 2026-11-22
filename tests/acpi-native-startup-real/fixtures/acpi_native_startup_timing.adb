pragma Ada_2022;
with Owner_Test_Control;
-- Owner-control fixture: diagnostics are modeled as passive so these tests do
-- not consume the scenario's deadline clock or bounded logger. The actual
-- reporter and transport are tested independently by acpi-work-timing/native-entry.
package body ACPI_Native_Startup_Timing is
 use type P.Kind;
 procedure Start is begin null; end Start;
 procedure Enter (Item : P.Kind; Handle : out Token) is
 begin
  if Item=P.AML_Stall then Owner_Test_Control.Stall_Entries:=Owner_Test_Control.Stall_Entries+1;
  elsif Item=P.AML_Sleep then Owner_Test_Control.Sleep_Entries:=Owner_Test_Control.Sleep_Entries+1;end if;
  Handle:=P.No_Token;end Enter;
 procedure Leave (Handle : in out Token) is
 begin Handle:=P.No_Token; end Leave;
 procedure Observe_Work (Item : AML_Work_Kinds.Kind; Entering : Boolean) is
  pragma Unreferenced(Item,Entering);
 begin null; end Observe_Work;
 function Snapshot (Item : P.Kind) return P.Counters is
  pragma Unreferenced (Item);
 begin return (others => <>); end Snapshot;
 procedure Report is begin Owner_Test_Control.Timing_Reports:=Owner_Test_Control.Timing_Reports+1; end Report;
end ACPI_Native_Startup_Timing;
