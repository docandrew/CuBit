pragma Ada_2022;
-- Owner-control fixture: diagnostics are modeled as passive so these tests do
-- not consume the scenario's deadline clock or bounded logger. The actual
-- reporter and transport are tested independently by acpi-work-timing/native-entry.
package body ACPI_Native_Startup_Timing is
 procedure Start is begin null; end Start;
 procedure Enter (Item : P.Kind; Handle : out Token) is
  pragma Unreferenced (Item);
 begin Handle:=P.No_Token; end Enter;
 procedure Leave (Handle : in out Token) is
 begin Handle:=P.No_Token; end Leave;
 procedure Observe_Work (Item : AML_Work_Kinds.Kind; Entering : Boolean) is
  pragma Unreferenced(Item,Entering);
 begin null; end Observe_Work;
 procedure Report is begin null; end Report;
end ACPI_Native_Startup_Timing;
