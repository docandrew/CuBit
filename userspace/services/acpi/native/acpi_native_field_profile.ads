pragma Ada_2022;
with Interfaces;
with AML_Field_Notes;
-- Temporary module-only measurements. TSC wall ticks include preemption and
-- probe overhead; nested spans overlap and must not be added together.
package ACPI_Native_Field_Profile with SPARK_Mode => Off is
   subtype Tick is Interfaces.Unsigned_64;
   No_Tick : constant Tick := 0;
   type Span_Kind is (Field_Entry, Field_Dispatch, Field_Exit, Field_Resolution, PCI_Inspect);
   procedure Start;
   procedure Stop;
   function Stamp return Tick;
   procedure Finish (Kind : Span_Kind; Since : Tick);
   procedure Note (Item : AML_Field_Notes.Event);
   -- Once, after Stop, through the established copied logger. No access
   -- authority, deadline, retry, AML result or admission decision is changed.
   procedure Report;
end ACPI_Native_Field_Profile;
