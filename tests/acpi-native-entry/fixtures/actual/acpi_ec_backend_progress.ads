pragma Ada_2022;
with ACPI_EC_Registers;
with ACPI_EC_Native_Calls;
with ACPI_EC_Event_Publisher;
with ACPI_EC_Types;
--  Trusted backend only. One serialized dispatcher for one catalog,its saved
--  Field reply owner and event publisher. No receive loop,hardware admission,
--  blocking peer call or native launch is provided by this package.
generic
   with package Registers is new ACPI_EC_Registers (<>);
   with package Calls is new ACPI_EC_Native_Calls (Registers => Registers, others => <>);
   with package Events is new ACPI_EC_Event_Publisher (Registers => Registers, others => <>);
package ACPI_EC_Backend_Progress is
   type Action_Kind is (No_Work, Advance_Field, Advance_Query, Publish_Notice,
                       Wait_Until, Owner_Fault, Inconsistent_Owners);
   type Report is record
      Action : Action_Kind := No_Work;
      At_Time : ACPI_EC_Types.Microseconds := 0;
      Field_Reply_Pending, Notice_Retained, Hardware_Recovery : Boolean := False;
      Event_Stage : Events.Phase := Events.Unconfigured;
      Field : Calls.Report;
   end record;
   --  Read-only. Publication can be runnable even when catalog has No_Work,
   --  or while Field progress waits. Retained/stalled/unknown delivery does not
   --  imply runnable publication,and is never automatically resumed/replayed.
   function Next (Now : ACPI_EC_Types.Microseconds) return Report;
   --  At most one owning adapter Advance or one nonblocking publication attempt.
   --  Alternates runnable Field/publication work when both are ready. No internal
   --  wait,polling loop,AML evaluation or direct Engine.Step. Fields remain able
   --  to progress beside unknown notice retention. Fault/conflict retains state.
   procedure Advance (Now : ACPI_EC_Types.Microseconds; Result : out Report);
   --  After one bounded pause or supervised wait failure,check the owning
   --  engine once even if its advisory wake remains future. This spends its
   --  original work budget and checks its fresh clock; minimum-delay/IO guards
   --  remain in the engine. It never claims time reached or bypasses ownership.
   procedure Advance_Waiter (Now : ACPI_EC_Types.Microseconds; Result : out Report);
   --  No_Work is not global quiescence. All returned obligations remain owned;
   --  the outer controller handles new authenticated calls/events/receipts,
   --  bounded OS waits,clock conversion and reservation/GlobalLock lifetime.
end ACPI_EC_Backend_Progress;
