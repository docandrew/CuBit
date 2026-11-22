pragma Ada_2022;
with Interfaces; with CuBit.Messages;
with ACPI_Lock_Trace; with ACPI_Global_Lock_Protocol; with ACPI_Register_Roles;
package ACPI_Native_Lock_Notes is
   package G renames ACPI_Global_Lock_Protocol;
   package Trace is new ACPI_Lock_Trace
     (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Interfaces.Unsigned_64,
      Interfaces.Unsigned_64 (ACPI_Register_Roles.Frontend_Tag));
   procedure Begin_Request (Caller : CuBit.Messages.Process_ID; Scope : Interfaces.Unsigned_64; Kind : G.Operation);
   procedure Finish_Request (Reply : G.Reply);
   procedure Begin_Maintenance;
   procedure Finish_Maintenance (Outcome : G.Lock_Outcome);
   procedure Observe_Word (Word : Interfaces.Unsigned_32; Available : Boolean);
   procedure Observe_CAS (Expected, Replacement, Observed : Interfaces.Unsigned_32; Known, Attempted, Exchanged : Boolean);
   procedure Observe_Signal (Result : Trace.Signal_Result; Reads, Writes : Natural);
   procedure Handle_Dump
     (Caller : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Reply : in out CuBit.Messages.Message; Handled : out Boolean);
   -- Called only between completed operations, after the preceding IPC reply.
   -- The actual physical owner must establish no held/uncertain lease first.
   procedure Flush (No_Lease_Held : Boolean);
end ACPI_Native_Lock_Notes;
