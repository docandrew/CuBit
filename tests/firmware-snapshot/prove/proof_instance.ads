pragma Ada_2022;
with Interfaces; use Interfaces;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Snapshots.Transfers;
with Firmware_Snapshot_ABI;
with Firmware_Snapshot_Service;

--  A SPARK instance of the kernel's service body for gnatprove: the same
--  shape as the kernel's (a snapshot state and the caller's memory behind
--  subprograms), so the body's absence of run-time errors and its
--  postconditions are proved for an instance. The kernel's own actuals
--  (acpi's Boot_Snapshot, Process.User_Memory) are SPARK_Mode Off and are
--  assumed to behave like these: total, writing only what they say.
package Proof_Instance with SPARK_Mode is
   package S renames Firmware_Tables.Snapshots;

   Snapshot : S.State;

   function Ready return Boolean is (S."=" (S.Current (Snapshot), S.Ready));
   function Count return Natural is (S.Count (Snapshot));
   function Describe (Index : Natural) return S.Metadata_Result is
     (S.Describe (Snapshot, Index));
   procedure Copy_Part
     (Index, Offset : Natural; Destination : out Firmware_Tables.Bytes;
      Result : out S.Transfers.Transfer_Result)
   with Global => Snapshot;

   Written : Unsigned_64 := 0;
   procedure Write_User
     (Destination : Unsigned_64; Source : Firmware_Tables.Bytes; Success : out Boolean)
   with Global => (In_Out => Written);
   procedure Write_Reply (Item : Firmware_Snapshot_ABI.Reply; Success : out Boolean)
   with Global => (In_Out => Written);

   package Service is new Firmware_Snapshot_Service
     (Snapshot_Ready => Ready, Table_Count => Count, Describe => Describe,
      Copy_Part => Copy_Part, Write_User => Write_User, Write_Reply => Write_Reply);
end Proof_Instance;
