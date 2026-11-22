pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
with ACPI_Firmware_Data_Replies;
with ACPI_Target_Storage;
generic
   -- Called once only after an immutable operation entry has been retained.
   -- The controller must resolve Descriptor under Caller and recheck its live
   -- scope, deadline, physical lease, record geometry and capability facts.
   with procedure Execute
     (Caller : ACPI_Firmware_Data_Frames.Word;
      Descriptor : ACPI_Firmware_Data_Frames.Identifier; For_Write : Boolean;
      Input : ACPI_Firmware_Data_Frames.Word;
      Output : out ACPI_Firmware_Data_Frames.Word;
      Status : out ACPI_Firmware_Data_Frames.IPC.Outcome);
   Initial_Operations : Positive := 1;
-- One process-lifetime journal per backend endpoint. All actual metadata is
-- plain, copied and pointer-free. No release/reset or operation-ID recycling.
package ACPI_Firmware_Data_History with SPARK_Mode is
   package F renames ACPI_Firmware_Data_Frames;
   package IPC renames F.IPC;
   package Replies renames ACPI_Firmware_Data_Replies;
   subtype Word is F.Word;
   subtype Identifier is F.Identifier;
   type Outcome is (No_Evidence, Running, Finished, Identity_Conflict,
      Allocation_Refused, Invalid_Identity, Reentered);
   type Report is record
      State : Outcome := No_Evidence;
      Status : IPC.Outcome := IPC.Indeterminate;
      Value : Word := 0;
      Effect : Replies.Effect_Kind := Replies.No_Effect;
   end record;
   type Context is limited private;
   type Statistics is record
      Operations, Capacity : Natural := 0;
      Working : Boolean := False;
   end record;
   function Observe (Owner : Context) return Statistics;
   type Capacity_Problem is (No_Capacity_Failure, Process_Heap_Limit,
      Entry_Limit, Byte_Limit);
   type Capacity_Report is record
      Problem : Capacity_Problem := No_Capacity_Failure;
      Demand, Capacity, Requested_Bytes : Word := 0;
   end record;
   function Observe_Capacity (Owner : Context) return Capacity_Report;
   -- Full caller identity is authenticated receive metadata. Payload IDs
   -- never authorize an access. Matching duplicates return retained history;
   -- changing kind/value under the same key cannot dispatch a second effect.
   procedure Submit (Owner : in out Context; Caller : Word;
      Descriptor, Operation_ID : Identifier; For_Write : Boolean; Input : Word;
      Result : out Report);
   -- Passive, including after close/expiry or during Execute reentry. Does
   -- not allocate, resolve the descriptor, resume AML or dispatch any effect.
   function Query (Owner : Context; Caller : Word;
      Descriptor, Operation_ID : Identifier) return Report;
private
   type Entry_Record is record
      Caller : Word := 0;
      Descriptor, Operation_ID : Identifier := F.No_ID;
      For_Write : Boolean := False;
      Input : Word := 0;
      Saved : Report := (State => Running, others => <>);
   end record;
   package Store is new ACPI_Target_Storage (Entry_Record, Initial_Operations);
   type Context is limited record
      Items : Store.Context;
      Working : Boolean := False;
   end record;
end ACPI_Firmware_Data_History;
