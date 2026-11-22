pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Process_IDs;
with ACPI_Local_Hardware;
with ACPI_Platform_Pool_Native;
with ACPI_Platform_Regions_Native;
with ACPI_Backend_Device_Arenas;
generic
   with package Hardware is new ACPI_Local_Hardware (<>);
   Local_Scope : Interfaces.Unsigned_64;
   with function Setup_Allowed return Boolean;
   with function Field_Access_Allowed (For_Write : Boolean) return Boolean;
   with function Arena_Is_Exclusive (First, Limit : Interfaces.Unsigned_64) return Boolean;
   Maximum_Regions, Maximum_Mappings : Positive;
-- Single-service root composition, after Hardware.Initialize and before AML.
-- Setup_Allowed excludes every held/pending/uncertain physical Field frame.
-- Arena_Is_Exclusive uses the final process layout contract and its actual
-- device allocator ownership, not an address supplied by an IPC requester.
package ACPI_Local_Memory is
   subtype Word is Interfaces.Unsigned_64;
   type Phase is (Fresh, Initializing, Ready, Failed, Closed);
   type Reason is (Not_Attempted, Initialized, Already_Attempted,
      Owner_Unavailable, Arena_Refused, Invalid_Partition,
      Pool_Refused, Cache_Refused, PCI_Refused, Setup_Stopped);
   type Pool_Configuration is record
      Present : Boolean := False;
      Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Expected_Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Request, Deadline_Milliseconds : Word := 0;
   end record;
   type PCI_Configuration is record
      Present : Boolean := False;
      Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Expected_Owner : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      Epoch : Word := 0;
   end record;
   type Report is record
      Stage : Phase := Fresh;
      Status : Reason := Not_Attempted;
      Arena_Claimed, Pool_Acquired, Cache_Ready, PCI_Ready : Boolean := False;
      Partition : ACPI_Backend_Device_Arenas.Selection;
      Pool : ACPI_Platform_Pool_Native.Owner.Status;
   end record;
   function Observe return Report;
   function Cache_Arena_Is_Exclusive (First, Limit : Word) return Boolean;
   function Cache_Access_Allowed (For_Write : Boolean) return Boolean;
   package Regions is new ACPI_Platform_Regions_Native
     (Maximum_Regions,Maximum_Mappings,Cache_Arena_Is_Exclusive,Cache_Access_Allowed);
   Memory : Regions.Cache.Context;
   -- Bind Regions.Cache and this exact Memory to Hardware.Pooled_Fields.
   -- Local_Scope must be the same authenticated root constant as Hardware.
   procedure Initialize (Pool : Pool_Configuration; PCI : PCI_Configuration;
      Accepted : out Boolean);
   -- One attempt, including refusal/unknown completion. A failed attempt
   -- retains its claimed arena, pool carriage, mappings and PCI owner state.
   -- No logger, second FACS owner, listener, unmap, reinitialization or retry.
   procedure Close_Admission;
   -- Cache/pool only: root also closes its Field provider and Hardware.
   -- Retained PCI backing remains governed by that same Field access guard.
   procedure Retry_Disposal (Completed : out Boolean);
   -- Explicit disposal of an UNACCEPTED pool carriage only, while setup is
   -- permitted; never reacquires a pool or drops admitted mapping authority.
private
   Info : Report;
   Pool_State : ACPI_Platform_Pool_Native.Owner.Context;
end ACPI_Local_Memory;
