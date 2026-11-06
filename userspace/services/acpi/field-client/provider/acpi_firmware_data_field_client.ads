pragma Ada_2022;
with ACPI_Firmware_Data_Transport;
with ACPI_Target_Storage;
generic
   type Channel is limited private;
   with package RPC is new ACPI_Firmware_Data_Transport (Channel => Channel,others => <>);
   Connection : in out Channel;
   Initial_Regions, Initial_Frames, Initial_Descriptors : Positive := 1;
-- Frontend lifecycle only. No namespace alias, hardware capability, address
-- mapping or physical lock owner. The enclosing AML provider must validate
-- original sealed-table literal declaration provenance BEFORE Publish, and
-- acquire its existing whole-Field physical lease BEFORE Begin_Field.
package ACPI_Firmware_Data_Field_Client is
   package P renames RPC.P;
   package F renames P.F;
   package IPC renames P.IPC;
   subtype Word is P.Word;
   subtype Identifier is F.Identifier;
   type Outcome is (Ready, Completed, Denied, Invalid_Request, Not_Ready,
      Allocation_Refused, Exhausted, Pending, Failed, Closed, Reentered);
   type Result is record
      Status : Outcome := Not_Ready;
      ID : Identifier := F.No_ID;
      Value : Word := 0;
   end record;
   type Context is limited private;
   type Statistics is record
      Regions, Frames, Descriptors, Depth : Natural;
      Working, Failed, Cleanup_Due : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   type Cache_Kind is (No_Cache, Region_Cache, Frame_Cache, Descriptor_Cache);
   type Capacity_Problem is (No_Capacity_Failure, Process_Heap_Limit, Entry_Limit, Byte_Limit);
   type Capacity_Report is record
      Cache : Cache_Kind := No_Cache;
      Problem : Capacity_Problem := No_Capacity_Failure;
      Demand, Capacity, Requested_Bytes : Word := 0;
   end record;
   function Observe_Capacity (Owner : Context) return Capacity_Report;
   -- Passive evidence from the most recent actual publication RPC. It is
   -- retained through cleanup; a local precondition/cache hit does not invent
   -- an exchange. Transport failure and validated backend refusal are distinct.
   type Publication_Diagnostic is record
      Present : Boolean := False;
      Transport : RPC.Outcome := RPC.Invalid_Request;
      Has_Remote : Boolean := False;
      Remote : P.Outcome := P.Invalid_Request;
      Deadline : IPC.Time_Instant := 0;
   end record;
   function Observe_Publication (Owner : Context) return Publication_Diagnostic;
   procedure Publish (Owner : in out Context; Region : F.L.Region;
      Deadline : IPC.Time_Instant; Reply : out Result);
   -- The immutable deadline is supplied by the enclosing Field owner. A
   -- required lock needs a nonzero ticket exported by its existing lease;
   -- a NoLock child may still borrow an enclosing held lease. No acquisition.
   procedure Begin_Field (Owner : in out Context; Requires_Lock : Boolean;
      Deadline : IPC.Time_Instant; Domain, Sequence : Word; Reply : out Result);
   -- Record came from this context's successful Publish. Offset/width are
   -- the exact selected AML datum. Backend selection revalidates them too.
   procedure Access_Datum (Owner : in out Context; Record_ID : Identifier;
      Offset : Word; Width : F.T.Access_Width; For_Write : Boolean; Input : Word;
      Reply : out Result);
   -- Every accepted Begin must unwind once, even after failure. Close never
   -- releases the physical lock. An independent finite cleanup deadline is
   -- required; original Field deadline is never refreshed for data access.
   procedure End_Field (Owner : in out Context; Deadline : IPC.Time_Instant; Reply : out Result);
   -- Stops new admission first. At depth0, close retained/uncertain scopes
   -- using passive Describe_Scope when the server ID was not received. This
   -- does not replay publication, selection or data and cannot rearm AML.
   procedure Cleanup (Owner : in out Context; Deadline : IPC.Time_Instant; Reply : out Result);
private
   type Region_Phase is (Publishing, Published, Region_Stopped);
   type Region_Record is record
      Geometry : F.L.Region;
      Client, Server : Identifier := F.No_ID;
      Stage : Region_Phase := Publishing;
   end record;
   type Frame_Phase is (Opening, Live, Closing, Retired);
   type Frame_Record is record
      Client, Server : Identifier := F.No_ID;
      Parent : Natural := 0;
      Deadline : IPC.Time_Instant := 0;
      Domain, Sequence : Word := 0;
      Stage : Frame_Phase := Opening;
   end record;
   type Descriptor_Record is record
      Frame : Natural := 0;
      Client, Server, Record_ID : Identifier := F.No_ID;
      Offset : Word := 0;
      Width : F.T.Access_Width := F.T.Byte_Access;
   end record;
   package Regions is new ACPI_Target_Storage (Region_Record,Initial_Regions);
   package Frames is new ACPI_Target_Storage (Frame_Record,Initial_Frames);
   package Descriptors is new ACPI_Target_Storage (Descriptor_Record,Initial_Descriptors);
   type Context is limited record
      Transport : RPC.Context;
      Region_Rows : Regions.Context;
      Frame_Rows : Frames.Context;
      Descriptor_Rows : Descriptors.Context;
      Active, Depth : Natural := 0;
      Working, Failed : Boolean := False;
      Uncertain : Boolean := False;
      Request : F.Request;
      Expected : P.Effect_Kind := P.No_Effect;
      Width : F.T.Access_Width := F.T.Qword_Access;
      Capacity : Capacity_Report;
      Publication : Publication_Diagnostic;
   end record;
end ACPI_Firmware_Data_Field_Client;
