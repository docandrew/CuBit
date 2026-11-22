pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Firmware_Data_Frames;
with ACPI_Target_Storage;
with ACPI_Capacity_Diagnostics;
generic
   type Session is limited private;
   type Declaration is private;
   -- Core has already matched a live original table/definition before this
   -- callback. The actual must also match literal completed SystemMemory
   -- attributes to Geometry. No namespace/arena alias is passed or retained.
   with function Original_Declaration (Item : Declaration;
      Geometry : AML_Field_Layout.Register_Geometry) return Boolean;
   with function Session_Pending (IO : Session) return Boolean;
   with procedure Publish (IO : in out Session; Region : ACPI_Firmware_Data_Frames.L.Region;
      Deadline : ACPI_Firmware_Data_Frames.IPC.Time_Instant;
      Record_ID : out ACPI_Firmware_Data_Frames.Identifier; Status : out AML_Field_Access.Completion);
   with procedure Begin_Data_Field (IO : in out Session; Requires_Lock : Boolean;
      Deadline : ACPI_Firmware_Data_Frames.IPC.Time_Instant;
      Domain, Sequence : ACPI_Firmware_Data_Frames.Word; Accepted : out Boolean);
   with procedure Access_Datum (IO : in out Session; Record_ID : ACPI_Firmware_Data_Frames.Identifier;
      Offset : ACPI_Firmware_Data_Frames.Word; Width : ACPI_Firmware_Data_Frames.T.Access_Width;
      For_Write : Boolean; Input : ACPI_Firmware_Data_Frames.Word;
      Value : out ACPI_Firmware_Data_Frames.Word;
      Status : out AML_Field_Access.Completion);
   with procedure End_Data_Field (IO : in out Session;
      Deadline : ACPI_Firmware_Data_Frames.IPC.Time_Instant; Accepted : out Boolean);
   with procedure Stop_And_Close (IO : in out Session;
      Deadline : ACPI_Firmware_Data_Frames.IPC.Time_Instant; Accepted : out Boolean);
   with procedure Publish_Deadline (Deadline : out ACPI_Firmware_Data_Frames.IPC.Time_Instant);
   -- Called AFTER the existing physical Field guard accepts Begin. Returns
   -- that Field's captured deadline and borrowed ticket. Never acquires a
   -- second physical lock. Composite owner unwinds it if data Begin refuses.
   with procedure Read_Field_Context (Requires_Lock, For_Write : Boolean;
      Deadline : out ACPI_Firmware_Data_Frames.IPC.Time_Instant;
      Domain, Sequence : out ACPI_Firmware_Data_Frames.Word; Accepted : out Boolean);
   with procedure Cleanup_Deadline (Deadline : out ACPI_Firmware_Data_Frames.IPC.Time_Instant);
   Allow_Writes : Boolean;
   Initial_Targets : Positive := 1;
   -- Passive typed diagnostic; no alias to the private session escapes.
   with procedure Inspect_Session_Capacity (IO : Session;
      Capacity : in out ACPI_Capacity_Diagnostics.Report) is null;
package ACPI_Firmware_Data_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package F renames ACPI_Firmware_Data_Frames;
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Ready (IO : Context) return Boolean;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   procedure Resolve_Startup_Firmware_Target (IO : in out Context; Item : Declaration;
      Geometry : L.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean);
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Begin_Field (IO : in out Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure End_Field (IO : in out Context; Accepted : in out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   -- Cleanup/history never resumes or manufactures reconciliation of AML.
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Cleanup (IO : in out Context; Accepted : out Boolean);
   type Capacity_Problem is (No_Capacity_Failure, Process_Heap_Limit, Entry_Limit, Byte_Limit);
   type Capacity_Report is record
      Problem : Capacity_Problem := No_Capacity_Failure;
      Demand, Capacity, Requested_Bytes : F.Word := 0;
   end record;
   function Observe_Capacity (IO : Context) return Capacity_Report;
   function Session_Capacity_Report (IO : Context) return ACPI_Capacity_Diagnostics.Report;
private
   type Association is record
      Geometry : L.Register_Geometry;
      Record_ID : F.Identifier := F.No_ID;
      Ready : Boolean := False;
   end record;
   package Store is new ACPI_Target_Storage (Association,Initial_Targets);
   type Context is limited record
      Inner : Session;
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Epoch : F.Identifier := F.No_ID;
      Rows : Store.Context;
      Consumed, Initialized, Working, Failed : Boolean := False;
      Depth : Natural := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Firmware_Data_Provider;
