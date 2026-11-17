pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.ACPI_Windows;
with CuBit.ACPI_Memory_Classes;
with CuBit.Handles;
with ACPI_FADT;
with ACPI_Software_Global_Lock;
with ACPI_Sleep_Entry;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Mapping_Startup;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Native_Catalog;
with ACPI_Backend_Startup_Native;
with ACPI_Backend_NVS_Native;
with ACPI_Global_Lock_Protocol;
generic
   Virtual_First, Virtual_Limit : Interfaces.Unsigned_64;
   Local_Scope : Interfaces.Unsigned_64;
   -- Required startup proof/check over the single service's actual layout:
   -- no overlap with owned RAM, received grants, snapshot/loan storage,
   -- stack/image/heap, or another device allocator. No permissive default.
   with function Arena_Is_Exclusive (First, Limit : Interfaces.Unsigned_64) return Boolean;
package ACPI_Local_Hardware is
   subtype Word is Interfaces.Unsigned_64;
   package B renames ACPI_Backend_Binding_Types;
   package Lock_Protocol renames ACPI_Global_Lock_Protocol;
   type Phase is (Fresh, Initializing, Ready, Failed, Stopping, Stopped);
   type Reason is (Not_Attempted, Initialized, Already_Attempted, Invalid_Arena,
      Invalid_Identity, Invalid_Clock, Empty_Grant, Invalid_Memory_Class,
      Mapping_Refused, Control_Timer_Refused, Binding_Refused, Mapping_Incomplete,
      NVS_Refused, Deadline_Stopped);
   type Report is record
      Stage : Phase := Fresh;
      Status : Reason := Not_Attempted;
      Binding : B.Report;
      Registers, NVS_Records : Natural := 0;
      Lock_Ready : Boolean := False;
   end record;
   function Observe return Report;
   function Admitting return Boolean;
   function Pending return Boolean;
   -- Final S5 local owner: no AML, IPC, caller address or new mapping. The
   -- endpoint supplies only a command from the validated local sequencer.
   -- Same retained catalog/handles as Fields and the Global Lock. An unknown
   -- write pins retirement; another attempt cannot replay it.
   function Sleep_Register_Model return ACPI_Sleep_Entry.Register_Model;
   procedure Sleep_Action (Command : ACPI_Sleep_Entry.Command;
      Deadline_Microseconds : Word; Output : out ACPI_Sleep_Entry.Control_Word;
      Result : out ACPI_Sleep_Entry.Completion);
   procedure Initialize (Input : aliased in CuBit.ACPI_Windows.Grant;
      FADT : ACPI_FADT.Description; Deadline : B.Tick; Result : out Report);
   -- FADT is parsed from this service's already-copied immutable table view
   -- (Core.Fixed_Description), before AML starts. No second table IPC/copy,
   -- peer registration, role35 driver, server receive loop or mutable alias.
   -- Latches before inspecting/mapping; all used handles/mappings and every
   -- FACS/NVS/lock context stay owned here after failure or stop.
   procedure Claim_Unused_Arena (First, Limit : out Word; Accepted : out Boolean);
   type Lease is limited private;
   procedure Acquire (Item : in out Lease; Deadline_Microseconds : Word;
      Budget : Lock_Protocol.Attempt_Budget; Result : out Lock_Protocol.Lock_Outcome);
   procedure Check_Access (Item : Lease; Result : out Lock_Protocol.Lock_Outcome);
   function Needs_Cleanup (Item : Lease) return Boolean;
   procedure Release (Item : in out Lease; Result : out Lock_Protocol.Lock_Outcome);
   -- Rearms only on confirmed Released evidence for this exact private
   -- ticket. Unknown/failed cleanup retains the original lease unchanged.
   -- Typed local lease only. Scope/self/domain are private; callers provide
   -- no physical address, process ID, wire ticket or completion evidence.
   procedure Begin_Stop;
   function May_Exit return Boolean;
   procedure Maintain (Wake_Deadline_Milliseconds : out Word; May_Continue : out Boolean);
   -- One serialized maintenance step before the sole service loop waits.
   -- The existing scheduler returns IPC-clock milliseconds itself; never cast
   -- a lease microsecond deadline. No lost/unknown effect is automatically retried.
private
   use type Word;
   Page_Bytes : constant Word := CuBit.ACPI_Memory_Classes.Page_Bytes;
   Arena_Parameters_Valid : constant Boolean := Virtual_First /= 0 and then
      Virtual_First < Virtual_Limit and then Virtual_Limit <= B.User_Virtual_Limit
      and then Virtual_First mod Page_Bytes = 0 and then Virtual_Limit mod Page_Bytes = 0
      and then (Virtual_Limit - Virtual_First) / Page_Bytes in 1 .. Word (Positive'Last);
   Mapping_Page_Limit : constant Positive :=
      (if Arena_Parameters_Valid then Positive ((Virtual_Limit - Virtual_First) / Page_Bytes) else 1);
   Maximum_Map_Pages_Per_Call : constant Positive := 1024; -- MAP_DEVICE contract
   package Mapping_Owner is new ACPI_Backend_Mapping_Startup
     (CuBit.Handles.Device_Memory,CuBit.Handles.No_Device_Memory,
      ACPI_Backend_Kernel_Handles.Map_Memory,ACPI_Backend_Kernel_Handles.Read_Clock,
      CuBit.ACPI_Windows.Maximum_Memory_Windows,Mapping_Page_Limit,Maximum_Map_Pages_Per_Call,
      CuBit.ACPI_Memory_Classes.Page_Limit * Page_Bytes,Virtual_Limit);
   Mapping : Mapping_Owner.Context;
   Last_Mapping : Mapping_Owner.Report;
   procedure Map_Window (Held : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : Word; Read_Only : Boolean; Mapped : out Boolean);
   package Native is new ACPI_Backend_Native_Catalog
     (CuBit.ACPI_Windows.Maximum_IO_Windows+CuBit.ACPI_Windows.Maximum_Memory_Windows,
      1,1,Virtual_First,Virtual_Limit,Mapping_Page_Limit,Map_Window);
   package Catalog renames Native.Owner;
   package Bindings is new ACPI_Backend_Startup_Native (Catalog);
   Hardware : Catalog.Context;
   function Is_NVS (Window : Catalog.W.Window_ID) return Boolean;
   package NVS_Native is new ACPI_Backend_NVS_Native
     (Catalog,Hardware,Is_NVS,CuBit.ACPI_Windows.Maximum_Memory_Windows);
   package NVS renames NVS_Native.Records;
   NVS_State : NVS.Context;
   NVS_Status : NVS.Outcome;

   Bound_Run : Bindings.Owner.Context;
   Grant : aliased CuBit.ACPI_Windows.Grant;
   Facts : ACPI_FADT.Description;
   Software_Lock : ACPI_Software_Global_Lock.Context;
   Self : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
   State : Phase := Fresh;
   Info : Report;
   Last_Startup_Time, Until_Time : B.Tick := 0;
   type Lease is limited record
      Prepared, Software_Only : Boolean := False;
      Software_Lease : ACPI_Software_Global_Lock.Lease;
      Domain, Sequence : Word := 0;
   end record;
end ACPI_Local_Hardware;
