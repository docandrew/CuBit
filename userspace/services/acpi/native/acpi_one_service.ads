pragma Ada_2022;
with ACPI_Native_Delays;
with ACPI_Local_Hardware.Pooled_Fields.Runtime;
with ACPI_Requests;
with ACPI_Requests.With_Local_Runtime;
with Interfaces;
with CuBit.ACPI_Windows;
with CuBit.ACPI_Memory_Classes;
with Process_Layout;
with ACPI_Service;
with ACPI_Local_Root;
with ACPI_Global_Lock_Protocol;
with ACPI_Native_PCI_Aliases;
with ACPI_Register_Roles;
with AML_Field_Access;

-- ACPI-ONE-001: the ONE acpi.svc. It holds the ACPI hardware grant itself
-- (FADT fixed-hardware ports, FACS/NVS firmware memory, the PCI
-- configuration broker and the platform-register pool) and runs the AML
-- interpreter over that authority in-process. There is no acpi-backend.svc,
-- no role-35 register endpoint and no per-access IPC: the backend's proved
-- pieces are libraries composed here through the ACPI agent's local root
-- (userspace/services/acpi/backend/native/acpi_local_root.ads).
--
-- Startup flow (ACPI_Native_Instance drives it, in this order):
--   1. Configure_Hardware: capture the launch grant, the pool and PCI
--      endpoints and the startup deadlines. Nothing is mapped yet.
--   2. The snapshot loader copies and admits the tables, then calls the
--      module callback (Root.Evaluation.Execute_Module_Startup), which
--      parses the FADT from that SAME copied catalog and initializes, in
--      order: Hardware (grant, FACS, Global Lock) -> the local memory bridge
--      (pool cache, PCI windows) -> Fields.Begin_Startup -> module AML.
--      Tables are therefore loaded BEFORE any Field provider is selected.
--   3. Device _INI/_STA (ACPI_Device_Initialization) runs through the same
--      Fields; Finish_Startup closes setup admission only AFTER it.
--   4. Serving: Serving_Maintenance (Root.Maintain_Owner plus every other
--      root-owned deadline, one kernel wait-clock domain) bounds the idle wait.
--   5. Stop: Consumers_Closed, then Root.Shutdown drives stop, Field cleanup
--      and hardware retirement; the process exits only when May_Retire.
package ACPI_One_Service is
   subtype Word is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   package Memory_Classes renames CuBit.ACPI_Memory_Classes;

   ---------------------------------------------------------------------------
   -- The hardware arena: a program-owned subrange of the device-mapping arena
   -- (Process_Layout). The kernel places nothing of its own in that arena and
   -- acpi.svc maps device memory nowhere else, so this subrange belongs to
   -- the root alone: Hardware windows, the pool region cache and PCI ECAM
   -- windows are carved from it (Hardware.Claim_Unused_Arena). 64 GiB keeps
   -- the page count well inside Integer'Last (2**24 pages).
   ---------------------------------------------------------------------------
   Hardware_Arena_Bytes : constant := 2 ** 36;
   Hardware_Arena_First : constant := Process_Layout.Device_Mapping_First;
   Hardware_Arena_Limit : constant := Hardware_Arena_First + Hardware_Arena_Bytes;
   Hardware_Arena_Pages : constant := Hardware_Arena_Bytes / Memory_Classes.Page_Bytes;
   pragma Compile_Time_Error
     (Hardware_Arena_Limit > Process_Layout.Device_Mapping_Limit,
      "the ACPI hardware arena must lie inside the device-mapping arena");
   pragma Compile_Time_Error
     (Hardware_Arena_First mod Memory_Classes.Page_Bytes /= 0
        or else Hardware_Arena_Bytes mod Memory_Classes.Page_Bytes /= 0,
      "the ACPI hardware arena must be page aligned");
   pragma Compile_Time_Error
     (Hardware_Arena_Pages > Integer'Last,
      "Hardware's Positive mapping page count must hold the arena");

   -- [First, Limit) is a non-empty part of the hardware arena. The root asks
   -- this before it maps into any range Hardware hands it.
   function Arena_Is_Exclusive (First, Limit : Word) return Boolean is
     (First < Limit and then First >= Hardware_Arena_First
      and then Limit <= Hardware_Arena_Limit);

   -- The one in-process issuer scope shared by Hardware's Global Lock
   -- sessions, named registers, NVS, PCI and pool Field targets. It must be
   -- the scope the proved startup binding authorizes in Hardware's catalog
   -- (ACPI_Backend_Startup: Catalog.Authorize_Scope with
   -- ACPI_Register_Roles.Frontend_Tag); any other value makes NVS and
   -- named-register setup refuse (seen as NVS_REFUSED). It is a local
   -- constant now: no wire peer or kernel badge carries it any more.
   Field_Scope : constant Word := Word (ACPI_Register_Roles.Frontend_Tag);

   -- Named ceilings for the root's retained tables (no hidden defaults).
   Platform_Region_Ceiling  : constant Positive := 128;  -- pool-backed SystemMemory regions
   Platform_Mapping_Ceiling : constant Positive := 256;  -- pool mappings backing them
   Field_Nesting_Ceiling    : constant Positive := 8;    -- nested Field transactions (Index/Bank)

   -- The Global Lock lease of one Field transaction (same as the former
   -- frontend's), in Hardware's lock clock: CuBit.Monotonic microseconds.
   Field_Lease_Microseconds : constant := 100_000;

   ---------------------------------------------------------------------------
   -- Root actuals. All fail closed: no permissive default exists.
   ---------------------------------------------------------------------------
   -- The whole rounded page envelope of a pool-backed region must not overlap
   -- any firmware memory window of the hardware grant (FACS/NVS are mapped
   -- write-back by Hardware). PCI ECAM is never a pool member: the kernel
   -- grants it only under Firmware_PCI_Config_Tag, and pool membership is
   -- the kernel's independent map-time check.
   function Compatible_Envelope (First_Page, Pages : Word) return Boolean;
   -- One Field transaction's lease deadline and attempt budget.
   procedure Begin_Window (Deadline_Microseconds : out Word;
      Budget : out ACPI_Global_Lock_Protocol.Attempt_Budget; Available : out Boolean);
   -- PCI window setup (an IPC to devmgr's broker) shares the startup
   -- deadline in GETTIME milliseconds; none once the startup window closes.
   procedure PCI_Setup_Deadline (Deadline_Milliseconds : in out Word;
      Available : in out Boolean);
   -- True once nothing can enter the evaluation owner any more: the serving
   -- loop has returned (or never started). Set by Mark_Consumers_Closed.
   procedure Close_Consumers (Accepted : out Boolean);
   -- Every root obligation outside Hardware/Fields/pool/PCI: the stopped
   -- runtime, final Core retained-result observation, and confirmed SCI
   -- quiescence (or Arm never attempted). Unknown/pending SCI cannot retire.
   -- EC events are not yet served. When Main adopts the EC
   -- event supervisor (ACPI_Native_Server.With_EC_Events), its retained
   -- delivery and completion state must be reported here.
   procedure Other_Owners_View (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Word);

   package Root is new ACPI_Local_Root
     (ACPI_Service, Hardware_Arena_First, Hardware_Arena_Limit, Field_Scope,
      Arena_Is_Exclusive, Compatible_Envelope, Begin_Window, PCI_Setup_Deadline,
      ACPI_Native_PCI_Aliases.Resolve,
      Platform_Region_Ceiling, Platform_Mapping_Ceiling, Field_Nesting_Ceiling,
      Close_Consumers, Other_Owners_View, ACPI_Native_Delays.Inspect_Window);
   -- The same delay domain supplies both event windows and Field inspection.
   -- One child facade claims the existing Fields/cache; no second Core exists.
   package Runtime is new Root.Fields.Runtime
     (ACPI_Native_Delays.Enter_Runtime,ACPI_Native_Delays.Begin_Event,
      ACPI_Native_Delays.End_Event,ACPI_Native_Delays.Close_Startup);
   package Runtime_Access is new ACPI_Requests.With_Local_Runtime
     (Runtime.Enter_Runtime,Runtime.Begin_Event,Runtime.End_Event);

   type Core_Retirement_State is (Unobserved,No_Core,Clear_Core,Retained_Results,Retained_Regions,Retained_Events);
   type SCI_Retirement_State is (Not_Armed,SCI_Unobserved,Clear_SCI,Retained_SCI);

   ---------------------------------------------------------------------------
   -- Glue state, driven by ACPI_Native_Instance.
   ---------------------------------------------------------------------------
   -- Startup symbol of the local Field provider (one provider per process).
   Field_Symbol : constant AML_Field_Access.Endpoint_ID := 1;

   -- Captures the launch grant for Compatible_Envelope and configures the
   -- root (Root.Evaluation.Configure) once. Deadlines: Hardware's startup
   -- deadline in lock-clock microseconds, the setup deadline in GETTIME
   -- milliseconds. Nothing is mapped until the module callback runs.
   procedure Configure_Hardware
     (Input : CuBit.ACPI_Windows.Grant;
      Hardware_Deadline_Microseconds : Word;
      Setup_Deadline_Milliseconds : Word;
      Pool : Root.Backing.Pool_Configuration;
      PCI : Root.Backing.PCI_Configuration;
      Accepted : out Boolean);
   -- Device _INI is done: close setup admission (Root.Evaluation.Finish_Startup)
   -- and the PCI setup window. Runtime evaluation continues.
   procedure Finish_Startup (Accepted : out Boolean);
   procedure Mark_Consumers_Closed;

   -- The serving loop's maintenance step: Root.Maintain_Owner combined with
   -- every other root-owned deadline, all absolute kernel wait-clock
   -- milliseconds (CuBit.Messages.Wait_Forever when none is scheduled).
   procedure Serving_Maintenance (Wake_Deadline_Milliseconds : out Word;
      May_Continue : out Boolean);

   -- Pre-serving terminal step. Closes this backend's admission and advances
   -- retirement once, without draining control/service queues or waiting.
   -- False retains cleanup obligations; it never permits runtime resumption.
   procedure Try_Retire (Core_View : Core_Retirement_State;
      SCI_View : SCI_Retirement_State; Retired : out Boolean);
   -- After serving: drive Root.Shutdown until May_Retire, pacing explicit
   -- disposal of a refused pool carriage. Returns True only when the root
   -- may be retired; False means retained state must outlive this call
   -- (the caller parks rather than exiting).
   procedure Retire (Core_View : Core_Retirement_State;
      SCI_View : SCI_Retirement_State; Retired : out Boolean);
   -- Retirement was refused or cannot progress: keep this process (and so
   -- every mapping, lease and handle the root retains) alive, answering
   -- typed terminal refusals. Never returns normally.
   Region_State_Released_On_Stop : exception;
   -- Park raises only this typed exit disposition after authenticated Stop and
   -- confirmed physical quiescence. It does not claim REG0/retirement success.
   procedure Park with No_Return;

   function Earliest (Left, Right : Word) return Word is
     (if Left <= Right then Left else Right);
private
   -- Pool carriage disposal retry interval (explicit, kernel milliseconds).
   Disposal_Retry_Milliseconds : constant Word := 100;
   -- Keep retirement/parking event and request drains finite under traffic.
   Retirement_Drain_Budget : constant Positive := 32;
end ACPI_One_Service;
