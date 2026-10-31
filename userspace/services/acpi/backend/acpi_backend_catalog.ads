pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Backend_Windows;
with ACPI_Backend_Bindings;
with ACPI_Backend_Binding_Types;
with ACPI_Region_Policy;
with ACPI_Region_Protocol;
generic
   type Memory_Authority is private;
   No_Memory : Memory_Authority;
   type Port_Authority is private;
   No_Ports : Port_Authority;
   with procedure Inspect_Memory
     (Authority : Memory_Authority; Item : out ACPI_Backend_Binding_Types.Description);
   with procedure Inspect_Ports
     (Authority : Port_Authority; Item : out ACPI_Backend_Binding_Types.Description);
   with procedure Map_Memory
     (Authority : Memory_Authority; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean);
   with procedure Read_Clock
     (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
   -- One exact scalar access through an already validated borrowed view.
   -- No retained view, callback reentry, implicit RMW or automatic retry.
   with procedure Transfer_Memory
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width;
      For_Write : Boolean; Input : Interfaces.Unsigned_64;
      Output : out Interfaces.Unsigned_64; Completed : out Boolean);
   with procedure Transfer_Ports
     (Authority : Port_Authority; Port : Interfaces.Unsigned_16;
      Width : ACPI_FADT.Transactions.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean);
   Maximum_Windows, Maximum_Registers, Maximum_Scopes : Positive;
   Virtual_First, Virtual_Limit : Interfaces.Unsigned_64;
   Mapping_Page_Limit : Positive;
package ACPI_Backend_Catalog is
   use Interfaces;
   package W renames ACPI_Backend_Windows;
   package B renames ACPI_Backend_Binding_Types;
   package P renames ACPI_Region_Policy;
   package Wire renames ACPI_Region_Protocol;
   type Revision_Number is new Unsigned_64;
   No_Revision : constant Revision_Number := 0;
   type Context is limited private;
   -- Backend-local typed handles, never exported by inventory or scalar IPC.
   subtype Memory_Handle is Memory_Authority;
   subtype Port_Handle is Port_Authority;
   Null_Memory : constant Memory_Handle := No_Memory;
   Null_Ports : constant Port_Handle := No_Ports;
   -- Describe only an already-held handle. Bind rechecks the current handle
   -- independently; these copied facts are not admission or authority.
   procedure Describe_Memory (Authority : Memory_Handle; Item : out B.Description);
   procedure Describe_Ports (Authority : Port_Handle; Item : out B.Description);
   -- Trusted startup only; these are not public wire operations. Scope stamps
   -- must come from independently authorized kernel endpoint provisioning.
   procedure Bind_Memory
     (State : in out Context; Window : W.Granted_Window;
      Authority : Memory_Authority; Deadline : B.Tick; Result : out B.Report);
   procedure Bind_Ports
     (State : in out Context; Window : W.Granted_Window;
      Authority : Port_Authority; Deadline : B.Tick; Result : out B.Report);
   procedure Authorize_Scope (State : in out Context; Stamp : Unsigned_64; Accepted : out Boolean);
   type Add_Code is (Added, Setup_Closed, Scope_Unavailable, Catalog_Full,
                    Duplicate_Register, Conflicting_Resource, Derivation_Refused,
                    Window_Unavailable, Record_Refused);
   type Add_Report is record
      Status : Add_Code := Record_Refused;
      Admission : W.Outcome := W.Invalid_Record;
   end record;
   -- A repeated ID in a different scope must retain canonical geometry/window
   -- and derive a subset from its original delegable root. No new authority is
   -- established merely by a description or by ordinary endpoint re-badging.
   procedure Add
     (State : in out Context; Window : W.Window_ID;
      Item : P.Configuration; Writes : W.Write_Safety; Result : out Add_Report);
   procedure Seal (State : in out Context; Accepted : out Boolean);
   -- Stamp comes ONLY from authenticated kernel receive metadata. The public
   -- scalar protocol contains epoch/ID/value/zero, never address/offset/width.
   -- Pending completion correlation remains private; uncertainty stops all
   -- hardware dispatch across scopes/aliases without releasing any backing.
   procedure Dispatch
     (State : in out Context; Stamp : Unsigned_64;
      Item : Wire.Packet; Reply : out Wire.Packet);
   type Inventory (Available : Boolean := False) is record
      case Available is
         when True => Revision : Revision_Number; Count : Natural;
         when False => null;
      end case;
   end record;
   function Header (State : Context; Stamp : Unsigned_64) return Inventory;
   type Description (Available : Boolean := False) is record
      case Available is
         when True => Item : P.Configuration; Epoch : Unsigned_64; Exact_Write_Safe : Boolean;
         when False => null;
      end case;
   end record;
   -- Descriptions are copied metadata, not capabilities. Ordinals enumerate
   -- only this authenticated scope and require the same frozen revision.
   function Describe
     (State : Context; Stamp : Unsigned_64; Revision : Revision_Number;
      Ordinal : Natural) return Description;
   procedure Disable_Window
     (State : in out Context; ID : W.Window_ID; Accepted : out Boolean);
   procedure Revoke_Resource (State : in out Context; ID : Unsigned_64; Accepted : out Boolean);
   procedure Revoke_Scope (State : in out Context; Stamp : Unsigned_64; Accepted : out Boolean);
   type Statistics is record
      Sealed, Working, Quarantined : Boolean := False;
      Registers, Active_Registers, Pending, Retained_Windows : Natural := 0;
   end record;
   function Observe (State : Context) return Statistics;
private
   package Bindings is new ACPI_Backend_Bindings
     (Memory_Authority, No_Memory, Port_Authority, No_Ports,
      Inspect_Memory, Inspect_Ports, Map_Memory, Read_Clock,
      Maximum_Windows, Virtual_First, Virtual_Limit, Mapping_Page_Limit);
   type Slot_Count is new Natural range 0 .. Maximum_Registers;
   subtype Slot_Index is Slot_Count range 1 .. Slot_Count'Last;
   type Register_Entry is record
      Region : P.State;
      Window : W.Window_ID := W.No_Window;
      Root : Slot_Count := 0;
      Writes : W.Write_Safety := W.Unestablished;
   end record;
   type Register_Array is array (Slot_Index) of Register_Entry;
   type Scope_Count is new Natural range 0 .. Maximum_Scopes;
   subtype Scope_Index is Scope_Count range 1 .. Scope_Count'Last;
   type Scope_Array is array (Scope_Index) of Unsigned_64;
   type Context is limited record
      Windows : Bindings.Context;
      Items : Register_Array;
      Scopes : Scope_Array := [others => 0];
      Count : Slot_Count := 0;
      Scope_Last : Scope_Count := 0;
      Sealed, Working, Quarantined : Boolean := False;
      Revision : Revision_Number := No_Revision;
   end record;
end ACPI_Backend_Catalog;
