pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Windows;
with ACPI_Backend_Binding_Types;
-- Trusted backend only. No public wire operation calls Bind. Handle values
-- are retained references, not copied/derived kernel capabilities. The caller
-- must not close/revoke them independently while this owner retains backing.
-- Serialize all entries. Callbacks must return normally and never reenter.
-- Unexpected callback failure leaves Working set; the native runtime stops
-- the process. There is deliberately no reset, implicit retry or release.
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
   Maximum_Windows : Positive;
   Virtual_First, Virtual_Limit : Interfaces.Unsigned_64;
   Mapping_Page_Limit : Positive;
package ACPI_Backend_Bindings is
   use Interfaces;
   package W renames ACPI_Backend_Windows;
   package B renames ACPI_Backend_Binding_Types;
   package P renames W.P;
   package T renames W.T;
   type Context is limited private;
   type Statistics is record
      Stored, Ready, Map_Attempts : Natural := 0;
      Working : Boolean := False;
      Next_Virtual : Unsigned_64 := Virtual_First;
   end record;
   function Observe (State : Context) return Statistics;
   procedure Bind_Memory
     (State : in out Context; Window : W.Granted_Window;
      Authority : Memory_Authority; Deadline : B.Tick; Result : out B.Report);
   procedure Bind_Ports
     (State : in out Context; Window : W.Granted_Window;
      Authority : Port_Authority; Deadline : B.Tick; Result : out B.Report);
   -- Disable future views while retaining the handle, mapping and reservation.
   -- Accepted means the ID is retained, not that cleanup completed. No rearm.
   procedure Disable (State : in out Context; ID : W.Window_ID; Accepted : out Boolean);
   function Describe (State : Context; ID : W.Window_ID) return W.Window_Result;
   -- Internal lookup after named-record authorization. No ID arithmetic and
   -- no caller-controlled IPC address. The returned port reference is borrowed;
   -- it must never be closed or delegated by the consumer of this view.
   -- Views are borrowed for one serialized access; never cache across owner calls.
   type Access_View
     (Allowed : Boolean := False; Space : P.Address_Space := P.Memory_Space) is record
      case Allowed is
         when False => null;
         when True =>
            case Space is
               when P.Memory_Space => Virtual_Address : Unsigned_64;
               when P.IO_Space => Ports : Port_Authority; Port : Unsigned_16;
            end case;
      end case;
   end record;
   function Select_Access
     (State : Context; ID : W.Window_ID; Space : P.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean)
      return Access_View;
private
   type Slot_Count is new Natural range 0 .. Maximum_Windows;
   subtype Slot_Index is Slot_Count range 1 .. Slot_Count'Last;
   type Phase is (Inspecting, Refused, Mapping, Ready, Disabled, Mapping_Unknown);
   type Window_Entry is record
      Window : W.Granted_Window;
      At_Phase : Phase := Inspecting;
      Memory : Memory_Authority := No_Memory;
      Ports : Port_Authority := No_Ports;
      Virtual : Unsigned_64 := 0;
      Map_Attempted : Boolean := False;
   end record;
   type Entries is array (Slot_Index) of Window_Entry;
   type Context is limited record
      Items : Entries;
      Count : Slot_Count := 0;
      Cursor : Unsigned_64 := Virtual_First;
      Working, Seen_Clock : Boolean := False;
      Previous : B.Tick := 0;
   end record;
end ACPI_Backend_Bindings;
