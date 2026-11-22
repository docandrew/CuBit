pragma Ada_2022;
with Interfaces;
with ACPI_PCI_Bus_Geometry;
with ACPI_Backend_Binding_Types;
with ACPI_Local_PCI_Results;
generic
   type Held_Window is private;
   with function Current (Item : Held_Window) return Boolean;
   with function Geometry (Item : Held_Window) return ACPI_PCI_Bus_Geometry.Bus_Window;
   with function Virtual_Origin (Item : Held_Window) return Interfaces.Unsigned_64;
   with procedure Read_Clock (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
   with procedure Transfer (Virtual : Interfaces.Unsigned_64;
      Width : ACPI_PCI_Bus_Geometry.P.T.Access_Width;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean);
package ACPI_Local_PCI_Load is
   package G renames ACPI_PCI_Bus_Geometry;
   package P renames G.P;
   package B renames ACPI_Backend_Binding_Types;
   package R renames ACPI_Local_PCI_Results;
   type Context is limited private;
   function Pending (IO : Context) return Boolean;
   function Is_Stopped (IO : Context) return Boolean;
   -- Caller authenticates scope/epoch/record and retains the original window.
   -- Guard is synchronous, not retained, and checks the caller's physical
   -- Field scope immediately before the scalar load. No packet, broker,
   -- mapping, write, new physical owner or implicit retry.
   procedure Read (IO : in out Context; Item : Held_Window; Region : P.Region;
      Offset : P.Word; Width : P.T.Access_Width; Deadline_Microseconds : B.Tick;
      Guard : not null access function return Boolean; Result : out R.Result);
private
   type Context is limited record Working, Halted : Boolean := False; end record;
end ACPI_Local_PCI_Load;
