pragma Ada_2022;
with Interfaces;
with ACPI_Service_Core;
with ACPI_Local_Hardware;
with ACPI_Local_Hardware.Pooled_Fields;
with ACPI_Local_Memory;
with ACPI_Local_Memory.Modules;
with ACPI_Local_Memory.Modules.Shutdown;
with ACPI_Global_Lock_Protocol;
with AML_Field_Layout;
with ACPI_PCI_Bus_Geometry;
generic
   with package Core is new ACPI_Service_Core (<>);
   Virtual_First, Virtual_Limit, Local_Scope : Interfaces.Unsigned_64;
   with function Arena_Is_Exclusive (First,Limit : Interfaces.Unsigned_64) return Boolean;
   with function Compatible_Envelope (First_Page,Pages : Interfaces.Unsigned_64) return Boolean;
   with procedure Begin_Window (Deadline_Microseconds : out Interfaces.Unsigned_64;
      Budget : out ACPI_Global_Lock_Protocol.Attempt_Budget; Available : out Boolean);
   with procedure PCI_Setup_Deadline (Deadline_Milliseconds : in out Interfaces.Unsigned_64;
      Available : in out Boolean);
   with procedure Resolve_PCI_Alias (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Bus_Geometry.P.Bus_Key; Accepted : in out Boolean);
   Maximum_Regions, Maximum_Mappings, Maximum_Field_Depth : Positive;
   with procedure Close_Consumers (Accepted : out Boolean);
   with procedure Other_Owners_View (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64);
   with procedure Inspect_IO_Window (Deadline_Microseconds : out Interfaces.Unsigned_64;
      Available : out Boolean);
-- Concrete one-service composition. Layout, physical-envelope compatibility,
-- clocks and ALL external retirement obligations are required actuals; there
-- are no permissive startup/arena/cleanup defaults or guessed accessors.
package ACPI_Local_Root is
   subtype Word is Interfaces.Unsigned_64;
   package Hardware is new ACPI_Local_Hardware
     (Virtual_First,Virtual_Limit,Local_Scope,Arena_Is_Exclusive);
   function Control_Allowed return Boolean;
   function Field_Access_Allowed (For_Write : Boolean) return Boolean;
   package Backing is new ACPI_Local_Memory
     (Hardware,Local_Scope,Control_Allowed,Field_Access_Allowed,Arena_Is_Exclusive,
      Maximum_Regions,Maximum_Mappings);
   package Fields is new Hardware.Pooled_Fields
     (Core,Backing.Regions.Cache,Backing.Memory,Compatible_Envelope,Begin_Window,
      Maximum_Field_Depth,PCI_Setup_Deadline => PCI_Setup_Deadline,
      Resolve_PCI_Alias => Resolve_PCI_Alias,Inspect_IO_Window => Inspect_IO_Window);
   package Evaluation is new Backing.Modules (Core,Fields);
   -- Local pool/cache/PCI/Field observations, independent of Core REG bookkeeping.
   function Backing_Quiet return Boolean;
   procedure Read_Retirement (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Word);
   package Shutdown is new Evaluation.Shutdown (Close_Consumers,Read_Retirement);
   procedure Maintain_Owner (Wake_Deadline_Milliseconds : out Word;
      May_Continue : out Boolean) renames Hardware.Maintain;
   procedure Retry_Pool_Disposal (Completed : out Boolean) renames Backing.Retry_Disposal;
   -- Explicit rejected-carriage disposal only; caller paces known failures.
   -- Shutdown contributes local pool/cache/PCI observations in addition to
   -- caller-supplied external EC/report/grant lifetimes. It never frees/maps.
   -- Configure Evaluation before loader; loader uses Execute_Module_Startup.
   -- Bind runtime consumers to Evaluation; finish setup after device _INI.
   -- Inspect_IO_Window must observe the same finite owner window for every
   -- Field, including unlocked data. The native delay owner's Inspect_Window
   -- has this shape; no FACS/global-lock availability is implied by inspection.
   -- Bind serving maintenance to Maintain_Owner, and drive Shutdown after
   -- serving stops; do not free Core/owners while May_Retire is false.
end ACPI_Local_Root;
