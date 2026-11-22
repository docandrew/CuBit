pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with Platform_Pool_ABI;
with ACPI_Platform_Region_Cache;
with ACPI_Backend_Memory_Access;
generic
   Maximum_Regions, Maximum_Mappings : Positive;
   with function Arena_Is_Exclusive (First, Limit : Interfaces.Unsigned_64) return Boolean;
   with function May_Access (For_Write : Boolean) return Boolean;
package ACPI_Platform_Regions_Native is
   subtype Word is Interfaces.Unsigned_64;
   function Inspect_Pool (Held : CuBit.Handles.Device_Memory) return Boolean;
   procedure Map_Chunk (Held : CuBit.Handles.Device_Memory; Physical, Virtual, Pages : Word;
      Read_Only : Boolean; Status : out Word; Why : out Platform_Pool_ABI.Refusal_Record);
   procedure Read_Clock (Now : out Word; Available : out Boolean);
   package Cache is new ACPI_Platform_Region_Cache
      (CuBit.Handles.Device_Memory,CuBit.Handles.No_Device_Memory,
       Inspect_Pool,Arena_Is_Exclusive,Map_Chunk,Read_Clock,May_Access,
       Transfer => ACPI_Backend_Memory_Access.Transfer,Maximum_Regions => Maximum_Regions,Maximum_Mappings => Maximum_Mappings);
end ACPI_Platform_Regions_Native;
