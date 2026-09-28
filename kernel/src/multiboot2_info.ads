pragma Ada_2022;
with Interfaces; use Interfaces;
with Multiboot_Memory_Map;
with Boot_Modules;
with Boot_Framebuffer;
with Firmware_Tables;

-- Pointer-free Multiboot2 admission. Raw physical reads live in Multiboot.
package Multiboot2_Info with SPARK_Mode, Pure is
   Loader_Magic : constant Unsigned_32 := 16#36D76289#;
   Maximum_Bytes : constant := 1024 * 1024; -- boot metadata resource budget
   subtype Bytes is Multiboot_Memory_Map.Bytes;
   type Module_Description is record
      First, Limit : Unsigned_32 := 0;
      Name : Boot_Modules.Module_Name;
   end record;
   type Module_Array is array (Boot_Modules.Module_Index) of Module_Description;
   type Snapshot is limited record
      Frame : Boot_Framebuffer.Raw_Description := (others => <>);
      Root : Firmware_Tables.Root_Result := (Status => Firmware_Tables.Truncated);
      Modules : Module_Array := [others => <>];
      Module_Count : Boot_Modules.Module_Count := 0;
   end record;
   type Status is
     (Success, Bad_Header, Bad_Tag, Duplicate_Tag, Missing_End,
      Missing_Map, Missing_Framebuffer, Invalid_Map, Invalid_Module,
      Invalid_Framebuffer, Invalid_ACPI, Boot_Services_Active, Capacity_Exceeded);

   function Read_32 (Data : Bytes; Offset : Natural) return Unsigned_32
     with Pre => Offset <= Data'Length and then Data'Length - Offset >= 4;
   function Header_Extent (Data : Bytes) return Natural
     with Post => Header_Extent'Result = 0 or else
       Header_Extent'Result in 16 .. Maximum_Bytes;
   -- All output is owned values, not references into the loader buffer.
   -- On failure counts are zero and Value is empty. Map slots beyond Count
   -- are never published. Geometry and module physical ownership are admitted
   -- by the existing Boot_Framebuffer/Boot_Modules policy after parsing.
   procedure Parse
     (Data : Bytes; Maximum_Address : Unsigned_64;
      Value : out Snapshot; Map : out Multiboot_Memory_Map.Entries;
      Count : out Natural; Result : out Status)
     with Post => Count <= Map'Length and then
       (if Result /= Success then Count = 0 and then Value.Module_Count = 0);
end Multiboot2_Info;
