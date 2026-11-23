pragma Ada_2022;
with Interfaces;
-- Complete copied names only. No Core identity, callable handle or authority.
package ACPI_Source_Paths with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   Maximum_Segments:constant Positive:=64;
   Maximum_Characters:constant Positive:=5*Maximum_Segments;
   subtype Path_Length is Natural range 0..Maximum_Characters;
   type Status is (Unknown,Known,Too_Long);
   type Path is record
      State:Status:=Unknown;
      Length:Path_Length:=0;
      Text:String(1..Maximum_Characters):=[others=>' '];
   end record;
   Empty:constant Path:=(others=><>);
   function Same(Left,Right:Path) return Boolean with Global=>null;
   -- Seven bytes little endian, always representable by a signed CCL integer.
   -- Offset is checked at full width before conversion; offset=length is empty.
   function Chunk(Value:Path;Offset:Interfaces.Unsigned_64) return Interfaces.Unsigned_64
     with Global=>null,Post=>Chunk'Result<=16#00FF_FFFF_FFFF_FFFF#;
end ACPI_Source_Paths;
