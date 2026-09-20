with Interfaces;
-- Canonical fixed-width schema records, not Ada record images. References
-- are module-local. Decode validates representation; Define validates meaning.
package CCL.Types.Encoding with SPARK_Mode is
   Name_Size : constant := 1 + Maximum_Name_Length;
   Shape_Offset : constant := Name_Size;
   Count_Offset : constant := Shape_Offset + 1;
   Reserved_Offset : constant := Count_Offset + 1;
   Parts_Offset : constant := Reserved_Offset + 1;
   Part_Size : constant := Name_Size + 1;
   Definition_Size : constant := Parts_Offset + Maximum_Components * Part_Size;
   type Bytes is array (Natural range 0 .. Definition_Size - 1) of Interfaces.Unsigned_8;
   function Encode (Item : Description) return Bytes;
   procedure Decode (Data : Bytes; Item : out Description; Valid : out Boolean);
end CCL.Types.Encoding;
