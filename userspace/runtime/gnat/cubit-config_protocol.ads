pragma Ada_2022;
with Interfaces;

package CuBit.Config_Protocol with Pure, SPARK_Mode is
   type Operation is (Get_Value, Set_Value, Delete_Value, List_Keys);
   for Operation use
     (Get_Value => 16#0600#, Set_Value => 16#0601#,
      Delete_Value => 16#0602#, List_Keys => 16#0603#);
   Maximum_Key : constant := 128;
   Maximum_Value : constant := 4096;
   Buffer_Size : constant := 8192;
   subtype Key_Length is Natural range 0 .. Maximum_Key;
   subtype Value_Length is Natural range 0 .. Maximum_Value;
   type Request_Bounds is record
      Key : Key_Length := 0;
      Value : Value_Length := 0;
      Input_Bytes : Natural range 0 .. Maximum_Key + Maximum_Value := 0;
      Mapping_Bytes : Positive range 1 .. Buffer_Size := 1;
   end record;
   --  Four words: slot, generation, key size, value size (zero except Set).
   --  All validation precedes integer narrowing.
   procedure Decode
     (Op : Operation; Word_Count : Natural;
      Key, Value : Interfaces.Unsigned_64;
      Bounds : out Request_Bounds; Valid : out Boolean);
end CuBit.Config_Protocol;
