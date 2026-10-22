with AML_Decode; use AML_Decode;
package body Alias_Test_Support is
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Path (Data : Bytes) return AML_Names.Name_Result is (AML_Names.Read_Name (Data));
   function Path (Text : String) return AML_Names.Name_Result is (Path (Enc (Text)));
   function Block (Opcode, Payload : Bytes) return Bytes is
      Length : Natural := Payload'Length + 1;
   begin
      if Length <= 63 then return Opcode & [Byte (Length)] & Payload; end if;
      Length := Payload'Length + 2;
      if Length > 4095 then raise Program_Error with "fixture package extent"; end if;
      return Opcode & [Byte (16#40# + Length mod 16), Byte (Length / 16)] & Payload;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Alias_Of (Source, Destination : Bytes) return Bytes is
     (Bytes'[16#06#] & Source & Destination);
   function Alias_Of (Source, Destination : String) return Bytes is
     (Alias_Of (Enc (Source), Enc (Destination)));
end Alias_Test_Support;
