package CuBit.Log_Records is
   type Log_Record is record
      Text : String (1 .. 512) := [others => ' '];
      Length : Natural := 0;
   end record;
   type Decoded is record
      Success : Boolean := True;
      Value : Log_Record;
   end record;
   function Make (Text : String) return Decoded;
end CuBit.Log_Records;
