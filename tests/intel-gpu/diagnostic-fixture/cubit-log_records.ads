package CuBit.Log_Records is
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);
   type Log_Record is record
      Text : String (1 .. 512) := [others => ' '];
      Length : Natural := 0;
      Level : Severity := Information;
   end record;
   type Decoded is record
      Success : Boolean := True;
      Value : Log_Record;
   end record;
   function Make (Text : String; Level : Severity := Information) return Decoded;
end CuBit.Log_Records;
