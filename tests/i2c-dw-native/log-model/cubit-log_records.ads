package CuBit.Log_Records is
   type Severity is (Information, Warning);
   type Log_Record is null record;
   type Decoded is record
      Success : Boolean;
      Value : Log_Record;
   end record;
   function Make (Text : String; Level : Severity) return Decoded;
end CuBit.Log_Records;
