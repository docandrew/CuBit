package body CuBit.Log_Records is
   function Make (Text : String; Level : Severity) return Decoded is
      pragma Unreferenced (Text, Level);
   begin return (Success => True, Value => (null record)); end Make;
end CuBit.Log_Records;
