package body CuBit.Log_Records is
   function Make (Text : String; Level : Severity := Information) return Decoded is
      R : Decoded;
   begin
      R.Value.Length := Text'Length;
      R.Value.Level := Level;
      R.Value.Text (1 .. Text'Length) := Text;
      return R;
   end;
end CuBit.Log_Records;
