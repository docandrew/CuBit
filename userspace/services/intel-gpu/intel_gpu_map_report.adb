package body Intel_GPU_Map_Report with SPARK_Mode is
   use type CuBit.Log_Records.Severity;
   Hex_Digits : constant String (1 .. 16) := "0123456789ABCDEF";

   function Hex (Value : Unsigned_64) return String
     with Post => Hex'Result'Length in 3 .. 18
   is
      Text  : String (1 .. 16) := [others => '0'];
      V     : Unsigned_64 := Value;
      First : Positive range 1 .. 16 := 16;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
         if Text (I) /= '0' then
            First := I;
         end if;
      end loop;
      return "0x" & Text (First .. 16);
   end Hex;

   function Denied (Physical, Bytes : Unsigned_64) return String is
     ("map-denied " & Hex (Physical) & " +" & Hex (Bytes));

   Marker : constant String := "denied";

   function Escalate
     (Text : String; Level : CuBit.Log_Records.Severity)
      return CuBit.Log_Records.Severity is
   begin
      if Level < CuBit.Log_Records.Warning and then Text'Length >= Marker'Length then
         for I in Text'First .. Text'Last - (Marker'Length - 1) loop
            if Text (I .. I + (Marker'Length - 1)) = Marker then
               return CuBit.Log_Records.Warning;
            end if;
         end loop;
      end if;
      return Level;
   end Escalate;
end Intel_GPU_Map_Report;
