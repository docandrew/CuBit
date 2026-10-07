package body CuBit.Logging is
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Submitted : out Boolean) is
      pragma Unreferenced (Item);
   begin
      Last_Value := Value; Emissions := Emissions + 1; Submitted := True;
   end Emit;
end CuBit.Logging;
