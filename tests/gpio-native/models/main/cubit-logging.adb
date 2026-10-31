package body CuBit.Logging is
   procedure Emit (Object : in out Publisher; Item : CuBit.Log_Records.Log_Record; Submitted : out Boolean) is
      pragma Unreferenced (Item);
   begin Submitted := Object.Present; end Emit;
   procedure Disconnect (Object : in out Publisher; Done : out Boolean) is
   begin Object.Present := False; Done := True; end Disconnect;
end CuBit.Logging;
