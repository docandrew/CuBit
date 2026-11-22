package body CuBit.Logging is
   function Bound_To (Target : Natural) return Publisher is
      pragma Unreferenced (Target);
   begin return Result : Publisher; end Bound_To;
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Submitted : out Boolean) is
      pragma Unreferenced (Item, Value);
   begin
      Emissions := Emissions + 1;
      if Disconnects > 0 then Late_Emissions := Late_Emissions + 1; end if;
      Submitted := True;
   end Emit;
   procedure Disconnect (Item : in out Publisher; Done : out Boolean) is
      pragma Unreferenced (Item);
   begin
      Disconnects := Disconnects + 1;
      if On_Disconnect /= null then On_Disconnect.all; end if;
      Done := Complete;
   end Disconnect;
end CuBit.Logging;
