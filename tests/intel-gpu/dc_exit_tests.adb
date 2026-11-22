with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_DC_Exit;
procedure DC_Exit_Tests is
   procedure Check (Low_Power : Boolean; Fault : Natural) is
      Initial : constant Unsigned_32 := 16#00300000# or (if Low_Power then 16#60000000# else 3);
      Register_Value : Unsigned_32 := Initial;
      Writes, Reads, Restores, Clock_Reads : Natural := 0;
      function Read_Control return Unsigned_32 is
      begin
         Reads := Reads + 1;
         return (if Fault = 1 then Unsigned_32'Last else Register_Value);
      end Read_Control;
      procedure Write_Control (Value : Unsigned_32; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         pragma Assert ((Value and 16#00300000#) = 16#00300000#);
         if Low_Power and Writes = 1 then
            pragma Assert (Value = 16#40300000#);
         else pragma Assert (Value = 16#00300000#); end if;
         Register_Value := Value; Success := Fault /= 2;
      end Write_Control;
      function Now_Us return Unsigned_64 is
      begin
         Clock_Reads := Clock_Reads + 1;
         return (if Fault = 3 then Unsigned_64'Last
                 elsif Fault = 4 then 0
                 else Unsigned_64 (Clock_Reads) * 100);
      end Now_Us;
      procedure Pause is begin null; end Pause;
      procedure Restore (Prior_Control : Unsigned_32; Success : out Boolean) is
      begin
         Restores := Restores + 1;
         pragma Assert (Prior_Control = Initial and Reads >= 8);
         pragma Assert (not Low_Power or else Clock_Reads >= 4);
         Success := Fault /= 5;
         if Fault = 6 then Register_Value := Register_Value or 1; end if;
      end Restore;
      package Exit_DC is new Intel_GPU_DC_Exit (Read_Control, Write_Control, Now_Us, Pause, Restore);
      use Exit_DC;
      R : Report;
      Saved_Reads, Saved_Writes : Natural;
   begin
      Execute (False, True, 10, R);
      pragma Assert (R.Status = Rejected and Reads = 0 and Writes = 0);
      Execute (True, False, 10, R);
      pragma Assert (R.Status = Rejected and Reads = 0 and Writes = 0);
      Execute (True, True, 10, R);
      if Fault = 0 or else (not Low_Power and Fault in 3 | 4) then
         pragma Assert (R.Status = Ready and Restores = 1);
         pragma Assert (Writes = (if Low_Power then 2 else 1));
      elsif Fault = 1 then pragma Assert (R.Status = Invalid_MMIO and Writes = 0);
      elsif Fault = 2 then pragma Assert (R.Status = Write_Failed and Restores = 0);
      elsif Fault = 3 then pragma Assert (R.Status = Clock_Unavailable and Writes = 0);
      elsif Fault = 4 then pragma Assert (R.Status = Delay_Failed and Restores = 0);
      elsif Fault = 5 then pragma Assert (R.Status = Restore_Failed);
      else pragma Assert (R.Status = Invalid_MMIO); end if;
      Saved_Reads := Reads; Saved_Writes := Writes;
      Execute (True, True, 10, R);
      pragma Assert (R.Status = Rejected and Reads = Saved_Reads and Writes = Saved_Writes);
   end Check;
begin
   for Low_Power in Boolean loop
      for Fault in 0 .. 6 loop Check (Low_Power, Fault); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("DC exit PASS: ordered exit, latch preservation, failures and no replay (hosted)");
end DC_Exit_Tests;
