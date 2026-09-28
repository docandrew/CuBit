with Interfaces; use Interfaces;
with Intel_GPU_Engine_Stop;
with Ada.Text_IO;
procedure Engine_Stop_Tests is
   Mode : Unsigned_32 := 16#200#;
   Pending, Power : Unsigned_32 := 0;
   Clock, Step : Unsigned_64 := 0;
   Writes, Power_Reads : Natural := 0;
   function Read_Mode return Unsigned_32 is (Mode);
   function Read_Pending return Unsigned_32 is (Pending);
   function Read_Power return Unsigned_32 is
   begin Power_Reads := Power_Reads + 1; return Power; end;
   procedure Write_Mode (Value : Unsigned_32) is
   begin pragma Assert (Value = 16#01000100#); Writes := Writes + 1; end;
   procedure Write_Prefetch (Value : Unsigned_32) is
   begin pragma Assert (Value = 16#04000400#); Writes := Writes + 1; end;
   function Now return Unsigned_64 is (Clock);
   procedure Pause is
   begin Clock := Clock + Step; end;
   package Engine is new Intel_GPU_Engine_Stop
     (Read_Mode, Read_Pending, Read_Power, Write_Mode, Write_Prefetch, Now, Pause);
   use type Engine.Result;
   Status : Engine.Result;
begin
   Step := 1;
   for Request in Unsigned_32 range 0 .. 31 loop
      for Mask in Unsigned_32 range 0 .. 31 loop
         Clock := 0; Power_Reads := 0;
         Pending := Shift_Left (Request, 9) or Shift_Left (Mask, 25);
         Power := Request and Mask;
         Engine.Stop (8, Status);
         pragma Assert (Status = Engine.Stopped);
         pragma Assert (Clock = (if Power = 0 then 0 else 6));
         pragma Assert (Power_Reads = (if Power = 0 then 0 else 1));
      end loop;
   end loop;
   Pending := 16#02000200#; Power := 0;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Timed_Out);
   Power := 1;
   Engine.Stop (1, Status); -- exactly1 rounded us is not sufficient
   pragma Assert (Status = Engine.Timed_Out);
   Engine.Stop (2, Status); -- 2us also falls within the error allowance
   pragma Assert (Status = Engine.Timed_Out);
   Step := 0;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Timed_Out);
   Step := 1; Pending := Unsigned_32'Last;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Invalid_MMIO);
   Pending := 0; Mode := Unsigned_32'Last;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Invalid_MMIO);
   Mode := 0;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Timed_Out);
   Writes := 0;
   Engine.Stop (3, Status, 0);
   pragma Assert (Status = Engine.Timed_Out and Writes = 0);
   Clock := Unsigned_64'Last;
   Engine.Stop (3, Status);
   pragma Assert (Status = Engine.Invalid_Clock and Writes = 0);
   Ada.Text_IO.Put_Line ("PASS: engine stop and 1024 pending-forcewake combinations");
end Engine_Stop_Tests;
