with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_MCR_Access;
procedure MCR_Access_Tests is
   Initial : constant Unsigned_32 := 16#12000042#;
   Selector : Unsigned_32 := Initial;
   Owned : Boolean := True;
   Writes, Targets, Scenario : Natural := 0;
   Target : Unsigned_32 := 16#E18C#;
   function Owner return Boolean is (Owned);
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Offset = 16#FDC# then return Selector; end if;
      pragma Assert (Offset = Target and Selector = 16#83000042#);
      Targets := Targets + 1;
      return (if Scenario = 2 then Unsigned_32'Last else 16#8001#);
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      Writes := Writes + 1; Success := True;
      if Offset = 16#FDC# then
         Selector := Value;
         if Scenario = 1 and Writes = 1 then Success := False; end if;
         if Scenario = 3 and Value = Initial then Success := False; end if;
         if Scenario = 4 then Owned := False; end if;
      else
         pragma Assert (Offset = 16#E18C# and Selector = 16#83000042#);
         pragma Assert (Value = 16#80018001#);
         Targets := Targets + 1;
      end if;
   end Write32;
   package M is new Intel_GPU_MCR_Access (Owner, Read32, Write32);
   Value : Unsigned_32;
   OK : Boolean;
begin
   for Case_Number in 0 .. 4 loop
      declare Object : M.State; Old_Writes : Natural; begin
         Scenario := Case_Number; Selector := Initial;
         Owned := True; Writes := 0; Targets := 0; Value := 0;
         M.Access_Register (Object, 16#E18C#, 3, False, Value, OK);
         pragma Assert (OK = (Scenario = 0));
         pragma Assert (M.Failed (Object) = (Scenario /= 0));
         if Scenario /= 4 then pragma Assert (Selector = Initial); end if;
         if Scenario = 0 then
            pragma Assert (Value = 16#8001# and Targets = 1);
            Value := 16#80018001#;
            M.Access_Register (Object, 16#E18C#, 3, True, Value, OK);
            pragma Assert (OK and Targets = 2 and Selector = Initial);
         else
            Old_Writes := Writes;
            M.Access_Register (Object, 16#E18C#, 3, False, Value, OK);
            pragma Assert (not OK and Writes = Old_Writes);
         end if;
      end;
   end loop;
   declare Object : M.State; begin
      Scenario := 0; Owned := True; Selector := Initial;
      Writes := 0; Targets := 0; Target := 16#5584#;
      M.Access_Register (Object, Target, 3, False, Value, OK);
      pragma Assert (OK and Value = 16#8001# and Targets = 1 and Selector = Initial);
      Writes := 0; Targets := 0;
      M.Access_Register (Object, Target, 3, True, Value, OK);
      pragma Assert (not OK and M.Failed (Object) and Writes = 0 and Targets = 0);
   end;
   Ada.Text_IO.Put_Line ("MCR access PASS: multicast, selected reads, restoration and quarantine (mock MMIO)");
end MCR_Access_Tests;
