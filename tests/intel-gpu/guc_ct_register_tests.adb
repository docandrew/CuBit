with Ada.Text_IO; with Interfaces;
with Intel_GPU_GuC_CT_Setup; with Intel_GPU_GuC_CT_Register;
procedure GuC_CT_Register_Tests is
   use Interfaces; use Intel_GPU_GuC_CT_Setup;
   Calls, Failure_Step, Mode, Checks, Lose_Check : Natural := 0;
   Owner : Boolean := True;
   Expected : constant Plan := Prepare (16#200000#, 32768, 4096);
   function Ready return Boolean is
   begin
      Checks := Checks + 1;
      if Checks = Lose_Check then Owner := False; end if;
      return Owner;
   end Ready;
   procedure Send (Request : Intel_GPU_GuC_CT_Setup.Request;
                   Reply : out Unsigned_32; Success : out Boolean) is
   begin
      Calls := Calls + 1;
      pragma Assert (Calls <= 7);
      pragma Assert (Request = (if Calls = 7 then Expected.Enable
                               else Expected.Register_Buffers (Calls)));
      Reply := (if Calls = 7 then 16#F0000000# else 16#F0000001#);
      Success := True;
      if Calls = Failure_Step then
         case Mode is
            when 1 => Success := False;
            when 2 => Reply := Reply xor 1;
            when 3 => Owner := False;
            when others => null;
         end case;
      end if;
   end Send;
   package Driver is new Intel_GPU_GuC_CT_Register (Ready, Send);
   use Driver;
   Status : Result;
   procedure Reset is
   begin Calls := 0; Checks := 0; Lose_Check := 0; Owner := True; end Reset;
begin
   for M in 1 .. 3 loop
      Mode := M;
      for F in 1 .. 7 loop
         declare Object : Registration; begin
            Reset; Failure_Step := F;
            Execute (Object, 16#200000#, 32768, 4096, Status);
            pragma Assert (Status = (case M is when 1 => Transport_Failed,
              when 2 => (if F = 7 then Enable_Refused else Registration_Refused),
              when others => Ownership_Lost));
            pragma Assert (Attempted (Object) and not Enabled (Object));
            pragma Assert (Calls = F and Last_Step (Object) = F);
            Owner := True;
            Execute (Object, 16#200000#, 32768, 4096, Status);
            pragma Assert (Status = Rejected and Calls = F);
         end;
      end loop;
   end loop;
   Mode := 0; Failure_Step := 0;
   -- Admission + before/after each of seven exchanges.
   for Lost in 1 .. 15 loop
      declare Object : Registration; begin
         Reset; Lose_Check := Lost;
         Execute (Object, 16#200000#, 32768, 4096, Status);
         pragma Assert (Status = (if Lost = 1 then Rejected else Ownership_Lost));
         pragma Assert (Calls = (Lost - 1) / 2 and not Enabled (Object));
      end;
   end loop;
   declare Object : Registration; begin
      Reset;
      Execute (Object, 16#200001#, 32768, 4096, Status);
      pragma Assert (Status = Rejected and not Attempted (Object) and Calls = 0);
      Execute (Object, 16#200000#, 32768, 4096, Status);
      pragma Assert (Status = Complete and Enabled (Object) and Calls = 7);
      pragma Assert (Last_Step (Object) = 7 and Last_Reply (Object) = 16#F0000000#);
      Execute (Object, 16#200000#, 32768, 4096, Status);
      pragma Assert (Status = Rejected and Calls = 7);
      Owner := False; pragma Assert (not Enabled (Object));
   end;
   Ada.Text_IO.Put_Line ("GuC CT registration PASS: seven exchanges, all partial failures, ownership loss, no retries");
end GuC_CT_Register_Tests;
