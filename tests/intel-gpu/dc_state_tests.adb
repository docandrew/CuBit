with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_DC_State; use Intel_GPU_DC_State;
procedure DC_State_Tests is
   B, A : Snapshot;
begin
   for Requests in Unsigned_32 range 0 .. 15 loop
      for States in Unsigned_32 range 0 .. 15 loop
         B := [PLL => 16#80000020#, others => 0]; A := B;
         A (PLL) := 16#C0000020#;
         for F in Buffer_0 .. Buffer_3 loop
            if (Requests and Shift_Left (1, Field'Pos (F) - Field'Pos (Buffer_0))) /= 0 then
               B (F) := 16#80000000#;
            end if;
            A (F) := B (F);
            if (States and Shift_Left (1, Field'Pos (F) - Field'Pos (Buffer_0))) /= 0 then
               A (F) := A (F) or 16#40000000#;
            end if;
         end loop;
         pragma Assert ((Check (B, A) = Preserved) = (Requests = States));
      end loop;
   end loop;
   B := [PLL => 16#C0000020#, others => 0];
   for F in Field loop
      A := B; A (F) := Unsigned_32'Last;
      pragma Assert (Check (B, A) = Invalid_Read);
      pragma Assert (Check (A, B) = Invalid_Read);
   end loop;
   A := B; A (Clock_Control) := 1;
   pragma Assert (Check (B, A) = Clock_Changed);
   A := B; A (PLL) := 16#C0000021#;
   pragma Assert (Check (B, A) = Clock_Changed);
   A := B; A (PLL) := 16#80000020#;
   pragma Assert (Check (B, A) = Clock_Unsettled);
   A := B; A (PLL) := A (PLL) or 16#00800000#;
   pragma Assert (Check (B, A) = Clock_Unsettled);
   A := B; A (Reference) := 16#60000000#;
   pragma Assert (Check (B, A) = Invalid_Clock);
   A := B; A (Reference) := 16#20000000#;
   pragma Assert (Check (B, A) = Clock_Changed);
   A := B; A (PLL) := 16#C0000000#;
   pragma Assert (Check (B, A) = Invalid_Clock);
   for F in Buffer_0 .. Buffer_3 loop
      A := B; A (F) := 16#C0000000#;
      pragma Assert (Check (B, A) = Buffer_Changed);
   end loop;
   B := [others => 0]; pragma Assert (Check (B, B) = Preserved);
   Ada.Text_IO.Put_Line ("DC retained-state PASS: 256 buffer masks, clock changes and invalid samples");
end DC_State_Tests;
