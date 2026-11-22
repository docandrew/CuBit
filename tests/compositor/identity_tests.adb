with Ada.Text_IO; use Ada.Text_IO;
with Compositor_Identity; use Compositor_Identity;
procedure Identity_Tests is
   Inputs : array (1 .. 6) of Value :=
     (0, 2 ** 31 - 1, 2 ** 32 - 1, 2 ** 32, Value'Last - 1, Value'Last) with Volatile;
   Expected : constant array (Inputs'Range) of Value :=
     (1, 2 ** 31, 2 ** 32, 2 ** 32 + 1, Value'Last, 0);
   Actual : Value;
begin
   pragma Assert (Value'Size = 64);
   for I in Inputs'Range loop
      Actual := Next (Inputs (I));
      pragma Assert (Actual = Expected (I));
   end loop;
   pragma Assert (Next (6, 7) = 7 and Next (7, 7) = 0);
   Put_Line ("IDENTITY: PASS 64-bit layout, signed/unsigned 32-bit crossings, maximum and non-wrapping refusal");
end Identity_Tests;
