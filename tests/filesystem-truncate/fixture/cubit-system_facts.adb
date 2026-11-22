with Ada.Environment_Variables;
package body CuBit.System_Facts is
   function Wall_Clock_Offset return Unsigned_64 is
      Clock : constant String :=
        Ada.Environment_Variables.Value ("CUBIT_TEST_WALL_CLOCK", "");
   begin
      return (if Clock = "" then 0 else Unsigned_64'Value (Clock) * 1_000);
   end Wall_Clock_Offset;
end CuBit.System_Facts;
