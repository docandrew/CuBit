with Ada.Text_IO;
with AML_Clock;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Timer_Verification; use Timer_Verification;
procedure Timer_Clock_Tests is
   use type Integer_Value;
   Clock : Clock_State;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Sample (US : Integer_Value; Available : Boolean;
                     Expected : Execution_Status; Value : Integer_Value;
                     Width : Integer_Width := Bits_64) is
      Old_Reads : constant Natural := Clock.Reads;
   begin
      Clock.Microseconds := US; Clock.Available := Available;
      Run ([16#A4#,16#5B#,16#33#], Width, 100, Clock, Result);
      Check (Result.Status = Expected);
      if Expected = Returned then Check (Result.Value = Value); end if;
      Check (Clock.Reads = Old_Reads + 1 and Clock.Active = 0);
   end Sample;
begin
   Sample (0, True, Returned, 0);
   Sample (1, True, Returned, 10);
   Sample (1, True, Returned, 10);
   Sample (0, True, Unsupported, 0);
   Check (AML_Clock.Last_Accepted (Clock.Adapter) = 10);
   Sample (2, False, Unsupported, 0);
   Check (AML_Clock.Last_Accepted (Clock.Adapter) = 10);
   Sample (2, True, Returned, 20);
   Sample (429_496_730, True, Returned, 4, Bits_32);
   Check (AML_Clock.Last_Accepted (Clock.Adapter) = 4_294_967_300);
   Sample (429_496_730, True, Returned, 4_294_967_300);
   Sample (AML_Clock.Max_Microseconds, True, Returned, 18_446_744_073_709_551_610);
   Sample (AML_Clock.Max_Microseconds + 1, True, Unsupported, 0);
   Check (AML_Clock.Last_Accepted (Clock.Adapter) = 18_446_744_073_709_551_610);
   declare
      Fresh_Clock : Clock_State;
   begin
      Run ([16#A4#,16#5B#,16#33#], Bits_64, 0, Fresh_Clock, Result);
      Check (Result.Status = Budget_Exceeded and Fresh_Clock.Reads = 0 and Fresh_Clock.Active = 0);
   end;
   Ada.Text_IO.Put_Line ("Timer clock integration checks:" & Checks'Image);
end Timer_Clock_Tests;
