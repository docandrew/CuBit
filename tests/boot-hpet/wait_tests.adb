with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Monotonic_Wait;
procedure Wait_Tests is
   type Samples is array (Positive range <>) of Unsigned_64;
   Values : Samples (1 .. 4) := [others => 0];
   Reads, Pauses : Natural := 0;
   Fail_At : Natural := 0;
   procedure Read (Stamp : out Unsigned_64; Success : out Boolean) is
   begin
      Reads := Reads + 1;
      Stamp := Values (Reads);
      Success := Reads /= Fail_At;
   end Read;
   procedure Pause is
   begin Pauses := Pauses + 1; end Pause;
   package Delay_Clock is new Monotonic_Wait (Read, Pause);
   use Delay_Clock;
   Result : Outcome;
   procedure Reset (Input : Samples; Fail : Natural := 0) is
   begin Values := Input; Reads := 0; Pauses := 0; Fail_At := Fail; end Reset;
begin
   Reset ([0, 0, 0, 0]);
   At_Least (0, Unsigned_64'Last, 3, Result);
   pragma Assert (Result = Completed and Reads = 0);
   At_Least (Unsigned_64'Last, 1, 3, Result);
   pragma Assert (Result = Unrepresentable and Reads = 0);
   -- Every sampled duration/error pair rejects the premature boundary and
   -- accepts the inclusive conservative boundary, including a nonzero epoch.
   for Duration in Unsigned_64 range 1 .. 100 loop
      for Error in Unsigned_64 range 0 .. 10 loop
         Reset ([123, 123 + Duration + Error - 1,
                 123 + Duration + Error, 0]);
         At_Least (Duration, Error, 3, Result);
         pragma Assert (Result = Completed and Reads = 3 and Pauses = 1);
      end loop;
   end loop;
   Reset ([9, 9, 9, 9]);
   At_Least (1, 2, 3, Result);
   pragma Assert (Result = Polls_Exhausted and Reads = 4 and Pauses = 2);
   Reset ([9, 11, 10, 99]);
   At_Least (5, 0, 3, Result);
   pragma Assert (Result = Regressed and Reads = 3);
   Reset ([Unsigned_64'Last, 0, 0, 0]);
   At_Least (1, 0, 3, Result);
   pragma Assert (Result = Regressed);
   Reset ([Unsigned_64'Last - 1, Unsigned_64'Last, 0, 0]);
   At_Least (1, 0, 3, Result);
   pragma Assert (Result = Completed);
   for Failure in 1 .. 4 loop
      Reset ([0, 0, 0, 0], Failure);
      At_Least (1, 0, 3, Result);
      pragma Assert (Result = Unavailable and Reads = Failure);
   end loop;
   Put_Line ("PASS: common bounded minimum delay, uncertainty and failure boundaries");
end Wait_Tests;
