with Ada.Text_IO;
with Interfaces;
with AML_Clock; use AML_Clock;
procedure Clock_Tests is
   use type Interfaces.Unsigned_64;
   Clock : State := Fresh;
   Value : Tick;
   Status : Sample_Status;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Sample (US : Tick; Available : Boolean; Expected : Sample_Status; Output, Last : Tick) is
   begin
      Observe (Clock, US, Available, Value, Status);
      Check (Status = Expected);
      Check (Value = Output);
      Check (Last_Accepted (Clock) = Last);
   end Sample;
begin
   Sample (0, True, Accepted, 0, 0);
   Sample (1, True, Accepted, 10, 10);
   Sample (1, True, Accepted, 10, 10);
   Sample (0, True, Regressed, 0, 10);
   Sample (2, False, Unavailable, 0, 10);
   Sample (2, True, Accepted, 20, 20);
   Sample (16#FFFF_FFFF#, True, Accepted, 42_949_672_950, 42_949_672_950);
   Sample (16#1_0000_0000#, True, Accepted, 42_949_672_960, 42_949_672_960);
   Sample (Max_Microseconds, True, Accepted, 18_446_744_073_709_551_610, 18_446_744_073_709_551_610);
   Sample (Max_Microseconds + 1, True, Out_Of_Range, 0, 18_446_744_073_709_551_610);
   Sample (Tick'Last, False, Unavailable, 0, 18_446_744_073_709_551_610);
   Sample (Tick'Last, True, Out_Of_Range, 0, 18_446_744_073_709_551_610);
   Sample (0, True, Regressed, 0, 18_446_744_073_709_551_610);
   Sample (Max_Microseconds, True, Accepted, 18_446_744_073_709_551_610, 18_446_744_073_709_551_610);
   Ada.Text_IO.Put_Line ("AML clock adapter checks:" & Checks'Image);
end Clock_Tests;
