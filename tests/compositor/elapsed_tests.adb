with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Elapsed; use Compositor_Elapsed;
procedure Elapsed_Tests is
   type Example is record
      First, Last : Tick;
      Accepted : Boolean;
      Delta_Us : Tick;
   end record;
   Cases : constant array (Positive range <>) of Example :=
     ((0, 0, True, 0), (17, 17, True, 0), (1, 1_001, True, 1_000),
      (10, 4_177, True, 4_167), (2 ** 32 - 2, 2 ** 32 + 3, True, 5),
      (Tick'Last - 100, Tick'Last - 1, True, 99),
      (0, Tick'Last - 1, True, Tick'Last - 1),
      (100, 99, False, 0), (1, 0, False, 0),
      (0, Tick'Last, False, 0), (Tick'Last, 0, False, 0),
      (Tick'Last, Tick'Last, False, 0));
begin
   for Case_Data of Cases loop
      declare R : constant Sample := Measure (Case_Data.First, Case_Data.Last);
      begin
         pragma Assert (R.Valid = Case_Data.Accepted);
         if R.Valid then pragma Assert (R.Microseconds = Case_Data.Delta_Us); end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("elapsed: PASS 12 clock boundary examples");
end Elapsed_Tests;
