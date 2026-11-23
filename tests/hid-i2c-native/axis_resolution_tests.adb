pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with HID_Axis_Resolution;
with HID_Axis_Motion;
with HID_Motion_Fragments;
with Pointer_Pending;
procedure Axis_Resolution_Tests is
   package R renames HID_Axis_Resolution;
   package M renames HID_Axis_Motion;
   package F renames HID_Motion_Fragments;
   use type R.Status, F.Motion;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   G : R.Gain;
   Result : R.Status;
   procedure Expect (L, P : R.Span; Unit : Natural; Exp : R.Unit_Exponent;
      N, D : Long_Long_Integer) is
   begin
      R.Resolve (L, P, Interfaces.Unsigned_32 (Unit), Exp, (1, 1), G, Result);
      Check (Result = R.Normalized);
      Check (G.N * D = N * G.D);
   end Expect;
   procedure Conservation (Delta_Counts : M.Delta_Value; Scale : R.Gain;
      Reverse_Axis : Boolean; Before : F.Motion) is
      Move, Carry : F.Motion;
      Remaining : F.Frame;
      Part : Pointer_Pending.Report;
      Done : Boolean := False;
      Sum : Long_Long_Integer := 0;
   begin
      M.Scale (Delta_Counts, Scale, Reverse_Axis, Before, Move, Carry);
      Check ((if Reverse_Axis then -Delta_Counts else Delta_Counts) * Scale.N
        + Long_Long_Integer (Before) = Long_Long_Integer (Move) * Scale.D + Long_Long_Integer (Carry));
      Check (abs Long_Long_Integer (Carry) < Scale.D);
      -- Large extrema use the exact one-step contract; bounded practical moves
      -- additionally exercise the entire motion -> wire fragmentation path.
      if abs Move <= 100_000 then
         Remaining := (Buttons => 0, X => Move, Y => 0, Wheel => 0, Flags => 0);
         while not Done loop
            F.Take (Remaining, 0, Part, Done);
            Sum := Sum + Long_Long_Integer (Part.X);
         end loop;
         Check (Sum = Long_Long_Integer (Move));
      end if;
   end Conservation;
   type Counts is array (Positive range <>) of M.Delta_Value;
   Samples : constant Counts := [M.Delta_Value'First, -3217, -2097, -1,
      0, 1, 2097, 3217, M.Delta_Value'Last];
   type Gains is array (Positive range <>) of R.Gain;
   Scales : constant Gains := [(1, 1), (32, 1), (500_000, 408_559),
      (R.Numerator'Last, 1), (R.Numerator'Last, R.Denominator'Last)];
begin
   Expect (3217, 1000, 16#11#, -2, 500_000, 3217 * 127);
   Expect (2097, 500, 16#11#, -2, 250_000, 2097 * 127);
   Expect (3217, 254, 16#11#, -2, 1000, 3217);
   Expect (3217, 1, 16#13#, 0, 1000, 3217);
   Expect (3217, 1000, 16#13#, -3, 1000, 3217);
   Expect (3217, 1, 16#13#, -8, 1, 321_700_000);
   R.Resolve (3217, 1000, 0, 7, (3, 7), G, Result);
   Check (Result = R.Caller_Units and G.N = 3 and G.D = 7);
   R.Resolve (3217, 1000, 16#1001#, -4, (1, 1), G, Result);
   Check (Result = R.Unsupported_Unit);
   R.Resolve (1, R.Span'Last, 16#13#, 7, (R.Numerator'Last, 1), G, Result);
   Check (Result = R.Capacity_Exceeded);
   for Delta_Counts of Samples loop
      for Scale of Scales loop
         for Reverse_Axis in Boolean loop
            Conservation (Delta_Counts, Scale, Reverse_Axis, 0);
            Conservation (Delta_Counts, Scale, Reverse_Axis, F.Motion (Scale.D - 1));
            Conservation (Delta_Counts, Scale, Reverse_Axis, F.Motion (1 - Scale.D));
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Axis resolution checks" & Checks'Image);
end Axis_Resolution_Tests;
