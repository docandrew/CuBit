pragma Ada_2022;
with Ada.Text_IO;
with HID_Motion_Fragments;
with Pointer_Pending;
procedure Motion_Fragments_Tests is
   package F renames HID_Motion_Fragments;
   package P renames Pointer_Pending;
   use type F.Motion, P.Button_State;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   type Values is array (Positive range <>) of F.Motion;
   Edges : constant Values := [F.Motion'First, -4097, -3217, -2097, -2049,
     -2048, -2047, -1, 0, 1, 2047, 2048, 2097, 3217, 4097, F.Motion'Last];
   Swipes : constant Values := [-6000, -4096, -3217, -2097, -2048, -2047,
     -1, 0, 1, 2047, 2048, 2097, 3217, 4096, 6000];
   procedure First_Part (X, Y : F.Motion) is
      Remaining : F.Frame := (1, X, Y, -3, 4);
      Data : P.Report;
      Done : Boolean;
   begin
      F.Take (Remaining, 0, Data, Done);
      Check (Long_Long_Integer (X) = Long_Long_Integer (Remaining.X) + Long_Long_Integer (Data.X));
      Check (Long_Long_Integer (Y) = Long_Long_Integer (Remaining.Y) + Long_Long_Integer (Data.Y));
      Check (Data.Wheel + Remaining.Wheel = -3);
      Check (Data.Flags = 4);
      Check (Data.Buttons = (if Done then 1 else 0));
      Check ((if X >= 0 then Remaining.X in 0 .. X else Remaining.X in X .. 0));
      Check ((if Y >= 0 then Remaining.Y in 0 .. Y else Remaining.Y in Y .. 0));
   end First_Part;
   procedure Swipe (X, Y : F.Motion; Before, After : P.Button_State) is
      Remaining : F.Frame := (After, X, Y, 2, 8);
      Data : P.Report;
      Done : Boolean := False;
      Sum_X, Sum_Y : Long_Long_Integer := 0;
      Sum_Wheel, Parts : Natural := 0;
   begin
      while not Done loop
         F.Take (Remaining, Before, Data, Done);
         Parts := Parts + 1;
         Check (Parts <= 4);
         Check (Data.Flags = 8);
         Check (Data.Buttons = (if Done then After else Before));
         Check (Data.Wheel = (if Done then 2 else 0));
         Sum_X := Sum_X + Long_Long_Integer (Data.X);
         Sum_Y := Sum_Y + Long_Long_Integer (Data.Y);
         Sum_Wheel := Sum_Wheel + Data.Wheel;
      end loop;
      Check (Sum_X = Long_Long_Integer (X) and Sum_Y = Long_Long_Integer (Y));
      Check (Sum_Wheel = 2 and Remaining.X = 0 and Remaining.Y = 0 and Remaining.Wheel = 0);
   end Swipe;
begin
   for X of Edges loop
      for Y of Edges loop First_Part (X, Y); end loop;
   end loop;
   for X of Swipes loop
      for Y of Swipes loop
         Swipe (X, Y, 0, 1);
         Swipe (X, Y, 1, 0);
      end loop;
   end loop;
   Swipe (0, 0, 0, 1); Swipe (0, 0, 1, 0);
   Ada.Text_IO.Put_Line ("Motion fragments checks" & Checks'Image);
end Motion_Fragments_Tests;
