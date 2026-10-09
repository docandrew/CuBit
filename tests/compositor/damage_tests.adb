with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Damage; use Compositor_Damage;
procedure Damage_Tests is
   S : State;
   type Pixels is array (Natural range 0 .. 31, Natural range 0 .. 31) of Boolean;
   Dirty : Pixels;
   Seed : Unsigned_32 := 12345;
   Checks : Natural := 0;
   function Next (Limit : Positive) return Natural is
   begin
      Seed := Seed * 1_664_525 + 1_013_904_223;
      return Natural (Shift_Right (Seed, 8) mod Unsigned_32 (Limit));
   end Next;
   procedure Check is
      Visits : Natural;
      B : constant Box := Bounds (S);
      Left, Top : Natural := 32;
      Right, Bottom : Natural := 0;
   begin
      pragma Assert (Valid (S));
      for Y in 0 .. 31 loop
         for X in 0 .. 31 loop
            if Dirty (X, Y) then
               Left := Natural'Min (Left, X); Top := Natural'Min (Top, Y);
               Right := Natural'Max (Right, X + 1); Bottom := Natural'Max (Bottom, Y + 1);
            end if;
            Visits := 0;
            for I in 1 .. Count (S) loop
               declare R : constant Box := Item (S, I); begin
                  if X >= R.Left and X < R.Right and Y >= R.Top and Y < R.Bottom then
                     Visits := Visits + 1;
                  end if;
               end;
            end loop;
            pragma Assert (Visits <= 1);
            pragma Assert (not Dirty (X, Y) or Visits = 1);
            pragma Assert (Visits = 0 or else
              (X >= B.Left and X < B.Right and Y >= B.Top and Y < B.Bottom));
         end loop;
      end loop;
      pragma Assert (B = (Left, Top, Right, Bottom));
      Checks := Checks + 1;
   end Check;
begin
   --  Two separated 2x2 updates copy 8 pixels instead of their 32x32 box.
   Clear (S);
   Add (S, (0, 0, 2, 2)); Add (S, (30, 30, 32, 32));
   pragma Assert (Count (S) = 2);
   pragma Assert (Bounds (S) = (0, 0, 32, 32));
   --  Contained repeats do not create copies; isolated overlaps stay local.
   Add (S, (0, 0, 1, 1)); pragma Assert (Count (S) = 2);
   Add (S, (1, 1, 3, 3)); pragma Assert (Count (S) = 2);
   pragma Assert (Covers (S, (0, 0, 3, 3)) and Covers (S, (30, 30, 32, 32)));
   -- A local envelope intersecting another region must preserve nonoverlap.
   Clear (S); Add (S, (0, 0, 2, 8)); Add (S, (4, 4, 6, 6));
   Add (S, (0, 0, 8, 2));
   pragma Assert (Count (S) = 1 and Covers (S, (0, 0, 8, 8)));
   Clear (S);
   for I in 0 .. Capacity - 1 loop Add (S, (I * 2, 0, I * 2 + 1, 1)); end loop;
   pragma Assert (Count (S) = Capacity);
   Add (S, (0, 0, 1, 2));
   pragma Assert (Count (S) = Capacity and Covers (S, (0, 0, 1, 2)));
   Add (S, (20, 20, 21, 21)); pragma Assert (Count (S) = 1);
   --  Extreme edges need no overflowing coordinate addition in the planner.
   Clear (S);
   Add (S, (0, 0, 1, 1));
   Add (S, (Natural'Last - 1, Natural'Last - 1, Natural'Last, Natural'Last));
   pragma Assert (Bounds (S).Right = Natural'Last);
   Add (S, (Natural'Last - 2, Natural'Last - 2, Natural'Last, Natural'Last));
   pragma Assert (Count (S) = 2 and Bounds (S).Right = Natural'Last);
   for Cycle in 1 .. 100 loop
      Clear (S); Dirty := (others => (others => False));
      for Step in 1 .. 32 loop
         declare
            X : constant Natural := Next (32);
            Y : constant Natural := Next (32);
            W : constant Positive := Next (32 - X) + 1;
            H : constant Positive := Next (32 - Y) + 1;
         begin
            for YY in Y .. Y + H - 1 loop
               for XX in X .. X + W - 1 loop Dirty (XX, YY) := True; end loop;
            end loop;
            Add (S, (X, Y, X + W, Y + H));
            Check;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("COMPOSITOR-DAMAGE: PASS" & Checks'Image &
     " coverage/nonoverlap grids; sparse, repeat, overflow and extreme edges");
end Damage_Tests;
