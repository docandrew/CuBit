with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces;
with Buddy_Geometry; use Buddy_Geometry;
procedure Main is
   use type Interfaces.Unsigned_64;
   package Random_Frames is new Ada.Numerics.Discrete_Random (Frame);
   Generator : Random_Frames.Generator;
   Parent, Left, Right : Block;
   procedure Check_Split (Start : Frame; Span : Frame_Count) is
      Parent : constant Block := Make (Start, Span);
      Left, Right : Block;
   begin
      Split (Parent, Left, Right);
      pragma Assert (First (Left) = Start);
      pragma Assert (Last (Left) < First (Right));
      pragma Assert (First (Right) = Start + Span / 2);
      pragma Assert (Last (Right) = Start + Span - 1);
      pragma Assert (Can_Merge (Left, Right));
      pragma Assert (not Can_Merge (Right, Left));
      pragma Assert (not Can_Merge (Left, Left));
      pragma Assert (Merge (Left, Right) = Parent);
   end Check_Split;
begin
   -- Includes odd/non-power-of-two extents: the geometry core works for any
   -- aligned even span, while the kernel supplies spans from buddy orders.
   for Limit in Frame range 0 .. 64 loop
      for Span in Frame_Count range 1 .. 64 loop
         for Start in Frame range 0 .. 64 loop
            pragma Assert (Fits (Start, Span, Limit) =
              (Start + Span - 1 <= Limit and Start mod Span = 0));
         end loop;
      end loop;
   end loop;
   for Span in Frame_Count range 1 .. 64 loop
      if Span mod 2 = 0 then
         for Index in Count range 0 .. 64 loop
            Check_Split (Index * Span, Span);
         end loop;
      end if;
   end loop;

   -- Adjacent but belonging to different parents is not sufficient to merge.
   pragma Assert (not Can_Merge (Make (1, 1), Make (2, 1)));
   pragma Assert (not Can_Merge (Make (0, 1), Make (2, 1)));
   pragma Assert (not Can_Merge (Make (0, 2), Make (2, 1)));
   pragma Assert (not Can_Merge (Make (0, Frame_Count'Last),
                                 Make (Frame_Count'Last, Frame_Count'Last)));

   Random_Frames.Reset (Generator, 20260913);
   for O in 1 .. 39 loop
      Parent := Make (0, 2 ** O);
      Split (Parent, Left, Right);
      pragma Assert (Can_Merge (Left, Right));
      pragma Assert (Merge (Left, Right) = Parent);
      Check_Split (Frame'Last - 2 ** O + 1, 2 ** O);
      for Trial in 1 .. 10_000 loop
         declare
            Span : constant Frame_Count := 2 ** O;
            Start : constant Frame := (Random_Frames.Random (Generator) / Span) * Span;
         begin
            Check_Split (Start, Span);
            -- Independent machine-bit oracle for the kernel's dyadic case.
            Parent := Make (Start, Span);
            Split (Parent, Left, Right);
            pragma Assert (Interfaces.Unsigned_64 (First (Right)) =
              (Interfaces.Unsigned_64 (First (Left)) xor
               Interfaces.Unsigned_64 (Span / 2)));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS buddy geometry: boundaries, invalid pairs, 390000 full-width splits");
end Main;
