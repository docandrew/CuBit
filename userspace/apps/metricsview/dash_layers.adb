pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;
with Client_Canvas_Geometry;

package body Dash_Layers is
   package G renames Client_Canvas_Geometry;
   use type CuBit.UI.Rect;
   Bytes_Per_Pixel : constant := 4;

   type Pixel_Array is array (Natural range <>) of Unsigned_32;
   type Pixel_Access is access Pixel_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Pixel_Array, Pixel_Access);

   Buffer : Pixel_Access;
   --  Move_Blocks' sources, grown as needed and kept.
   Scratch : Pixel_Access;
   Logical_W, Logical_H : Natural := 0;
   Physical_W, Physical_H : Natural := 0;
   Num, Den : G.Component := 1;

   type Rect_List is array (1 .. Maximum_Dirty) of Rect;
   Dirty_Rects : Rect_List := [others => (others => 0)];
   Used : Natural := 0;

   function Union (A, B : Rect) return Rect is
     (if CuBit.UI.Is_Empty (A) then B elsif CuBit.UI.Is_Empty (B) then A else CuBit.UI.Union_Rect (A, B));
   function Area (R : Rect) return Natural is (R.w * R.h);

   procedure Resize (Width, Height : Natural; Numerator, Denominator : Positive) is
      N : constant G.Component := G.Component (Numerator);
      D : constant G.Component := G.Component (Denominator);
      W : constant Natural := G.Edge (Natural'Min (Width, G.Logical_Edge'Last), N, D);
      H : constant Natural := G.Edge (Natural'Min (Height, G.Logical_Edge'Last), N, D);
   begin
      if Buffer = null or else Buffer'Length < W * H then
         if Buffer /= null then
            Free (Buffer);
         end if;
         Buffer := new Pixel_Array (0 .. Natural'Max (W * H, 1) - 1);
      end if;
      Logical_W := Width;
      Logical_H := Height;
      Physical_W := W;
      Physical_H := H;
      Num := N;
      Den := D;
      Clear_Dirty;
      Invalidate ((0, 0, Width, Height));
   end Resize;

   function Ready return Boolean is (Buffer /= null and then Physical_W > 0 and then Physical_H > 0);

   function Canvas return CuBit.UI.Canvas is
     ((addr => (if Buffer = null then System.Null_Address else Buffer.all'Address),
       width => Logical_W, height => Logical_H, pitch => Physical_W * Bytes_Per_Pixel,
       densityNumerator => Num, densityDenominator => Den, others => <>));

   function Canvas (Clip : Rect) return CuBit.UI.Canvas is (CuBit.UI.With_Clip (Canvas, Clip));

   procedure Invalidate (R : Rect) is
      Clipped : constant Rect := CuBit.UI.Clamp_Rect (Canvas, R);
   begin
      if CuBit.UI.Is_Empty (Clipped) then
         return;
      end if;
      --  Already covered: nothing to add.
      for I in 1 .. Used loop
         if Union (Dirty_Rects (I), Clipped) = Dirty_Rects (I) then
            return;
         end if;
      end loop;
      if Used < Maximum_Dirty then
         Used := Used + 1;
         Dirty_Rects (Used) := Clipped;
         return;
      end if;
      --  Full: merge into the rectangle it grows least.
      declare
         Best : Positive := 1;
         Best_Growth : Natural := Natural'Last;
      begin
         for I in 1 .. Used loop
            declare
               Growth : constant Natural := Area (Union (Dirty_Rects (I), Clipped)) - Area (Dirty_Rects (I));
            begin
               if Growth < Best_Growth then
                  Best := I;
                  Best_Growth := Growth;
               end if;
            end;
         end loop;
         Dirty_Rects (Best) := Union (Dirty_Rects (Best), Clipped);
      end;
   end Invalidate;

   function Dirty_Count return Natural is (Used);
   function Dirty (I : Positive) return Rect is (Dirty_Rects (I));

   procedure Clear_Dirty is
   begin
      Used := 0;
   end Clear_Dirty;

   function Dirty_Bounds return Rect is
      B : Rect := (others => 0);
   begin
      for I in 1 .. Used loop
         B := Union (B, Dirty_Rects (I));
      end loop;
      return B;
   end Dirty_Bounds;

   --  R's physical pixels: [X0, X1) x [Y0, Y1), clamped to both surfaces.
   procedure Physical (R : Rect; Limit_W, Limit_H : Natural; X0, X1, Y0, Y1 : out Natural);
   procedure Physical (R : Rect; Limit_W, Limit_H : Natural; X0, X1, Y0, Y1 : out Natural) is
      Right : constant Natural := Natural'Min (R.x + R.w, Logical_W);
      Bottom : constant Natural := Natural'Min (R.y + R.h, Logical_H);
   begin
      X0 := Natural'Min (G.Edge (Natural'Min (R.x, Logical_W), Num, Den), Limit_W);
      Y0 := Natural'Min (G.Edge (Natural'Min (R.y, Logical_H), Num, Den), Limit_H);
      X1 := Natural'Min (G.Edge (Right, Num, Den), Limit_W);
      Y1 := Natural'Min (G.Edge (Bottom, Num, Den), Limit_H);
   end Physical;

   procedure Copy_Out (Target : CuBit.UI.Canvas; R : Rect) is
      use type System.Address;
      X0, X1, Y0, Y1 : Natural;
      Target_Pitch : constant Natural := Target.pitch / Bytes_Per_Pixel;
      Target_Rows : constant Natural :=
        G.Edge (Natural'Min (Target.height, G.Logical_Edge'Last), Target.densityNumerator, Target.densityDenominator);
   begin
      if not Ready or else Target.addr = System.Null_Address or else Target.densityNumerator /= Num
        or else Target.densityDenominator /= Den
      then
         return;
      end if;
      Physical (R, Natural'Min (Physical_W, Target_Pitch), Natural'Min (Physical_H, Target_Rows), X0, X1, Y0, Y1);
      if X1 <= X0 or else Y1 <= Y0 then
         return;
      end if;
      declare
         Length : constant Natural := Target_Pitch * Target_Rows;
         Pixels : Pixel_Array (0 .. Length - 1) with Import, Address => Target.addr;
      begin
         for Y in Y0 .. Y1 - 1 loop
            Pixels (Y * Target_Pitch + X0 .. Y * Target_Pitch + X1 - 1) :=
              Buffer (Y * Physical_W + X0 .. Y * Physical_W + X1 - 1);
         end loop;
      end;
   end Copy_Out;

   procedure Shift (R : Rect; Dx : Integer) is
      X0, X1, Y0, Y1 : Natural;
   begin
      if Dx = 0 or else not Ready then
         return;
      end if;
      if (abs Dx * Num) mod Den /= 0 or else abs Dx >= R.w then
         Invalidate (R);
         return;
      end if;
      Physical (R, Physical_W, Physical_H, X0, X1, Y0, Y1);
      declare
         Move : constant Natural := abs Dx * Num / Den;
      begin
         if X1 - X0 <= Move then
            Invalidate (R);
            return;
         end if;
         for Y in Y0 .. Y1 - 1 loop
            declare
               Row : constant Natural := Y * Physical_W;
            begin
               --  Slices overlap: Ada's slice assignment copies as if
               --  through a temporary (memmove).
               if Dx < 0 then
                  Buffer (Row + X0 .. Row + X1 - 1 - Move) := Buffer (Row + X0 + Move .. Row + X1 - 1);
               else
                  Buffer (Row + X0 + Move .. Row + X1 - 1) := Buffer (Row + X0 .. Row + X1 - 1 - Move);
               end if;
            end;
         end loop;
      end;
   end Shift;
   procedure Move_Blocks (Moves : Block_Moves; Done : out Boolean) is
      type Box is record
         X0, X1, Y0, Y1 : Natural := 0;
      end record;
      type Box_List is array (Moves'Range) of Box;
      type Flag_List is array (Moves'Range) of Boolean;
      Sources, Targets : Box_List;
      Pending : Flag_List := [others => True];
      --  A block whose source went to Scratch (it closed a cycle), and where.
      Saved : Flag_List := [others => False];
      Saved_At : array (Moves'Range) of Natural := [others => 0];
      Scratch_Used : Natural := 0;
      Left : Natural := Moves'Length;

      function Overlap (A, B : Box) return Boolean is
        (A.X0 < B.X1 and then B.X0 < A.X1 and then A.Y0 < B.Y1 and then B.Y0 < A.Y1);

      --  Rows of the block, in the order that keeps an overlapping move
      --  of a block onto itself intact (a slice assignment is a memmove).
      procedure Copy_Rows (From, To : Box) is
         W : constant Natural := From.X1 - From.X0;
         H : constant Natural := From.Y1 - From.Y0;
      begin
         if To.Y0 > From.Y0 then
            for R in reverse 0 .. H - 1 loop
               Buffer ((To.Y0 + R) * Physical_W + To.X0 .. (To.Y0 + R) * Physical_W + To.X0 + W - 1) :=
                 Buffer ((From.Y0 + R) * Physical_W + From.X0 .. (From.Y0 + R) * Physical_W + From.X0 + W - 1);
            end loop;
         else
            for R in 0 .. H - 1 loop
               Buffer ((To.Y0 + R) * Physical_W + To.X0 .. (To.Y0 + R) * Physical_W + To.X0 + W - 1) :=
                 Buffer ((From.Y0 + R) * Physical_W + From.X0 .. (From.Y0 + R) * Physical_W + From.X0 + W - 1);
            end loop;
         end if;
      end Copy_Rows;
   begin
      Done := False;
      if not Ready then
         return;
      end if;
      for I in Moves'Range loop
         declare
            S : Box renames Sources (I);
            T : Box renames Targets (I);
         begin
            Physical (Moves (I).From, Physical_W, Physical_H, S.X0, S.X1, S.Y0, S.Y1);
            Physical (Moves (I).To, Physical_W, Physical_H, T.X0, T.X1, T.Y0, T.Y1);
            if Moves (I).From.w /= Moves (I).To.w or else Moves (I).From.h /= Moves (I).To.h
              or else S.X1 - S.X0 /= T.X1 - T.X0 or else S.Y1 - S.Y0 /= T.Y1 - T.Y0
            then
               return;
            end if;
         end;
      end loop;
      --  A block goes once no other pending block still has to be read
      --  from where it lands; a cycle (blocks trading places) puts one
      --  source aside in Scratch first. Most moves never touch Scratch.
      while Left > 0 loop
         declare
            Moved_One : Boolean := False;
         begin
            for I in Moves'Range loop
               if Pending (I) then
                  declare
                     Blocked : Boolean := False;
                  begin
                     for J in Moves'Range loop
                        if J /= I and then Pending (J) and then not Saved (J)
                          and then Overlap (Targets (I), Sources (J))
                        then
                           Blocked := True;
                        end if;
                     end loop;
                     if not Blocked then
                        if Saved (I) then
                           declare
                              W : constant Natural := Targets (I).X1 - Targets (I).X0;
                              At_Pixel : Natural := Saved_At (I);
                           begin
                              for Y in Targets (I).Y0 .. Targets (I).Y1 - 1 loop
                                 Buffer (Y * Physical_W + Targets (I).X0 .. Y * Physical_W + Targets (I).X0 + W - 1) :=
                                   Scratch (At_Pixel .. At_Pixel + W - 1);
                                 At_Pixel := At_Pixel + W;
                              end loop;
                           end;
                        else
                           Copy_Rows (Sources (I), Targets (I));
                        end if;
                        Pending (I) := False;
                        Left := Left - 1;
                        Moved_One := True;
                     end if;
                  end;
               end if;
            end loop;
            if not Moved_One then
               --  A cycle: the first pending source that is still in place
               --  goes aside.
               for I in Moves'Range loop
                  if Pending (I) and then not Saved (I) then
                     declare
                        S : constant Box := Sources (I);
                        W : constant Natural := S.X1 - S.X0;
                        Need : constant Natural := Scratch_Used + W * (S.Y1 - S.Y0);
                     begin
                        if Scratch = null or else Scratch'Length < Need then
                           declare
                              Grown : constant Pixel_Access :=
                                new Pixel_Array (0 .. Natural'Max (Need, 2 * (if Scratch = null then 0
                                                                               else Scratch'Length)) - 1);
                           begin
                              if Scratch /= null then
                                 Grown (0 .. Scratch_Used - 1) := Scratch (0 .. Scratch_Used - 1);
                                 Free (Scratch);
                              end if;
                              Scratch := Grown;
                           end;
                        end if;
                        Saved_At (I) := Scratch_Used;
                        for Y in S.Y0 .. S.Y1 - 1 loop
                           Scratch (Scratch_Used .. Scratch_Used + W - 1) :=
                             Buffer (Y * Physical_W + S.X0 .. Y * Physical_W + S.X0 + W - 1);
                           Scratch_Used := Scratch_Used + W;
                        end loop;
                        Saved (I) := True;
                     end;
                     exit;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      Done := True;
   end Move_Blocks;
end Dash_Layers;
