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
end Dash_Layers;
