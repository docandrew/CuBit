------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Client_Canvas_Geometry;

package body CuBit.UI.Graphs is

   function Overlaps (A, B : Box; Margin : Natural) return Boolean is
     (A.X < B.X + B.W + Margin and then B.X < A.X + A.W + Margin and then
      A.Y < B.Y + B.H + Margin and then B.Y < A.Y + A.H + Margin);

   function Node_Size (Widest_Chars : Natural; Rows : Row_Count; More_Rows : Boolean) return Box is
      W : constant Natural := Natural'Max (MIN_NODE_W, Natural'Min
        (MAX_NODE_W, Natural'Min (Widest_Chars, MAX_NODE_W) * CHAR_WIDTH + 2 * NODE_PAD + PORT_WIDTH));
   begin
      return (X => 0, Y => 0, W => W,
              H => HEADER_HEIGHT + (Rows + (if More_Rows then 1 else 0)) * ROW_HEIGHT + NODE_PAD / 2);
   end Node_Size;

   function Find_Place (L : Layout; Key : Node_Key) return Natural is
   begin
      for P in 1 .. L.Total loop
         if L.Places (P).Key = Key then
            return P;
         end if;
      end loop;
      return 0;
   end Find_Place;

   --  Move Candidate down until it overlaps no present place.
   procedure Free_Space (L : Layout; Candidate : in out Box) is
      Moved : Boolean := True;
      Guard : Natural := 0;
   begin
      while Moved and then Guard <= MAX_PLACES loop
         Moved := False;
         Guard := Guard + 1;
         for P in 1 .. L.Total loop
            if L.Places (P).Present and then Overlaps (Candidate, L.Places (P).Where, GAP_Y / 2) then
               Candidate.Y := World'Min (World'Last - Candidate.H,
                                         L.Places (P).Where.Y + L.Places (P).Where.H + GAP_Y);
               Moved := True;
            end if;
         end loop;
      end loop;
   end Free_Space;

   procedure Update
     (L : in out Layout; Keys : Key_Array; Sizes : Box_Array; Pinned : Pin_Array;
      Edges : Edge_Array; Wrap_Width : Natural := 0)
   is
      Count : constant Natural := Natural'Min (Keys'Length, MAX_NODES);
      Done : array (Node_Index) of Boolean := [others => False];
      Progress : Boolean := True;

      function Valid (E : Edge) return Boolean is (E.From <= Count and then E.To <= Count);

      procedure Settle (N : Node_Index; Candidate : Box; Parent : Box := (others => <>)) is
         Spot : Box := Candidate;
      begin
         --  Past the wrap: below the parent, left edges aligned.
         if Wrap_Width > 0 and then Parent.W > 0 and then Spot.X + Spot.W > Wrap_Width then
            Spot.X := Parent.X;
            Spot.Y := World'Min (World'Last - Spot.H, Parent.Y + Parent.H + GAP_Y);
         end if;
         Free_Space (L, Spot);
         if L.Total < MAX_PLACES then
            L.Total := L.Total + 1;
            L.Places (L.Total) := (Key => Keys (N), Where => Spot, Present => True, Pinned => False);
            L.Of_Node (N) := L.Total;
            L.Placed_New := L.Placed_New + 1;
         end if;
         Done (N) := True;
      end Settle;

      procedure Place_Children is
      begin
         Progress := True;
         while Progress loop
            Progress := False;
            for E of Edges loop
               if Valid (E) and then Done (E.From) and then not Done (E.To) and then L.Of_Node (E.From) /= 0 then
                  declare
                     Parent : constant Box := L.Places (L.Of_Node (E.From)).Where;
                     Size   : constant Box := Sizes (E.To);
                  begin
                     --  Beside the parent, tops aligned: a list runs in a row.
                     Settle (E.To, (X => World'Min (World'Last - Size.W, Parent.X + Parent.W + GAP_X),
                                    Y => World'Max (World'First, World'Min (World'Last - Size.H, Parent.Y)),
                                    W => Size.W, H => Size.H), Parent);
                     Progress := True;
                  end;
               end if;
            end loop;
         end loop;
      end Place_Children;
   begin
      L.Placed_New := 0;
      L.Of_Node := [others => 0];
      for P in 1 .. L.Total loop
         L.Places (P).Present := False;
      end loop;
      --  Pinned nodes where they are pinned; known nodes where they were.
      for N in 1 .. Count loop
         declare
            P : Natural := Find_Place (L, Keys (N));
         begin
            if Pinned (N) then
               if P = 0 and then L.Total < MAX_PLACES then
                  L.Total := L.Total + 1;
                  P := L.Total;
                  L.Places (P).Key := Keys (N);
               end if;
               if P /= 0 then
                  L.Places (P).Where := Sizes (N);
                  L.Places (P).Pinned := True;
               end if;
            end if;
            if P /= 0 then
               L.Places (P).Where.W := Sizes (N).W;
               L.Places (P).Where.H := Sizes (N).H;
               L.Places (P).Present := True;
               L.Of_Node (N) := P;
               Done (N) := True;
            end if;
         end;
      end loop;
      Place_Children;
      --  Roots and anything unreached: the first column, stacked.
      for N in 1 .. Count loop
         if not Done (N) then
            Settle (N, (X => 0, Y => 0, W => Sizes (N).W, H => Sizes (N).H));
            Place_Children;
         end if;
      end loop;
   end Update;

   procedure Relayout
     (L : in out Layout; Keys : Key_Array; Sizes : Box_Array; Pinned : Pin_Array;
      Edges : Edge_Array; Wrap_Width : Natural := 0)
   is
      Kept : Place_Count := 0;
   begin
      for P in 1 .. L.Total loop
         if L.Places (P).Pinned then
            Kept := Kept + 1;
            L.Places (Kept) := L.Places (P);
         end if;
      end loop;
      L.Total := Kept;
      Update (L, Keys, Sizes, Pinned, Edges, Wrap_Width);
   end Relayout;

   function Box_Of (L : Layout; N : Node_Index) return Box is
     (if L.Of_Node (N) = 0 then (others => <>) else L.Places (L.Of_Node (N)).Where);

   function Bounds (L : Layout) return Box is
      Result : Box;
      First : Boolean := True;
      Right, Bottom : Integer;
   begin
      for P in 1 .. L.Total loop
         if L.Places (P).Present then
            if First then
               Result := L.Places (P).Where;
               First := False;
            else
               Right := Integer'Max (Result.X + Result.W, L.Places (P).Where.X + L.Places (P).Where.W);
               Bottom := Integer'Max (Result.Y + Result.H, L.Places (P).Where.Y + L.Places (P).Where.H);
               Result.X := Integer'Min (Result.X, L.Places (P).Where.X);
               Result.Y := Integer'Min (Result.Y, L.Places (P).Where.Y);
               Result.W := Right - Result.X;
               Result.H := Bottom - Result.Y;
            end if;
         end if;
      end loop;
      return Result;
   end Bounds;

   procedure Pin (L : in out Layout; N : Node_Index; X, Y : World) is
   begin
      if L.Of_Node (N) /= 0 then
         L.Places (L.Of_Node (N)).Where.X := X;
         L.Places (L.Of_Node (N)).Where.Y := Y;
         L.Places (L.Of_Node (N)).Pinned := True;
      end if;
   end Pin;

   function GCD (A, B : Positive) return Positive is
     (if A mod B = 0 then B else GCD (B, A mod B));

   procedure Zoomed (C : Canvas; Area : Rect; Step : Zoom_Step; Result : out Canvas; Zone : out Rect;
                     OK : out Boolean) is
      CN : constant Positive := Positive (C.densityNumerator);
      CD : constant Positive := Positive (C.densityDenominator);
      N0 : constant Positive := CN * ZOOM_N (Step);
      D0 : constant Positive := CD * ZOOM_D (Step);
      G  : constant Positive := GCD (N0, D0);
      N  : constant Positive := N0 / G;
      D  : constant Positive := D0 / G;
      function Phys (X : Natural) return Natural is ((X * CN + CD - 1) / CD);
      function Up (Px : Natural) return Natural is ((Px * D + N - 1) / N);
      function Down (Px : Natural) return Natural is (Px * D / N);
      function Map (R : Rect) return Rect is
        (Up (Phys (R.x)), Up (Phys (R.y)),
         Natural'Max (Down (Phys (R.x + R.w)), Up (Phys (R.x))) - Up (Phys (R.x)),
         Natural'Max (Down (Phys (R.y + R.h)), Up (Phys (R.y))) - Up (Phys (R.y)));
      Clip : Rect;
   begin
      Result := C;
      Zone := Area;
      OK := N <= Positive (Client_Canvas_Geometry.Component'Last) and then D <= Positive (Client_Canvas_Geometry.Component'Last);
      if not OK then
         return;
      end if;
      Result.densityNumerator := N;
      Result.densityDenominator := D;
      Result.width := Down (Phys (C.width));
      Result.height := Down (Phys (C.height));
      Zone := Map (Area);
      Clip := Zone;
      if C.clipEnabled then
         declare
            K : constant Rect := Map (C.clip);
            X0 : constant Natural := Natural'Max (Clip.x, K.x);
            Y0 : constant Natural := Natural'Max (Clip.y, K.y);
            X1 : constant Natural := Natural'Min (Clip.x + Clip.w, K.x + K.w);
            Y1 : constant Natural := Natural'Min (Clip.y + Clip.h, K.y + K.h);
         begin
            Clip := (X0, Y0, (if X1 > X0 then X1 - X0 else 0), (if Y1 > Y0 then Y1 - Y0 else 0));
         end;
      end if;
      Result.clipEnabled := True;
      Result.clip := Clip;
      Result.inputClipEnabled := False;
   end Zoomed;
end CuBit.UI.Graphs;
