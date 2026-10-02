with Client_Canvas_Geometry;
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Combo_Boxes; use CuBit.UI.Combo_Boxes;
with CuBit.UI.Controls;
procedure Combo_Tests is
   package Controls renames CuBit.UI.Controls;
   use type Controls.Pointer_Action;
   use type Color;
   A : aliased constant String := "Alpha";
   B : aliased constant String := "Beta";
   G : aliased constant String := "Gamma";
   D : Model;
   S : Combo_State;
   Map : Controls.Control_Map;
   type Surface is array (0 .. 319, 0 .. 399) of Color;
   Pixels : aliased Surface := [others => [others => 0]];
   Reference : Surface;
   C : Canvas := (addr => Pixels'Address, width => 400, height => 320, pitch => 1600, others => <>);
   R : Rect := (20, 20, 180, 32);
   Changed, Handled, Dispatched, Visual : Boolean;
   procedure Paint is
   begin
      Controls.Clear (Map);
      Draw (C, Map, S, D, 1, R, CuBit_Alloy, Focused => True);
      pragma Assert (Controls.Is_Valid (Map));
   end Paint;
   procedure Press (K : Key; Letter : Character := ' ') is
   begin Handle_Key (S, D, K, Changed, Handled, Letter); end Press;
   procedure Pointer (Target : Natural; Action : Controls.Pointer_Action) is
      Hit_Box : constant Rect := Controls.Bounds (Map, Target);
   begin
      Controls.Dispatch_Pointer (Map, Target, Action,
        Hit_Box.x + Hit_Box.w / 2, Hit_Box.y + Hit_Box.h / 2, Visual, Dispatched);
      Handle_Pointer (S, D, Map, 1, Target, Action, Changed, Handled);
   end Pointer;
begin
   D.Count := 4;
   D.Choices (1) := (A'Unchecked_Access, True);
   D.Choices (2) := (B'Unchecked_Access, False);
   D.Choices (3) := (B'Unchecked_Access, True);
   D.Choices (4) := (G'Unchecked_Access, True);
   Set_Selection (S, D, 1);
   Press (Down); pragma Assert (Changed and Selection (S) = 3 and not Is_Open (S));
   Press (Toggle); Press (Down);
   pragma Assert (Selection (S) = 3 and Is_Open (S) and not Changed);
   Press (Cancel); pragma Assert (Selection (S) = 3 and not Is_Open (S));
   Press (Toggle); Press (End_Key); Press (Commit);
   pragma Assert (Changed and Selection (S) = 4 and not Is_Open (S));
   Press (Type_Character, 'a'); pragma Assert (Changed and Selection (S) = 1);
   Press (Toggle); Press (Down); Press (Tab_Key);
   pragma Assert (not Handled and not Is_Open (S) and Selection (S) = 1);
   for Cycle in 1 .. 100 loop
      Set_Selection (S, D, 1); Paint;
      Pointer (1, Controls.Pointer_Press); pragma Assert (Is_Open (S)); Paint;
      Pointer (1, Controls.Pointer_Release); pragma Assert (Is_Open (S));
      Pointer (Choice_ID (1, 3), Controls.Pointer_Press); Paint;
      Pointer (Choice_ID (1, 3), Controls.Pointer_Release);
      pragma Assert (Changed and Selection (S) = 3 and not Is_Open (S));
      Paint; Pointer (1, Controls.Pointer_Press); Paint;
      -- Drag from the field into the list commits once, without a row press.
      Pointer (Choice_ID (1, 4), Controls.Pointer_Move); Paint;
      Pointer (Choice_ID (1, 4), Controls.Pointer_Release);
      pragma Assert (Changed and Selection (S) = 4 and not Is_Open (S));
      Press (Toggle); Paint;
      Handle_Pointer (S, D, Map, 1, 99, Controls.Pointer_Press, Changed, Handled);
      pragma Assert (Handled and not Changed and not Is_Open (S));
      Press (Toggle); Press (Down); Pointer (0, Controls.Pointer_Cancel);
      pragma Assert (not Is_Open (S) and Selection (S) = 4);
   end loop;
   D.Count := Max_Choices;
   for I in 5 .. Max_Choices loop D.Choices (I) := (A'Unchecked_Access, True); end loop;
   Press (Toggle); Press (End_Key); Paint;
   pragma Assert (Controls.Hit (Map, 30, 246) = Choice_ID (1, 64));
   Handle_Wheel (S, D, Integer'First, Handled); pragma Assert (Handled);
   Press (Commit); pragma Assert (Selection (S) = 1);
   R := (20, 280, 180, 32); Press (Toggle); Press (End_Key); Paint;
   pragma Assert (Controls.Hit (Map, 30, 270) = Choice_ID (1, 64));
   Handle_Key (S, D, Down, Changed, Handled, Enabled => False);
   pragma Assert (not Handled and not Is_Open (S));
   D.Count := 0; Press (Toggle); pragma Assert (not Handled and not Is_Open (S));
   D.Count := 4;
   -- Tiny fields stay inside their bounds; no popup is open in this sweep.
   for W in 0 .. 28 loop
      for H in 0 .. 20 loop
         Pixels := [others => [others => 16#E155AA#]];
         R := (10, 10, W, H); Paint;
         for Y in 0 .. 50 loop
            for X in 0 .. 50 loop
               if not Point_In_Rect (X, Y, R) then
                  pragma Assert (Pixels (Y, X) = 16#E155AA#);
               end if;
            end loop;
         end loop;
      end loop;
   end loop;
   C.width := 200; C.height := 160;
   R := (12, 14, 150, 30);
   Set_Selection (S, D, 1); Press (Toggle);
   for N in 4 .. 8 loop
      C.densityNumerator := N; C.densityDenominator := 4;
      for Dark in Boolean loop
         declare
            T : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
            Clip : constant Rect := (22, 30, 80, 75);
            function Scale (X : Natural) return Natural is
              (Client_Canvas_Geometry.Relative (0, X, N, 4));
         begin
            Pixels := [others => [others => 16#E155AA#]]; Controls.Clear (Map);
            Draw (C, Map, S, D, 1, R, T); Reference := Pixels;
            Pixels := [others => [others => 16#E155AA#]]; Controls.Clear (Map);
            Draw (With_Clip (C, Clip), Map, S, D, 1, R, T);
            for Y in Pixels'Range (1) loop
               for X in Pixels'Range (2) loop
                  pragma Assert (Pixels (Y, X) =
                    (if X >= Scale (Clip.x) and X < Scale (Clip.x + Clip.w) and
                      Y >= Scale (Clip.y) and Y < Scale (Clip.y + Clip.h)
                     then Reference (Y, X) else 16#E155AA#));
               end loop;
            end loop;
         end;
      end loop;
   end loop;
   declare
      V : constant Vertical_Scrollbar_Layout := Layout_Vertical_Scrollbar ((10, 10, 16, 100), 0, 99, 20, 10);
      H : constant Horizontal_Scrollbar_Layout := Layout_Horizontal_Scrollbar ((10, 10, 100, 16), 0, 99, 20, 10);
   begin
      pragma Assert (V.thumb.x = 10 and V.thumb.w = 16);
      pragma Assert (H.thumb.y = 10 and H.thumb.h = 16);
   end;
   Put_Line ("PASS combo: keyboard commit/cancel, disabled/empty, 100 retained pointer cycles, overflow/wheel/flip, 609 tiny fields, 10 popup palette/density clips; full-width scrollbar thumbs");
end Combo_Tests;
