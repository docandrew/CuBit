------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Graphs.Views is
   use CuBit.UI.Instrument;

   LINE_H     : constant := CuBit.UI_Metrics.Line_Height;
   EDGE_W     : constant := 2;
   MARK_EDGE  : constant := 3;
   GRID_STEP  : constant := 32;
   GRID_DOT   : constant := 2;
   STUB       : constant := 16;
   PORT_BOX   : constant := 8;
   PAN_STEP   : constant := 48;
   DRAG_SLOP  : constant := 3;
   FIT_MARGIN : constant := 24;
   --  Room above the nodes for back edges routed over them.
   BACK_EDGE_MARGIN : constant := 3 * FIT_MARGIN;

   --  Keys (PS/2 set-1 make codes, as CuBit.UI.Input delivers them).
   KEY_UP    : constant := 16#48#;
   KEY_DOWN  : constant := 16#50#;
   KEY_LEFT  : constant := 16#4B#;
   KEY_RIGHT : constant := 16#4D#;
   KEY_MINUS : constant := 16#0C#;
   KEY_EQUAL : constant := 16#0D#;
   KEY_0     : constant := 16#0B#;
   KEY_R     : constant := 16#13#;

   ---------------------------------------------------------------------------
   function Widest (M : Model; N : Node_Index) return Natural is
      W : Natural := Title (M, N)'Length + 2 + Detail (M, N)'Length;
   begin
      for R in 1 .. Row_Total (M, N) loop
         W := Natural'Max (W, Row_Name (M, N, R)'Length + 2 + Row_Value (M, N, R)'Length);
      end loop;
      return W;
   end Widest;

   function Wrap_Width (Area : Rect) return Natural is
     (Area.w * ZOOM_D (FIT_ZOOM_MIN) / ZOOM_N (FIT_ZOOM_MIN));

   procedure Lay_Out (V : in out View; M : Model; Area : Rect; Afresh : Boolean) is
      Count : constant Node_Count := Node_Total (M);
      Keys  : Key_Array (1 .. Natural'Max (Count, 1));
      Sizes : Box_Array (1 .. Natural'Max (Count, 1));
      Pins  : Pin_Array (1 .. Natural'Max (Count, 1)) := [others => False];
      Edges : Edge_Array (1 .. Natural'Max (Edge_Total (M), 1));
   begin
      if Count = 0 then
         return;
      end if;
      for N in 1 .. Count loop
         Keys (N) := Key_Of (M, N);
         Sizes (N) := Node_Size (Widest (M, N), Row_Total (M, N), More_Rows (M, N) > 0);
         Pins (N) := Is_Pinned (M, N);
         if Pins (N) then
            --  A pinned node stays where the view last had it.
            declare
               Was : constant Box := Box_Of (V.Places, N);
            begin
               Sizes (N).X := Was.X;
               Sizes (N).Y := Was.Y;
            end;
         end if;
      end loop;
      for E in 1 .. Edge_Total (M) loop
         Edges (E) := Edge_At (M, E);
      end loop;
      if Afresh then
         Relayout (V.Places, Keys (1 .. Count), Sizes (1 .. Count), Pins (1 .. Count),
                   Edges (1 .. Edge_Total (M)), Wrap_Width (Area));
      else
         Update (V.Places, Keys (1 .. Count), Sizes (1 .. Count), Pins (1 .. Count),
                 Edges (1 .. Edge_Total (M)), Wrap_Width (Area));
      end if;
   end Lay_Out;

   procedure Update (V : in out View; M : Model; Area : Rect) is
   begin
      Lay_Out (V, M, Area, Afresh => False);
   end Update;

   procedure Relayout (V : in out View; M : Model; Area : Rect) is
   begin
      Lay_Out (V, M, Area, Afresh => True);
   end Relayout;

   procedure Fit (V : in out View; M : Model; Area : Rect) is
      B : constant Box := Bounds (V.Places);
      Has_Back : Boolean := False;
      Top : Natural;
   begin
      for E in 1 .. Edge_Total (M) loop
         Has_Back := Has_Back or else Edge_At (M, E).Back;
      end loop;
      Top := (if Has_Back then BACK_EDGE_MARGIN else FIT_MARGIN);
      V.Zoom := UNIT_ZOOM;
      while V.Zoom > FIT_ZOOM_MIN and then
        (B.W * ZOOM_N (V.Zoom) / ZOOM_D (V.Zoom) > Integer (Area.w) - 2 * FIT_MARGIN or else
         B.H * ZOOM_N (V.Zoom) / ZOOM_D (V.Zoom) > Integer (Area.h) - FIT_MARGIN - Top)
      loop
         V.Zoom := V.Zoom - 1;
      end loop;
      V.Pan_X := B.X - FIT_MARGIN * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom);
      V.Pan_Y := B.Y - Top * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom);
      V.Fitted := True;
   end Fit;

   ---------------------------------------------------------------------------
   procedure H_Line (C : Canvas; X0, X1, Y : Integer; Col : Color) is
   begin
      Fill (C, Integer'Min (X0, X1), Y - EDGE_W / 2, abs (X1 - X0) + EDGE_W / 2, EDGE_W, Col);
   end H_Line;

   procedure V_Line (C : Canvas; X, Y0, Y1 : Integer; Col : Color) is
   begin
      Fill (C, X - EDGE_W / 2, Integer'Min (Y0, Y1) - EDGE_W / 2, EDGE_W, abs (Y1 - Y0) + EDGE_W, Col);
   end V_Line;

   procedure Arrow (C : Canvas; Tip_X, Y : Integer; Col : Color) is
      HALF : constant := 5;
   begin
      for I in 0 .. HALF loop
         Fill (C, Tip_X - 2 * HALF + 2 * I, Y - (HALF - I), 2, 2 * (HALF - I) + 1, Col);
      end loop;
   end Arrow;

   --  A null link: a ground mark (three shrinking bars).
   procedure Ground (C : Canvas; X, Y : Integer; Col : Color) is
   begin
      Fill (C, X, Y - 5, 2, 6, Col);
      Fill (C, X - 6, Y + 1, 14, 2, Col);
      Fill (C, X - 4, Y + 4, 10, 2, Col);
      Fill (C, X - 2, Y + 7, 6, 2, Col);
   end Ground;

   procedure Draw
     (C : Canvas; Area : Rect; M : Model; V : View; Colors : CuBit.UI.Instrument.Palette)
   is
      P : CuBit.UI.Instrument.Palette renames Colors;
      Z : Canvas;
      Zone : Rect;
      OK : Boolean;
      Summary : constant Boolean := V.Zoom <= SUMMARY_ZOOM;
      function SX (WX : Integer) return Integer is (Integer (Zone.x) + WX - V.Pan_X);
      function SY (WY : Integer) return Integer is (Integer (Zone.y) + WY - V.Pan_Y);
   begin
      Zoomed (C, Area, V.Zoom, Z, Zone, OK);
      if not OK then
         Zoomed (C, Area, UNIT_ZOOM, Z, Zone, OK);
      end if;
      --  The grid: one dot per step, fixed to the world so it pans with it.
      declare
         WX : Integer := V.Pan_X - (V.Pan_X mod GRID_STEP);
         WY : Integer;
      begin
         while SX (WX) < Integer (Zone.x + Zone.w) loop
            WY := V.Pan_Y - (V.Pan_Y mod GRID_STEP);
            while SY (WY) < Integer (Zone.y + Zone.h) loop
               Fill (Z, SX (WX), SY (WY), GRID_DOT, GRID_DOT, P.Grid);
               WY := WY + GRID_STEP;
            end loop;
            WX := WX + GRID_STEP;
         end loop;
      end;
      --  Edges under the nodes.
      for E in 1 .. Edge_Total (M) loop
         declare
            Ed : constant Edge := Edge_At (M, E);
         begin
            if Ed.From <= Node_Total (M) and then Ed.To <= Node_Total (M)
              and then Ed.From_Row <= Row_Total (M, Ed.From)
            then
               declare
                  A : constant Box := Box_Of (V.Places, Ed.From);
                  T : constant Box := Box_Of (V.Places, Ed.To);
                  X0 : constant Integer := SX (A.X + A.W);
                  Y0 : constant Integer := SY (A.Y + Row_Center (Ed.From_Row));
                  X1 : constant Integer := SX (T.X);
                  Y1 : constant Integer := SY (T.Y + Row_Center (Ed.To_Row));
                  Hot : constant Boolean := Row_Marked (M, Ed.From, Ed.From_Row);
                  Col : constant Color := (if Hot then P.Changed elsif Ed.Back then P.Back_Edge else P.Edge);
               begin
                  if X1 >= X0 + 2 * STUB then
                     declare
                        Mid : constant Integer := X0 + (X1 - X0) / 2;
                     begin
                        H_Line (Z, X0, Mid, Y0, Col);
                        V_Line (Z, Mid, Y0, Y1, Col);
                        H_Line (Z, Mid, X1, Y1, Col);
                     end;
                  else
                     --  A node below its parent is entered through the gap
                     --  above it; a back edge goes round, above both.
                     declare
                        Above : constant Integer :=
                          (if T.Y >= A.Y + A.H then SY (T.Y) - GAP_Y / 2
                           else SY (Integer'Min (A.Y, T.Y)) - STUB);
                        Out_X : constant Integer := X0 + STUB;
                        In_X  : constant Integer := X1 - STUB;
                     begin
                        H_Line (Z, X0, Out_X, Y0, Col);
                        V_Line (Z, Out_X, Y0, Above, Col);
                        H_Line (Z, Out_X, In_X, Above, Col);
                        V_Line (Z, In_X, Above, Y1, Col);
                        H_Line (Z, In_X, X1, Y1, Col);
                        if Ed.Back and then not Summary then
                           Mono (Z, Integer'Min (Out_X, In_X) + CHAR_WIDTH, Above - LINE_H - 1, "cycle", P.Back_Edge);
                        end if;
                     end;
                  end if;
                  Arrow (Z, X1, Y1, Col);
               end;
            end if;
         end;
      end loop;
      --  Nodes.
      for N in 1 .. Node_Total (M) loop
         declare
            Bx : constant Box := Box_Of (V.Places, N);
            X : constant Integer := SX (Bx.X);
            Y : constant Integer := SY (Bx.Y);
            Visible : constant Boolean :=
              X < Integer (Zone.x + Zone.w) and then X + Bx.W > Integer (Zone.x) and then
              Y < Integer (Zone.y + Zone.h) and then Y + Bx.H > Integer (Zone.y);
            Text_W : constant Integer := Bx.W - 2 * NODE_PAD - PORT_WIDTH;
         begin
            if Visible and then Bx.W > 0 then
               Fill (Z, X, Y, Bx.W, Bx.H, P.Node_Fill);
               Fill (Z, X, Y, Bx.W, HEADER_HEIGHT,
                     (case Style (M, N) is
                        when Array_Style => P.Array_Head, when Pointer_Style => P.Pointer_Head,
                        when Enum_Style => P.Enum_Head, when Record_Style | Plain_Style => P.Record_Head));
               if not Summary then
                  declare
                     Head : constant String := Title (M, N);
                     Right : constant String := Detail (M, N);
                     Room : constant Integer := Text_W - Code_Text_Width (Right) - CHAR_WIDTH;
                     Both : constant Boolean := Right'Length > 0 and then Room > Code_Text_Width (Head);
                  begin
                     Mono (Z, X + NODE_PAD, Text_Y (Y, HEADER_HEIGHT), Fits (Head, (if Both then Room else Text_W)), P.Text);
                     if Both then
                        Mono (Z, X + Bx.W - NODE_PAD - Code_Text_Width (Right), Text_Y (Y, HEADER_HEIGHT), Right, P.Muted);
                     end if;
                  end;
               end if;
               for R in 1 .. Row_Total (M, N) + (if More_Rows (M, N) > 0 then 1 else 0) loop
                  declare
                     RY : constant Integer := Y + HEADER_HEIGHT + (R - 1) * ROW_HEIGHT;
                     TY : constant Integer := Text_Y (RY, ROW_HEIGHT);
                  begin
                     if R mod 2 = 0 then
                        Fill (Z, X, RY, Bx.W, ROW_HEIGHT, P.Pane_Alt);
                     end if;
                     if R <= Row_Total (M, N) then
                        declare
                           Hot : constant Boolean := Row_Marked (M, N, R);
                           Name : constant String := Row_Name (M, N, R);
                           Val : constant String := Row_Value (M, N, R);
                           Kind : constant Port_State := Port (M, N, R);
                           PY : constant Integer := Y + Row_Center (R);
                           Value_Col : constant Color :=
                             (if Kind = Dangling_Port then P.Danger elsif Hot then P.Changed
                              elsif Kind in Out_Port then P.Edge else P.Text);
                        begin
                           if Hot then
                              Fill (Z, X, RY, Bx.W, ROW_HEIGHT, P.Changed_Fill);
                              Fill (Z, X, RY, MARK_EDGE, ROW_HEIGHT, P.Changed);
                           end if;
                           if Summary then
                              Fill (Z, X + NODE_PAD, PY - 2, Natural'Min (Name'Length * CHAR_WIDTH, Text_W / 3), 4, P.Dim);
                              Fill (Z, X + NODE_PAD + Text_W / 2, PY - 2,
                                    Natural'Min (Val'Length * CHAR_WIDTH, Text_W / 2), 4, Value_Col);
                           else
                              Mono (Z, X + NODE_PAD, TY, Fits (Name, Text_W / 2), P.Muted);
                              Mono (Z, X + NODE_PAD + (Name'Length + 2) * CHAR_WIDTH, TY,
                                    Fits (Val, Text_W - (Name'Length + 2) * CHAR_WIDTH), Value_Col);
                           end if;
                           case Kind is
                              when No_Port => null;
                              when Null_Port =>
                                 Ground (Z, X + Bx.W - PORT_WIDTH / 2 - 1, PY - 2, P.Dim);
                              when Linked_Port =>
                                 Fill (Z, X + Bx.W - PORT_BOX - 2, PY - PORT_BOX / 2, PORT_BOX, PORT_BOX, P.Port);
                              when Open_Port =>
                                 Frame (Z, X + Bx.W - PORT_BOX - 2, PY - PORT_BOX / 2, PORT_BOX, PORT_BOX, 2, P.Port);
                              when Dangling_Port =>
                                 Frame (Z, X + Bx.W - PORT_BOX - 2, PY - PORT_BOX / 2, PORT_BOX, PORT_BOX, 2, P.Danger);
                              when In_Port =>
                                 Fill (Z, X + 2, PY - PORT_BOX / 2, PORT_BOX, PORT_BOX, P.Port);
                           end case;
                        end;
                     elsif not Summary then
                        Mono (Z, X + NODE_PAD, TY, "+" & Dec (Unsigned_64 (More_Rows (M, N))) & " more", P.Dim);
                     end if;
                  end;
               end loop;
               Frame (Z, X, Y, Bx.W, Bx.H, (if V.Selected = N then EDGE_W else 1),
                      (if V.Selected = N then P.Node_Selected elsif Node_Marked (M, N) then P.Changed
                       else P.Node_Edge));
            end if;
         end;
      end loop;
   end Draw;

   ---------------------------------------------------------------------------
   function World_X (V : View; Area : Rect; X : Natural) return Integer is
     (V.Pan_X + (Integer (X) - Integer (Area.x)) * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom));
   function World_Y (V : View; Area : Rect; Y : Natural) return Integer is
     (V.Pan_Y + (Integer (Y) - Integer (Area.y)) * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom));

   procedure Zoom_At (V : in out View; Area : Rect; To : Zoom_Step; X, Y : Natural) is
      DX : constant Integer := Integer (X) - Integer (Area.x);
      DY : constant Integer := Integer (Y) - Integer (Area.y);
      WX : constant Integer := World_X (V, Area, X);
      WY : constant Integer := World_Y (V, Area, Y);
   begin
      V.Zoom := To;
      V.Pan_X := WX - DX * ZOOM_D (To) / ZOOM_N (To);
      V.Pan_Y := WY - DY * ZOOM_D (To) / ZOOM_N (To);
   end Zoom_At;

   procedure Pointer_Down (V : in out View; X, Y : Natural) is
   begin
      V.Dragging := True;
      V.Drag_Moved := False;
      V.Drag_X := X;
      V.Drag_Y := Y;
      V.Drag_Pan_X := V.Pan_X;
      V.Drag_Pan_Y := V.Pan_Y;
   end Pointer_Down;

   procedure Pointer_Move (V : in out View; X, Y : Natural; Changed : out Boolean) is
      DX : constant Integer := Integer (X) - Integer (V.Drag_X);
      DY : constant Integer := Integer (Y) - Integer (V.Drag_Y);
   begin
      Changed := False;
      if V.Dragging and then (V.Drag_Moved or else abs DX > DRAG_SLOP or else abs DY > DRAG_SLOP) then
         V.Drag_Moved := True;
         V.Pan_X := V.Drag_Pan_X - DX * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom);
         V.Pan_Y := V.Drag_Pan_Y - DY * ZOOM_D (V.Zoom) / ZOOM_N (V.Zoom);
         Changed := True;
      end if;
   end Pointer_Move;

   procedure Pointer_Up (V : in out View; M : Model; Area : Rect; X, Y : Natural; Result : out Hit;
                         Changed : out Boolean) is
      Was_Drag : constant Boolean := V.Dragging and then V.Drag_Moved;
      WX : constant Integer := World_X (V, Area, X);
      WY : constant Integer := World_Y (V, Area, Y);
   begin
      Result := (others => <>);
      V.Dragging := False;
      Changed := Was_Drag;
      if Was_Drag then
         return;
      end if;
      V.Selected := 0;
      Changed := True;
      for N in reverse 1 .. Node_Total (M) loop
         declare
            Bx : constant Box := Box_Of (V.Places, N);
         begin
            if WX >= Bx.X and then WX < Bx.X + Bx.W and then WY >= Bx.Y and then WY < Bx.Y + Bx.H then
               V.Selected := N;
               Result := (Kind => Hit_Node, Node => N, Row => 0);
               if WY >= Bx.Y + HEADER_HEIGHT then
                  declare
                     R : constant Integer := (WY - Bx.Y - HEADER_HEIGHT) / ROW_HEIGHT + 1;
                  begin
                     if R in 1 .. Row_Total (M, N) then
                        Result.Row := R;
                        if Port (M, N, R) = Open_Port then
                           Result.Kind := Hit_Port;
                        end if;
                     end if;
                  end;
               end if;
               exit;
            end if;
         end;
      end loop;
   end Pointer_Up;

   procedure Wheel (V : in out View; Area : Rect; X, Y : Natural; Steps : Integer; Changed : out Boolean) is
   begin
      Changed := False;
      if Steps > 0 and then V.Zoom < Zoom_Step'Last then
         Zoom_At (V, Area, V.Zoom + 1, X, Y);
         Changed := True;
      elsif Steps < 0 and then V.Zoom > Zoom_Step'First then
         Zoom_At (V, Area, V.Zoom - 1, X, Y);
         Changed := True;
      end if;
   end Wheel;

   procedure Key (V : in out View; M : Model; Area : Rect; Code : Unsigned_64; Handled : out Boolean) is
      CX : constant Natural := Area.x + Area.w / 2;
      CY : constant Natural := Area.y + Area.h / 2;
   begin
      Handled := True;
      case Code is
         when KEY_LEFT => V.Pan_X := V.Pan_X - PAN_STEP;
         when KEY_RIGHT => V.Pan_X := V.Pan_X + PAN_STEP;
         when KEY_UP => V.Pan_Y := V.Pan_Y - PAN_STEP;
         when KEY_DOWN => V.Pan_Y := V.Pan_Y + PAN_STEP;
         when KEY_EQUAL =>
            if V.Zoom < Zoom_Step'Last then Zoom_At (V, Area, V.Zoom + 1, CX, CY); end if;
         when KEY_MINUS =>
            if V.Zoom > Zoom_Step'First then Zoom_At (V, Area, V.Zoom - 1, CX, CY); end if;
         when KEY_0 => Fit (V, M, Area);
         when KEY_R =>
            Relayout (V, M, Area);
            Fit (V, M, Area);
         when others => Handled := False;
      end case;
   end Key;
end CuBit.UI.Graphs.Views;
