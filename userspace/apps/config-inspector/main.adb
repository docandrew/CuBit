with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config_Inspection;
with CuBit.Config_Reader;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Labels;
with CuBit.UI.State;
with CuBit.UI.Trees;
with CuBit.UI.Widgets;
with Config_Tree;

procedure Main is
   package T renames Config_Tree;
   package P renames CuBit.Config_Inspection;
   use type P.Status;
   Refresh_ID : constant := 1;
   Split_ID : constant := 2;
   Tree_Scroll_ID : constant := 3;
   Value_Scroll_ID : constant := 4;
   First_Row_ID : constant := 10;
   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Model : T.Model;
   Rows : T.Rows;
   Selected : Natural := 1;
   Tree_Scroll, Value_Scroll : Natural := 0;
   Split : Natural := 285;
   Tree_Bounds : Rect := (others => 0);
   Tree_Viewport : Rect := (others => 0);
   Value_Bounds : Rect := (others => 0);
   Visible_Rows : Positive := 1;
   Value_Max_Scroll : Natural := 0;
   Focused : Boolean := True;
   Loaded : Boolean := False;
   Load_Status : P.Status := P.Unavailable;
   Value : P.Text;
   Value_Status : P.Status := P.OK;
   Ignore : Unsigned_64;

   function Explain (Status : P.Status) return String is
     (case Status is
        when P.OK => "Ready",
        when P.Denied => "Config read authority denied",
        when P.Missing => "Entry no longer exists; refresh the tree",
        when P.Too_Large => "Result exceeds the 1024-byte inspector limit; not truncated",
        when P.Invalid_Request => "Invalid Config response or unsupported namespace",
        when P.Unavailable => "Config service unavailable");

   procedure Read_Selected is
   begin
      Value := (others => <>); Value_Scroll := 0; Value_Status := P.OK;
      if Selected in 1 .. Model.Count and then Model.Nodes (Selected).Has_Value then
         CuBit.Config_Reader.Query (P.Read_Value, T.Key (Model, Selected), Value, Value_Status);
      end if;
   end Read_Selected;

   procedure Refresh is
      Names : P.Text;
      Accepted : Boolean;
      Old_Key : constant String := T.Key (Model, Selected);
   begin
      CuBit.Config_Reader.Query (P.List_Keys, "", Names, Load_Status);
      if Load_Status /= P.OK then return; end if;
      T.Load (Model, Names.Data (1 .. Names.Length), Accepted);
      if not Accepted then Load_Status := P.Invalid_Request; return; end if;
      Loaded := True;
      Selected := T.Find (Model, Old_Key);
      if Selected = 0 then Selected := 1; end if;
      Rows := T.Visible (Model);
      Tree_Scroll := 0;
      Read_Selected;
      debugPrint ("config-inspector: snapshot ready" & ASCII.LF);
   end Refresh;

   procedure Reveal_Selection is
   begin
      Rows := T.Visible (Model);
      for I in 1 .. Rows.Count loop
         if Rows.IDs (I) = Selected then
            if I <= Tree_Scroll then Tree_Scroll := I - 1;
            elsif I > Tree_Scroll + Visible_Rows then Tree_Scroll := I - Visible_Rows;
            end if;
            exit;
         end if;
      end loop;
   end Reveal_Selection;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Full : constant Rect := CuBit.UI.App.Full_Rect (Win);
      Colors : constant Theme := Current_Theme;
      Toolbar, Workspace, Left, Right, Content, Frame, Scrollbar : Rect;
      Result : Widget_Result;
      Maximum : Natural;
      procedure Text (R : Rect; S : String; Muted : Boolean := False) is
      begin
         CuBit.UI.Labels.Label (C, R, Colors, S, Muted);
      end Text;
   begin
      CuBit.UI.State.Begin_Frame (UI);
      CuBit.UI.Controls.Clear (Controls);
      Fill_Rect (C, Full, Colors.face);
      if Full.w < 600 or Full.h < 260 then
         Text (Full, "Enlarge this window to inspect configuration.");
         CuBit.UI.State.Finish_Frame (UI); return;
      end if;
      Toolbar := (8, 8, Full.w - 16, 38);
      CuBit.UI.Widgets.Toolbar (C, Toolbar, Colors);
      CuBit.UI.Widgets.Button (C, UI, Controls, Refresh_ID, (15, 14, 82, 26),
                              Toolbar, Colors, "Refresh", Result, retainedInput => True);
      Text ((110, 15, Toolbar.w - 240, 22), "Machine configuration", True);
      CuBit.UI.Widgets.Badge (C, (Full.w - 115, 16, 96, 21), Colors,
                              "Read only", CuBit.UI.Widgets.Badge_Neutral);
      Workspace := (8, 52, Full.w - 16, Full.h - 88);
      CuBit.UI.Widgets.Split_Pane (C, UI, Controls, Split_ID, Workspace, Workspace,
        Colors, True, Split, Left, Right, 7, 220, 300, retainedInput => True);
      CuBit.UI.Widgets.Group_Box (C, Left, Colors, "Configuration", Content, 8);
      Tree_Bounds := Content;
      CuBit.UI.Trees.View_Frame (C, Content, Colors, Focused, Frame);
      Tree_Viewport := (Frame.x, Frame.y, Frame.w - 16, Frame.h);
      Scrollbar := (Frame.x + Frame.w - 14, Frame.y, 14, Frame.h);
      Visible_Rows := Positive'Max (1, Natural'Min
        (CuBit.UI.Controls.MAX_CONTROLS - 5, Tree_Viewport.h / CuBit.UI.Trees.TREE_ROW_HEIGHT));
      Rows := T.Visible (Model);
      Maximum := (if Rows.Count > Visible_Rows then Rows.Count - Visible_Rows else 0);
      Tree_Scroll := Natural'Min (Tree_Scroll, Maximum);
      for Position in 1 .. Natural'Min (Visible_Rows, Rows.Count - Tree_Scroll) loop
         declare
            ID : constant T.Node_ID := Rows.IDs (Tree_Scroll + Position);
            N : T.Node renames Model.Nodes (ID);
            R : constant Rect := (Tree_Viewport.x, Tree_Viewport.y + (Position - 1) * 24,
                                  Tree_Viewport.w, 24);
         begin
            CuBit.UI.Trees.Tree_Item
              (C, UI, Controls, First_Row_ID + ID, R, Tree_Bounds,
               Colors, T.Label (Model, ID), ID, Selected,
               N.Depth, N.Expanded, N.Child /= 0,
               (if ID = 1 then CuBit.UI.Trees.Computer_Icon
                elsif N.Child /= 0 or not N.Has_Value then CuBit.UI.Trees.Folder_Icon
                else CuBit.UI.Trees.Setting_Icon), Focused, N.Next = 0,
               result => Result, retainedInput => True);
         end;
      end loop;
      CuBit.UI.Widgets.Vertical_Scrollbar (C, UI, Controls, Tree_Scroll_ID, Scrollbar,
        Tree_Bounds, Colors, 0, Maximum, Tree_Scroll, Result, Visible_Rows, retainedInput => True);

      CuBit.UI.Widgets.Group_Box (C, Right, Colors, "Selected entry", Content, 8);
      Text ((Content.x, Content.y, Content.w, 24),
            (if Selected = 1 then "Machine context" else T.Key (Model, Selected)));
      Text ((Content.x, Content.y + 26, Content.w, 22),
        (if Model.Nodes (Selected).Has_Value then "Stored bytes (no declared schema)"
         else "Namespace / select a setting to read its value"), True);
      Value_Bounds := (Content.x, Content.y + 56, Content.w, Content.h - 56);
      CuBit.UI.Draw_Table_Viewport (C, Value_Bounds, Colors);
      declare
         Interior : constant Rect := Table_Interior (Value_Bounds);
         Text_Area : constant Rect := (Interior.x + 6, Interior.y + 6, Interior.w - 28, Interior.h - 12);
         Columns : constant Positive := Positive'Max (1, Text_Area.w / 8);
         Lines : constant Positive := Positive'Max (1, Text_Area.h / Code_Text_Height);
         Line_Start : array (1 .. 1025) of Positive := [others => 1];
         Line_End : array (1 .. 1025) of Natural := [others => 0];
         Count : Positive := 1;
         Display : constant String :=
           (if Value_Status /= P.OK then Explain (Value_Status)
            elsif not Model.Nodes (Selected).Has_Value then ""
            elsif Value.Length = 0 then "(empty value)"
            else Value.Data (1 .. Value.Length));
      begin
         for I in Display'Range loop
            if Display (I) = ASCII.LF then
               Line_End (Count) := I - 1;
               Count := Count + 1; Line_Start (Count) := I + 1;
            else
               if I - Line_Start (Count) = Columns then
                  Line_End (Count) := I - 1;
                  Count := Count + 1; Line_Start (Count) := I;
               end if;
               Line_End (Count) := I;
            end if;
         end loop;
         Value_Max_Scroll := (if Count > Lines then Count - Lines else 0);
         Value_Scroll := Natural'Min (Value_Scroll, Value_Max_Scroll);
         for Row in 1 .. Natural'Min (Lines, Count - Value_Scroll) loop
            declare N : constant Positive := Row + Value_Scroll; begin
               Draw_Code_Text (With_Clip (C, Text_Area), Text_Area.x,
                 Text_Area.y + (Row - 1) * Code_Text_Height,
                 Display (Line_Start (N) .. Line_End (N)), Colors.text, Colors.field);
            end;
         end loop;
         CuBit.UI.Widgets.Vertical_Scrollbar (C, UI, Controls, Value_Scroll_ID,
           (Interior.x + Interior.w - 14, Interior.y, 14, Interior.h), Value_Bounds,
           Colors, 0, Value_Max_Scroll, Value_Scroll, Result, Lines, retainedInput => True);
      end;
      Draw_Status_Bar (C, (8, Full.h - 30, Full.w - 16, 22), Colors,
        (if Load_Status /= P.OK then Explain (Load_Status)
         else "Arrows navigate / Left-Right collapse-expand / F5 refresh"),
        (if Load_Status /= P.OK and Loaded then "stale snapshot" else "read-only IPC"));
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event;
      Dirty : in out Rect; Running : in out Boolean)
   is
      X, Y, Position : Natural;
      Hit : CuBit.UI.Controls.Control_ID;
      Changed : Boolean := False;
      Old_Selection : constant Natural := Selected;
   begin
      if Event.kind = CuBit.UI.App.INPUT_KEY_DOWN then
         if Event.payload0 = CuBit.UI.App.KEY_ESC then Running := False;
         elsif Event.payload0 = 16#3F# then Refresh; Changed := True;
         elsif Focused then
            Rows := T.Visible (Model); Position := 1;
            for I in 1 .. Rows.Count loop if Rows.IDs (I) = Selected then Position := I; end if; end loop;
            case Event.payload0 is
               when 16#48# => if Position > 1 then Selected := Rows.IDs (Position - 1); end if;
               when 16#50# => if Position < Rows.Count then Selected := Rows.IDs (Position + 1); end if;
               when 16#47# => Selected := 1;
               when 16#4F# => Selected := Rows.IDs (Rows.Count);
               when 16#4D# =>
                  if not Model.Nodes (Selected).Expanded then Model.Nodes (Selected).Expanded := True;
                  elsif Model.Nodes (Selected).Child /= 0 then Selected := Model.Nodes (Selected).Child;
                  end if;
               when 16#4B# =>
                  if Model.Nodes (Selected).Expanded and Model.Nodes (Selected).Child /= 0 then
                     Model.Nodes (Selected).Expanded := False;
                  elsif Model.Nodes (Selected).Parent /= 0 then Selected := Model.Nodes (Selected).Parent;
                  end if;
               when others => return;
            end case;
            Reveal_Selection; Changed := True;
         end if;
      elsif Event.kind = CuBit.UI.App.INPUT_POINTER_DOWN then
         X := Natural (Event.payload0 and 16#FFFF_FFFF#); Y := Natural (Shift_Right (Event.payload0, 32));
         Focused := Point_In_Rect (X, Y, Tree_Bounds);
         Dirty := Union_Rect (Dirty, Tree_Bounds);
      elsif Event.kind = CuBit.UI.App.INPUT_POINTER_UP then
         X := Natural (Event.payload0 and 16#FFFF_FFFF#); Y := Natural (Shift_Right (Event.payload0, 32));
         Hit := CuBit.UI.Controls.Hit (Controls, X, Y);
         if CuBit.UI.Controls.Take_Activated (Controls, Hit) then
            if Hit = Refresh_ID then Refresh; Changed := True;
            elsif Hit >= First_Row_ID then
               Position := Hit - First_Row_ID;
               if Position in 1 .. Model.Count then
                  Selected := Position;
                  declare
                     Disclosure : constant Natural := Tree_Viewport.x + Model.Nodes (Selected).Depth * 18 + 4;
                  begin
                     if X >= Disclosure and X < Disclosure + 9 and Model.Nodes (Selected).Child /= 0 then
                        Model.Nodes (Selected).Expanded := not Model.Nodes (Selected).Expanded;
                     end if;
                  end;
                  Changed := True;
               end if;
            end if;
         end if;
      elsif Event.kind = CuBit.UI.App.INPUT_POINTER_WHEEL then
         X := Natural (Event.payload0 and 16#FFFF_FFFF#); Y := Natural (Shift_Right (Event.payload0, 32));
         declare
            Wheel : constant Integer := CuBit.UI.App.Pointer_Wheel_Delta (Event);
            Maximum : constant Natural := (if Rows.Count > Visible_Rows then Rows.Count - Visible_Rows else 0);
         begin
            if Point_In_Rect (X, Y, Tree_Bounds) then
               if Wheel > 0 and Tree_Scroll > 0 then Tree_Scroll := Tree_Scroll - 1;
               elsif Wheel < 0 then Tree_Scroll := Natural'Min (Maximum, Tree_Scroll + 1); end if;
               Dirty := Union_Rect (Dirty, Tree_Bounds);
            elsif Point_In_Rect (X, Y, Value_Bounds) then
               if Wheel > 0 and Value_Scroll > 0 then Value_Scroll := Value_Scroll - 1;
               elsif Wheel < 0 then Value_Scroll := Natural'Min (Value_Max_Scroll, Value_Scroll + 1); end if;
               Dirty := Union_Rect (Dirty, Value_Bounds);
            end if;
         end;
      end if;
      if Selected /= Old_Selection then Read_Selected; end if;
      if Changed then Dirty := CuBit.UI.App.Full_Rect (Win); end if;
   end Handle_Event;
   procedure Run_UI is new CuBit.UI.App.Run
     (ui => UI, controls => Controls, Render => Render, Handle_Event => Handle_Event);
   Opened : Boolean;
begin
   Refresh;
   CuBit.UI.App.Open (Win, 860, 560,
     CuBit.UI.App.WINDOW_FLAG_DECORATED or CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
     CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
     CuBit.UI.App.WINDOW_FLAG_CLOSEABLE, Opened, title => "Config Inspector");
   if Opened then
      debugPrint ("config-inspector: native window ready" & ASCII.LF);
      Run_UI (Win);
      CuBit.UI.App.Close (Win);
   end if;
   Ignore := syscall (SYSCALL_EXIT);
end Main;
