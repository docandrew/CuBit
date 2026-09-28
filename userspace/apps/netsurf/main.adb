------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Web browser: CuBit.UI chrome (toolbar, location field, scrollbar,
--  status bar) around a Surface that the embedded NetSurf engine paints.
--  NetSurf renders pages; everything else is native Ada UI.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.Input;
with CuBit.UI.State;
with CuBit.UI.Surfaces;
with CuBit.UI.Widgets;
with CCL_Manifest_Bindings;
with Browser_Engine;

procedure Main is
   package E renames Browser_Engine;
   package C renames Interfaces.C;
   package App renames CuBit.UI.App;
   package Editor renames CuBit.UI.Editor;
   package Surfaces renames CuBit.UI.Surfaces;
   use type C.int;
   use type C.size_t;
   use type Surfaces.Event_Kind;

   Back_ID : constant := 1;
   Forward_ID : constant := 2;
   Reload_ID : constant := 3;
   Location_ID : constant := 4;
   Scroll_ID : constant := 5;
   Page_ID : constant := 6;

   Toolbar_Height : constant := 40;
   Status_Height : constant := 24;
   Scrollbar_Width : constant := 14;

   --  Desktop scancodes used by the chrome.
   Key_Esc : constant := 16#01#;
   Key_Backspace : constant := 16#0E#;
   Key_Enter : constant := 16#1C#;
   Key_A : constant := 16#1E#;
   Key_L : constant := 16#26#;
   Key_R : constant := 16#13#;
   Key_F5 : constant := 16#3F#;
   Key_Home : constant := 16#47#;
   Key_Left : constant := 16#4B#;
   Key_Right : constant := 16#4D#;
   Key_End : constant := 16#4F#;
   Key_Delete : constant := 16#53#;

   Win : App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;

   Toolbar_Area, Back_Area, Forward_Area, Reload_Area, Location_Area,
   Page_Area, Scrollbar_Area, Status_Area : Rect := (others => 0);

   Page : Surfaces.Surface;
   Location : Editor.Edit_State;
   Location_Focused : Boolean := False;
   Location_First : Natural := 0;  --  first displayed character, 0-based
   Started : Boolean := False;
   Start_Error : C.int := 0;
   Next_Poll : Unsigned_64 := 0;
   Ignore : Unsigned_64;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   function Contains (Outer, Inner : Rect) return Boolean is
     (Inner.x >= Outer.x and then Inner.y >= Outer.y and then
      Inner.x + Inner.w <= Outer.x + Outer.w and then
      Inner.y + Inner.h <= Outer.y + Outer.h);

   procedure Layout is
      W : constant Natural := App.Width (Win);
      H : constant Natural := App.Height (Win);
      Page_H : constant Natural :=
        (if H > Toolbar_Height + Status_Height
         then H - Toolbar_Height - Status_Height else 0);
      Page_W : constant Natural :=
        (if W > Scrollbar_Width then W - Scrollbar_Width else 0);
   begin
      Toolbar_Area := (0, 0, W, Toolbar_Height);
      Back_Area := (8, 7, 60, 26);
      Forward_Area := (72, 7, 72, 26);
      Reload_Area := (148, 7, 64, 26);
      Location_Area :=
        (220, 7, (if W > 228 then W - 228 else 0), 26);
      Page_Area := (0, Toolbar_Height, Page_W, Page_H);
      Scrollbar_Area := (Page_W, Toolbar_Height, Scrollbar_Width, Page_H);
      Status_Area :=
        (0, (if H > Status_Height then H - Status_Height else 0),
         W, Status_Height);
      Page.area := Page_Area;
   end Layout;

   --  Show the engine's current URL in the location field.
   procedure Load_Location is
      Text : constant String := E.Value (E.URL);
      Accepted : Boolean;
   begin
      Editor.Initialize
        (Location,
         Text (Text'First ..
               Text'First - 1 + Natural'Min (Text'Length, Editor.MAX_TEXT_LENGTH)),
         Accepted);
      Location_First := 0;
   end Load_Location;

   --  Something was asked of the engine: let it run promptly.
   procedure Touch is
      T : constant Unsigned_64 := Now;
   begin
      if Next_Poll = 0 or else Next_Poll > T then
         Next_Poll := T;
      end if;
   end Touch;

   --  Fold engine notifications into the window's damage.
   procedure Collect (Dirty : in out Rect) is
   begin
      if E.Damage_All then
         Dirty := Union_Rect (Dirty, Page_Area);
      elsif not Is_Empty (E.Damage) then
         Dirty := Union_Rect
           (Dirty, (Page_Area.x + E.Damage.x, Page_Area.y + E.Damage.y,
                    E.Damage.w, E.Damage.h));
      end if;
      E.Damage := (others => 0);
      E.Damage_All := False;
      if E.URL_Changed then
         E.URL_Changed := False;
         if not Location_Focused then
            Load_Location;
         end if;
      end if;
      if E.Title_Changed then
         E.Title_Changed := False;
         App.Set_Title
           (Win, (if E.Title.Length = 0 then "NetSurf"
                  else E.Value (E.Title) & " - NetSurf"));
      end if;
      if E.Chrome_Changed then
         E.Chrome_Changed := False;
         Dirty := Union_Rect (Dirty, Toolbar_Area);
         Dirty := Union_Rect (Dirty, Scrollbar_Area);
         Dirty := Union_Rect (Dirty, Status_Area);
      end if;
      if E.Cursor_Changed then
         --  The page control's cursor is re-registered by the next render.
         E.Cursor_Changed := False;
         Dirty := Union_Rect (Dirty, (Page_Area.x, Page_Area.y, 1, 1));
      end if;
   end Collect;

   ---------------------------------------------------------------------------
   --  Location field
   ---------------------------------------------------------------------------

   function Location_Text return String is (Editor.Content (Location));

   --  Scroll the field so the cursor stays visible.
   procedure Reveal_Location_Cursor is
      Text : constant String := Location_Text;
      Cursor : constant Natural := Editor.Cursor (Location) - 1;
      Room : constant Natural :=
        (if Location_Area.w > 20 then Location_Area.w - 20 else 1);
   begin
      if Cursor < Location_First then
         Location_First := Cursor;
      end if;
      while Location_First < Cursor and then
        UI_Text_Width (Text (Text'First + Location_First ..
                             Text'First + Cursor - 1)) > Room
      loop
         Location_First := Location_First + 1;
      end loop;
   end Reveal_Location_Cursor;

   function Location_Index_At (X : Natural) return Editor.Text_Position is
      Text : constant String := Location_Text;
      Left : Natural := Location_Area.x + 8;
      Width : Natural;
   begin
      for I in Text'First + Location_First .. Text'Last loop
         Width := UI_Text_Width (Text (I .. I));
         if X < Left + Width / 2 then
            return I - Text'First + 1;
         end if;
         Left := Left + Width;
      end loop;
      return Text'Length + 1;
   end Location_Index_At;

   procedure Draw_Location (Canvas : CuBit.UI.Canvas; Colors : Theme) is
      Text : constant String := Location_Text;
      Shown : constant String :=
        Text (Text'First + Natural'Min (Location_First, Text'Length) ..
              Text'Last);
      function Shift (Position : Editor.Text_Position) return Natural is
        (if Position - 1 < Location_First then 0
         else Position - 1 - Location_First);
   begin
      Draw_Text_Edit_Field
        (Canvas, Location_Area, Colors, Shown,
         Shift (Editor.Cursor (Location)),
         Shift (Editor.Selection_First (Location)),
         Shift (Editor.Selection_Last (Location)),
         Location_Focused, False);
   end Draw_Location;

   procedure Focus_Location is
   begin
      Location_Focused := True;
      Page.focused := False;
      Editor.Select_All (Location);
      Reveal_Location_Cursor;
   end Focus_Location;

   procedure Go is
      Text : constant String := Location_Text;
   begin
      if Text'Length > 0 and then
        E.Navigate (Text'Address, C.size_t (Text'Length)) = 0
      then
         Location_Focused := False;
         Page.focused := True;
      end if;
      Touch;
   end Go;

   --  Keyboard editing of the location field. Returns True when handled.
   function Edit_Location (Event : App.Input_Event) return Boolean is
      Code : constant Unsigned_64 := Event.payload0;
      Shift : constant Boolean := (Event.payload1 and App.KEYMOD_SHIFT) /= 0;
      Ctrl : constant Boolean := (Event.payload1 and App.KEYMOD_CTRL) /= 0;
      Changed : Boolean;
   begin
      if Event.kind = App.INPUT_TEXT then
         if Code in 32 .. 126 then
            Editor.Insert
              (Location, String'(1 => Character'Val (Natural (Code))), Changed);
         end if;
      elsif Event.kind = App.INPUT_KEY_DOWN then
         case Code is
            when Key_Enter => Go;
            when Key_Esc =>
               Load_Location;
               Location_Focused := False;
               Page.focused := True;
            when Key_Backspace => Editor.Backspace (Location, Changed);
            when Key_Delete => Editor.Delete_Forward (Location, Changed);
            when Key_Left =>
               Editor.Move (Location, (if Ctrl then Editor.Move_Word_Left
                                       else Editor.Move_Left), Shift);
            when Key_Right =>
               Editor.Move (Location, (if Ctrl then Editor.Move_Word_Right
                                       else Editor.Move_Right), Shift);
            when Key_Home => Editor.Move (Location, Editor.Move_Start, Shift);
            when Key_End => Editor.Move (Location, Editor.Move_End, Shift);
            when Key_A =>
               if Ctrl then
                  Editor.Select_All (Location);
               end if;
            when others => return False;
         end case;
      else
         return False;
      end if;
      Reveal_Location_Cursor;
      return True;
   end Edit_Location;

   ---------------------------------------------------------------------------
   --  Rendering
   ---------------------------------------------------------------------------

   procedure Paint_Page (Canvas : CuBit.UI.Canvas; Colors : Theme) is
      View : constant CuBit.UI.Canvas := Surfaces.View (Canvas, Page_Area);
      Clip : constant Rect :=
        (if View.clipEnabled then View.clip
         else (0, 0, View.width, View.height));
   begin
      if View.width = 0 or else View.height = 0 or else Is_Empty (Clip) then
         return;
      end if;
      if not Started then
         Fill_Rect (View, (0, 0, View.width, View.height), Colors.face);
         Draw_UI_Text_Transparent
           (View, 16, 16, "The NetSurf engine did not start (code" &
            C.int'Image (Start_Error) & ").", Colors.text);
         return;
      end if;
      E.Redraw
        (View.addr, C.int (View.width), C.int (View.height),
         C.int (View.pitch), C.int (Clip.x), C.int (Clip.y),
         C.int (Clip.w), C.int (Clip.h));
   end Paint_Page;

   procedure Render (Win : in out App.Window; Damage : Rect) is
      Canvas : constant CuBit.UI.Canvas := App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      Result : Widget_Result;
      Max_Scroll : constant Natural :=
        (if E.Extent_Height > Page_Area.h
         then E.Extent_Height - Page_Area.h else 0);
      Scroll : Natural := Natural'Min (E.Scroll_Y, Max_Scroll);
   begin
      CuBit.UI.State.Begin_Frame (UI);
      CuBit.UI.Controls.Clear (Controls);

      CuBit.UI.Widgets.Toolbar (Canvas, Toolbar_Area, Colors);
      if Started and then E.Can_Go_Back /= 0 then
         CuBit.UI.Widgets.Button
           (Canvas, UI, Controls, Back_ID, Back_Area, Toolbar_Area, Colors,
            "Back", Result, retainedInput => True);
      else
         CuBit.UI.Widgets.Disabled_Button (Canvas, Back_Area, Colors, "Back");
      end if;
      if Started and then E.Can_Go_Forward /= 0 then
         CuBit.UI.Widgets.Button
           (Canvas, UI, Controls, Forward_ID, Forward_Area, Toolbar_Area,
            Colors, "Forward", Result, retainedInput => True);
      else
         CuBit.UI.Widgets.Disabled_Button
           (Canvas, Forward_Area, Colors, "Forward");
      end if;
      CuBit.UI.Widgets.Button
        (Canvas, UI, Controls, Reload_ID, Reload_Area, Toolbar_Area, Colors,
         (if E.Busy then "Stop" else "Reload"), Result,
         retainedInput => True);
      CuBit.UI.Controls.Add
        (Controls, Location_ID, Location_Area, Location_Area,
         Pointer_Text);
      Draw_Location (Canvas, Colors);

      CuBit.UI.Widgets.Vertical_Scrollbar
        (Canvas, UI, Controls, Scroll_ID, Scrollbar_Area,
         Union_Rect (Page_Area, Scrollbar_Area), Colors, 0, Max_Scroll,
         Scroll, Result, Positive'Max (1, Page_Area.h),
         retainedInput => True);
      if Started and then Scroll /= E.Scroll_Y then
         --  Scrollbar drag: move the page before painting it below. The
         --  resulting page damage is painted by this frame when it covers
         --  the page, and by a follow-up full frame otherwise.
         E.Scroll_To (C.int (E.Scroll_X), C.int (Scroll));
         E.Damage := (others => 0);
         E.Damage_All := False;
         if not Contains (Damage, Page_Area) then
            CuBit.UI.State.Request_Followup_Render (UI);
         end if;
         Touch;
      end if;

      CuBit.UI.Controls.Add_Surface (Controls, Page_ID, Page_Area, E.Cursor);
      Paint_Page (Canvas, Colors);

      Draw_Status_Bar
        (Canvas, Status_Area, Colors, E.Value (E.Status),
         (if E.Busy then "Loading"
          elsif E.URL.Length >= 6 and then E.URL.Data (1 .. 6) = "https:"
          then "Encrypted (tls.svc)"
          elsif E.URL.Length >= 5 and then E.URL.Data (1 .. 5) = "http:"
          then "Not encrypted"
          else ""));
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   ---------------------------------------------------------------------------
   --  Input
   ---------------------------------------------------------------------------

   procedure Route_To_Page
     (Event : App.Input_Event; Dirty : in out Rect)
   is
      Routed : Surfaces.Surface_Event;
   begin
      Surfaces.Route (Page, Event, Routed);
      if not Started or else Routed.kind = Surfaces.No_Event then
         return;
      end if;
      case Routed.kind is
         when Surfaces.Pointer_Move =>
            E.Pointer (0, C.int (Routed.x), C.int (Routed.y),
                       (if Routed.primaryDown then 1 else 0));
         when Surfaces.Pointer_Down =>
            E.Pointer (1, C.int (Routed.x), C.int (Routed.y), 1);
         when Surfaces.Pointer_Up =>
            E.Pointer (2, C.int (Routed.x), C.int (Routed.y), 0);
         when Surfaces.Pointer_Leave =>
            E.Pointer (3, 0, 0, 0);
         when Surfaces.Wheel =>
            E.Wheel (C.int (Routed.x), C.int (Routed.y),
                     C.int (Routed.wheelDelta));
         when Surfaces.Key_Down =>
            if Routed.code = Key_Esc and then E.Busy then
               E.Stop;
            else
               E.Key (C.unsigned (Routed.code and 16#FF#),
                      C.unsigned (Routed.modifiers and 3));
            end if;
         when Surfaces.Text =>
            E.Text (Unsigned_32 (Routed.code and 16#1F_FFFF#));
         when Surfaces.Key_Up | Surfaces.No_Event =>
            null;
      end case;
      Touch;
      Collect (Dirty);
   end Route_To_Page;

   procedure Handle_Event
     (Win : in out App.Window; Event : App.Input_Event;
      Dirty : in out Rect; Running : in out Boolean)
   is
      Code : constant Unsigned_64 := Event.payload0;
      Ctrl : constant Boolean := (Event.payload1 and App.KEYMOD_CTRL) /= 0;
      Alt : constant Boolean := (Event.payload1 and App.KEYMOD_ALT) /= 0;
      Hit : CuBit.UI.Controls.Control_ID;
      X, Y : Natural;
      pragma Unreferenced (Running);
   begin
      if Event.kind = App.INPUT_CONFIGURE then
         Layout;
         if Started then
            E.Resize (C.int (Page_Area.w), C.int (Page_Area.h));
            Touch;
         end if;
         Dirty := App.Full_Rect (Win);
         return;
      end if;

      --  Window-wide shortcuts.
      if Event.kind = App.INPUT_KEY_DOWN and then Started then
         if Ctrl and then Code = Key_L then
            Focus_Location;
            Dirty := Union_Rect (Dirty, Toolbar_Area);
            return;
         elsif Code = Key_F5 or else (Ctrl and then Code = Key_R) then
            E.Reload;
            Touch;
            Collect (Dirty);
            return;
         elsif Alt and then Code in Key_Left | Key_Right then
            if Code = Key_Left then
               E.Back;
            else
               E.Forward;
            end if;
            Touch;
            Collect (Dirty);
            return;
         end if;
      end if;

      --  Toolbar buttons.
      if Event.kind = App.INPUT_POINTER_UP and then Started then
         X := CuBit.UI.Input.Pointer_X (Event);
         Y := CuBit.UI.Input.Pointer_Y (Event);
         Hit := CuBit.UI.Controls.Hit (Controls, X, Y);
         if CuBit.UI.Controls.Take_Activated (Controls, Hit) then
            case Hit is
               when Back_ID => E.Back;
               when Forward_ID => E.Forward;
               when Reload_ID =>
                  if E.Busy then
                     E.Stop;
                  else
                     E.Reload;
                  end if;
               when others => null;
            end case;
            Touch;
            Collect (Dirty);
            Dirty := Union_Rect (Dirty, Toolbar_Area);
         end if;
      end if;

      --  Location field focus and cursor placement.
      if Event.kind = App.INPUT_POINTER_DOWN then
         X := CuBit.UI.Input.Pointer_X (Event);
         Y := CuBit.UI.Input.Pointer_Y (Event);
         if Point_In_Rect (X, Y, Location_Area) then
            if Location_Focused then
               Editor.Place_Cursor (Location, Location_Index_At (X));
            else
               Focus_Location;
            end if;
            Dirty := Union_Rect (Dirty, Location_Area);
         elsif Location_Focused then
            Location_Focused := False;
            Dirty := Union_Rect (Dirty, Location_Area);
         end if;
      end if;

      if Location_Focused and then
        Event.kind in App.INPUT_KEY_DOWN | App.INPUT_TEXT
      then
         if Edit_Location (Event) then
            Dirty := Union_Rect (Dirty, Location_Area);
            Collect (Dirty);
         end if;
         return;
      end if;

      Route_To_Page (Event, Dirty);
   end Handle_Event;

   function Next_Deadline return Unsigned_64 is (Next_Poll);

   procedure On_Deadline
     (Win : in out App.Window; Dirty : in out Rect; Running : in out Boolean)
   is
      pragma Unreferenced (Win, Running);
      Wait : C.int;
   begin
      if not Started then
         Next_Poll := 0;
         return;
      end if;
      Wait := E.Poll;
      Next_Poll :=
        (if Wait < 0 then 0 else Now + Unsigned_64 (C.int'Max (Wait, 1)));
      Collect (Dirty);
   end On_Deadline;

   procedure Run_UI is new App.Run
     (ui => UI, controls => Controls, Render => Render,
      Handle_Event => Handle_Event, Next_Deadline => Next_Deadline,
      On_Deadline => On_Deadline);

   Opened : Boolean;
begin
   App.Open
     (Win, 880, 600,
      App.WINDOW_FLAG_DECORATED or App.WINDOW_FLAG_RESIZABLE or
      App.WINDOW_FLAG_MINIMIZABLE or App.WINDOW_FLAG_MAXIMIZABLE or
      App.WINDOW_FLAG_CLOSEABLE, Opened, title => "NetSurf");
   if Opened then
      Layout;
      Start_Error := E.Start
        (System.Null_Address, 0, C.int (Page_Area.w), C.int (Page_Area.h),
         CCL_Manifest_Bindings.Slot_tls);
      Started := Start_Error = 0;
      if Started then
         debugPrint ("netsurf: native shell ready" & ASCII.LF);
         Page.focused := True;
         Touch;
         declare
            Startup : Rect := (others => 0);
         begin
            Collect (Startup);  --  URL and title from the first navigation
         end;
      else
         debugPrint ("netsurf: engine start failed, code" & C.int'Image (Start_Error) & ASCII.LF);
      end if;
      Run_UI (Win);
      App.Close (Win);
   end if;
   Ignore := syscall (SYSCALL_EXIT);
end Main;
