------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Focus;
with CuBit.UI.Widgets;
with CuBit.Net_Link;
with Desktop_Deskbar_Style;
with CuBit.UI_Metrics;
with Desktop_Icons;
with Desktop_Icon_Pixels;
with Desktop_Load_Bars;
with Desktop_Net_Address;
with Desktop_Network_Glyphs;

package body Desktop_Status_Panel is
   use Interfaces;
   use type System.Address;
   use type CuBit.Wifi_Control.State, CuBit.Net_Link.Link_Kind, CuBit.Net_Link.Link_Condition;
   use type DNM.Phase, DNM.Failure, DNM.Security, DNR.Row_Kind, DAE.Method, DAE.Outcome;
   package Controls renames CuBit.UI.Controls;
   package Focus renames CuBit.UI.Focus;
   package W renames CuBit.UI.Widgets;
   package Editor renames CuBit.UI.Editor;
   package DBS renames Desktop_Deskbar_Style;
   package M renames CuBit.UI_Metrics;
   package DNA renames Desktop_Net_Address;
   package DLB renames Desktop_Load_Bars;
   use type DN.Urgency, DNA.Problem, NMR.Policy;

   Width : constant := DBS.Panel_Width;
   Bytes_Per_Pixel : constant := 4;
   Volume_Step : constant := 5;

   ---------------------------------------------------------------------------
   --  Control IDs (CuBit.UI.Controls): fixed per control, a row's from its
   --  place in the Network section or the notification list.
   ---------------------------------------------------------------------------
   ID_Mute       : constant := 1;
   ID_Volume     : constant := 2;
   ID_Quiet      : constant := 3;
   ID_Clear      : constant := 4;
   ID_Note_Base  : constant := 10;     --  + note row (dismiss)
   ID_Row_Base   : constant := 20;     --  + network row
   ID_Forget_Base : constant := 40;    --  + network row
   ID_Method     : constant := 60;     --  + method position
   ID_Address    : constant := 63;
   ID_Gateway    : constant := 64;
   ID_DNS        : constant := 65;
   ID_Apply      : constant := 66;
   ID_Revert     : constant := 67;
   ID_Passphrase : constant := 68;
   ID_Capture    : constant := 69;
   pragma Compile_Time_Error
     (ID_Note_Base + DBS.Panel_Visible_Notes >= ID_Row_Base
      or else ID_Row_Base + DNR.Maximum_Rows >= ID_Forget_Base
      or else ID_Forget_Base + DNR.Maximum_Rows >= ID_Method,
      "control ID ranges overlap");

   function Field_ID (F : DAE.Field_Name) return Controls.Control_ID is
     (case F is when DAE.Address_Field => ID_Address, when DAE.Gateway_Field => ID_Gateway,
                when DAE.DNS_Field => ID_DNS);

   ---------------------------------------------------------------------------
   --  State
   ---------------------------------------------------------------------------
   UI       : CuBit.UI.State.UI_State;
   Map      : Controls.Control_Map;
   Keys     : Focus.Focus_State;
   --  A key's activation (Enter/Space) or step (Left/Right) for the
   --  focused control, taken by the next render.
   Pending_Activate : Controls.Control_ID := Controls.NO_CONTROL;
   Pending_Step     : Integer range -1 .. 1 := 0;
   --  A control to focus once a render has drawn it.
   Pending_Focus    : Controls.Control_ID := Controls.NO_CONTROL;

   Secret   : aliased Editor.Edit_State;
   Fields   : array (DAE.Field_Name) of Editor.Edit_State;
   Fields_Editable : array (DAE.Field_Name) of Boolean := [others => False];
   Secret_Ready, Fields_Ready : Boolean := False;

   ---------------------------------------------------------------------------
   --  The buffer (CuAlloc), grown with the panel's height.
   ---------------------------------------------------------------------------
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   procedure Free (Item : System.Address)
     with Import, Convention => C, External_Name => "cualloc_free";
   Heap_Alignment : constant := 64;

   Store : System.Address := System.Null_Address;
   Store_Rows : Natural := 0;
   Rows_Drawn : Natural := 0;

   function Buffer return System.Address is (Store);
   function Buffer_Width return Natural is (Width);
   function Buffer_Height return Natural is (Rows_Drawn);
   function Buffer_Pitch return Natural is (Width * Bytes_Per_Pixel);

   procedure Reserve (Rows : Natural; OK : out Boolean) is
   begin
      OK := True;
      if Rows <= Store_Rows and then Store /= System.Null_Address then
         return;
      end if;
      if Store /= System.Null_Address then
         Free (Store);
      end if;
      Store := Allocate_Zeroed (Unsigned_64 (Rows) * Unsigned_64 (Width * Bytes_Per_Pixel), Heap_Alignment);
      Store_Rows := (if Store = System.Null_Address then 0 else Rows);
      OK := Store /= System.Null_Address;
   end Reserve;

   function To_Short (S : String) return Short_Text is
      T : Short_Text;
      N : constant Natural := Natural'Min (S'Length, T.Data'Length);
   begin
      T.Data (1 .. N) := S (S'First .. S'First + N - 1);
      T.Length := N;
      return T;
   end To_Short;

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------
   Pad : constant := DBS.Panel_Padding;
   --  The label column of the System and Network rows ("Memory",
   --  "Addressing" at the base text size).
   Label_W : constant := 6 * M.Base_Em + M.Space_M;

   function Net_Height (Net : DNR.Layout) return Natural is
     (if Net.Count = 0 then 0 else DBS.Panel_Section + DNR.Total_Height (Net));

   function Height (Net : DNR.Layout; Note_Rows : Natural; System_Shown, Remote_Shown : Boolean)
     return Natural is
     (DBS.Panel_Stripe + DBS.Panel_Header + 2 * DBS.Panel_Section + 2 * DBS.Panel_Row
      + (if Remote_Shown then DBS.Panel_Section + 2 * DBS.Panel_Row else 0)
      + (if System_Shown then DBS.Panel_Section + 2 * DBS.Panel_Row else 0)
      + Net_Height (Net)
      + (if Note_Rows = 0 then DBS.Panel_Row else Note_Rows * DBS.Panel_Note_Row)
      + DBS.Panel_Footer);

   function Count_Text (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Count_Text;

   function Minute_Text (M : DN.Minute_Of_Day) return String is
      Hours : constant Natural := Natural (M) / 60;
      Minutes : constant Natural := Natural (M) mod 60;
      function Digit (V : Natural) return Character is (Character'Val (Character'Pos ('0') + V));
   begin
      return [Digit (Hours / 10), Digit (Hours mod 10), ':', Digit (Minutes / 10), Digit (Minutes mod 10)];
   end Minute_Text;

   function Text_Y (R : Rect) return Natural is
     (R.y + (if R.h > UI_Text_Height then (R.h - UI_Text_Height) / 2 else 0));

   ---------------------------------------------------------------------------
   --  Drawing helpers on the toolkit's primitives
   ---------------------------------------------------------------------------
   --  An icon of the icon store, over Bg.
   procedure Draw_Icon (C : Canvas; Id : Desktop_Icons.Icon_ID; X, Y : Natural; Bg : Color) is
      Edge : Desktop_Icons.Edge_Index := Desktop_Icons.Edge_Index'First;
   begin
      for E in Desktop_Icons.Edge_Index loop
         if Desktop_Icons.Edges (E) = Desktop_Icons.ICON_SIZE then
            Edge := E;
         end if;
      end loop;
      declare
         Item : constant Desktop_Icon_Pixels.Asset := (Desktop_Icon_Pixels.Application, Id, Edge);
         Size : constant Positive := Desktop_Icons.Edges (Edge);
      begin
         for YY in 0 .. Size - 1 loop
            for XX in 0 .. Size - 1 loop
               declare
                  P : constant Unsigned_32 := Desktop_Icon_Pixels.Pixel (Item, XX, YY);
                  A : constant Unsigned_32 := Shift_Right (P, 24);
                  function Mix (Shift : Natural) return Unsigned_32 is
                    ((((Shift_Right (P, Shift) and 16#FF#) * A
                       + (Shift_Right (Bg, Shift) and 16#FF#) * (255 - A)) / 255) and 16#FF#);
               begin
                  if A /= 0 then
                     Set_Pixel (C, X + XX, Y + YY,
                                Shift_Left (Mix (16), 16) or Shift_Left (Mix (8), 8) or Mix (0));
                  end if;
               end;
            end loop;
         end loop;
      end;
   end Draw_Icon;

   package NG renames Desktop_Network_Glyphs;
   Glyph_Size : constant := Desktop_Icons.ICON_SIZE;
   Lock_Width : constant := NG.Lock_Width;

   procedure Draw_Part (C : Canvas; X, Y : Natural; P : NG.Part; Ink : Color) is
   begin
      if P.Outline then
         Stroke_Rect (C, (X + P.X, Y + P.Y, P.W, P.H), Ink, Ink);
      else
         Fill_Rect (C, (X + P.X, Y + P.Y, P.W, P.H), Ink);
      end if;
   end Draw_Part;

   procedure Draw_Signal (C : Canvas; X, Y : Natural; Bars : DNM.Signal_Bars; Ink, Faint : Color) is
   begin
      for B in NG.Bar_Index loop
         Draw_Part (C, X, Y, NG.Bar (B), (if B <= Natural (Bars) then Ink else Faint));
      end loop;
   end Draw_Signal;

   procedure Draw_Port (C : Canvas; X, Y : Natural; Up : Boolean; Ink, Faint : Color) is
   begin
      for P in NG.Port_Index loop
         Draw_Part (C, X, Y, NG.Port (P), (if Up then Ink else Faint));
      end loop;
   end Draw_Port;

   procedure Draw_Lock (C : Canvas; X, Y : Natural; Ink : Color) is
   begin
      for L in NG.Lock_Index loop
         Draw_Part (C, X, Y, NG.Lock (L), Ink);
      end loop;
   end Draw_Lock;

   procedure Section (C : Canvas; R : Rect; Title : String; Colors : Theme) is
   begin
      Fill_Rect (C, R, Colors.panel);
      Fill_Rect (C, (R.x, R.y, R.w, 1), Colors.edge);
      Draw_Emphasis_Text_Transparent (C, R.x + Pad, Text_Y (R), Title, Colors.muted);
   end Section;

   ---------------------------------------------------------------------------
   --  Render
   ---------------------------------------------------------------------------
   procedure Add (Acts : in out Action_List; A : Action) is
   begin
      if Acts.Count < Maximum_Actions then
         Acts.Count := Acts.Count + 1;
         Acts.Items (Acts.Count) := A;
      end if;
   end Add;

   --  A control the keyboard reaches: registered in Tab order; True when
   --  a key activated it this render.
   function Keyed (ID : Controls.Control_ID) return Boolean is
   begin
      Focus.Register (Keys, ID);
      return Focus.Is_Focused (Keys, ID) and then Pending_Activate = ID;
   end Keyed;

   procedure Ring (C : Canvas; R : Rect; ID : Controls.Control_ID; Colors : Theme) is
   begin
      if Focus.Is_Focused (Keys, ID) then
         Draw_Focus_Ring (C, R, Colors);
      end if;
   end Ring;

   --  A row that acts as one button (pointer or Enter): toolkit hit testing.
   function Row_Pressed (R : Rect; ID : Controls.Control_ID) return Boolean is
      Result : Widget_Result;
   begin
      Controls.Add (Map, ID, R, R);
      Result := CuBit.UI.State.Button (UI, Controls.Bounds (Map, ID), CuBit.UI.State.Widget_ID (ID));
      if Result.activated then
         Focus.Set (Keys, ID);
      end if;
      return Result.activated or else Keyed (ID);
   end Row_Pressed;

   --  A toolkit text field showing State, focused by a click.
   procedure Field (C : Canvas; R : Rect; ID : Controls.Control_ID; State : Editor.Edit_State;
                    Live, Masked : Boolean; Colors : Theme) is
      Result : Widget_Result;
      Ignore : Boolean;
   begin
      Controls.Add (Map, ID, R, R, CuBit.UI.Pointer_Text);
      Result := CuBit.UI.State.Button (UI, Controls.Bounds (Map, ID), CuBit.UI.State.Widget_ID (ID));
      Focus.Register (Keys, ID);
      if Result.activated then
         Focus.Set (Keys, ID);
      end if;
      Ignore := Keyed (ID);
      Draw_Text_Edit_Field
        (C, R, Colors, Editor.Content (State),
         Editor.Cursor (State) - 1, Editor.Selection_First (State) - 1, Editor.Selection_Last (State) - 1,
         Focus.Is_Focused (Keys, ID) and then Live, Result.hot, masked => Masked,
         --  A Wi-Fi passphrase is only somewhat secret: show its length (user, 2026-11-05).
         mask => CuBit.UI.Per_Character);
   end Field;

   procedure Render
     (V : View; Notes : DN.History; Net : DNR.Layout; Report : NMR.Report;
      Networks : DNM.Network_List; Network_Count : DNM.Network_Count;
      Join : DNM.Join_State; Editor : DAE.Editor; Acts : in out Action_List)
   is
      Colors : constant Theme := Current_Theme;
      Rows : constant Natural := Height (Net, V.Note_Rows, V.System_Shown, V.Remote_Shown);
      OK : Boolean;
      C : Canvas;
      Full : Rect;
      Y : Natural := 0;
      Result : Widget_Result;

      function Row_Rect (H : Natural) return Rect is ((0, Y, Width, H));

      procedure Line (R : Rect) is
      begin
         Fill_Rect (C, (R.x, R.y, R.w, 1), Colors.edge);
      end Line;

      ------------------------------------------------------------------
      procedure Header is
         R : constant Rect := (0, Y, Width, DBS.Panel_Header);
      begin
         Draw_Emphasis_Text_Transparent (C, Pad, Text_Y (R), V.Clock, Colors.text);
         if V.Date.Length > 0 then
            Draw_UI_Text_Transparent
              (C, Width - Pad - UI_Text_Width (Image (V.Date)), Text_Y (R), Image (V.Date), Colors.muted);
         end if;
         Y := Y + DBS.Panel_Header;
      end Header;

      procedure Quick_Settings is
         R : Rect;
         Level : Natural := V.Level;
         Quiet : Boolean := V.Quiet;
         Mute_W : constant := 6 * M.Base_Em;
         Slider_X : constant Natural := Pad + Mute_W + DBS.Status_Item_Gap;
         Box : constant := M.Glyph_Box;
      begin
         Section (C, Row_Rect (DBS.Panel_Section), "Quick settings", Colors);
         Y := Y + DBS.Panel_Section;
         R := Row_Rect (DBS.Panel_Row);
         if V.Audio then
            declare
               B : constant Rect := (Pad, R.y + (R.h - DBS.Toggle_Height - 4) / 2, Mute_W, DBS.Toggle_Height + 4);
               S : constant Rect := (Slider_X, R.y + (R.h - DBS.Thumb_Height) / 2, DBS.Slider_Width, DBS.Thumb_Height);
            begin
               W.Button (C, UI, Map, ID_Mute, B, Full, Colors, (if V.Muted then "Unmute" else "Mute"), Result);
               if Result.activated or else Keyed (ID_Mute) then
                  Add (Acts, (Kind => Toggle_Mute, others => <>));
               end if;
               Ring (C, B, ID_Mute, Colors);
               W.Horizontal_Slider (C, UI, Map, ID_Volume, S, Full, Colors, 0, Percent'Last, Level, Result);
               if Keyed (ID_Volume) then
                  null;   --  Enter on the slider does nothing; Left/Right step it
               end if;
               if Focus.Is_Focused (Keys, ID_Volume) and then Pending_Step /= 0 then
                  Level := Natural (Integer'Max (0, Integer'Min (Percent'Last, Level + Pending_Step * Volume_Step)));
               end if;
               if Level /= V.Level then
                  Add (Acts, (Kind => Set_Volume, Level => Level, others => <>));
               end if;
               Ring (C, S, ID_Volume, Colors);
               Draw_UI_Text_Transparent (C, S.x + S.w + DBS.Status_Item_Gap, Text_Y (R),
                                         (if V.Muted then "Muted" else Count_Text (V.Level) & "%"), Colors.text);
            end;
         else
            Draw_UI_Text_Transparent (C, Pad, Text_Y (R), "Sound: no mixer", Colors.muted);
         end if;
         Y := Y + DBS.Panel_Row;
         R := Row_Rect (DBS.Panel_Row);
         Line (R);
         declare
            B : constant Rect := (Width - Pad - Box, R.y + (R.h - Box) / 2, Box, Box);
         begin
            Draw_UI_Text_Transparent (C, Pad, Text_Y (R), "Do not disturb", Colors.text);
            W.Checkbox (C, UI, Map, ID_Quiet, B, Full, Colors, Quiet, Result);
            if Keyed (ID_Quiet) then
               Quiet := not Quiet;
            end if;
            if Quiet /= V.Quiet then
               Add (Acts, (Kind => Set_Quiet, Flag => Quiet, others => <>));
            end if;
            Ring (C, (B.x - 4, B.y - 4, B.w + 8, B.h + 8), ID_Quiet, Colors);
         end;
         Y := Y + DBS.Panel_Row;
      end Quick_Settings;

      ------------------------------------------------------------------
      --  Remote access: who the remote session (ccl-remote) is paired
      --  with, and the person's "Allow screen capture" switch (off by
      --  default, off again when a new host pairs; Desktop_Capture_Policy).
      --  First in the panel, so active remote access is never out of view.
      ------------------------------------------------------------------
      procedure Remote_Section is
         R : Rect;
         Box : constant := 16;
         Allowed : Boolean := V.Capture_Allowed;
      begin
         Section (C, Row_Rect (DBS.Panel_Section), "Remote access", Colors);
         Y := Y + DBS.Panel_Section;
         R := Row_Rect (DBS.Panel_Row);
         Draw_UI_Text_Transparent
           (C, Pad, Text_Y (R),
            Fit_UI_Text ((if V.Remote_Paired then "Paired with " & Image (V.Remote_Peer)
                          else "Waiting for a host to pair"), Width - 2 * Pad),
            (if V.Remote_Paired then Colors.text else Colors.muted));
         Y := Y + DBS.Panel_Row;
         R := Row_Rect (DBS.Panel_Row);
         Line (R);
         declare
            B : constant Rect := (Width - Pad - Box, R.y + (R.h - Box) / 2, Box, Box);
         begin
            Draw_UI_Text_Transparent (C, Pad, Text_Y (R), "Allow screen capture", Colors.text);
            W.Checkbox (C, UI, Map, ID_Capture, B, Full, Colors, Allowed, Result);
            if Keyed (ID_Capture) then
               Allowed := not Allowed;
            end if;
            if Allowed /= V.Capture_Allowed then
               Add (Acts, (Kind => Set_Capture, Flag => Allowed, others => <>));
            end if;
            Ring (C, (B.x - 4, B.y - 4, B.w + 8, B.h + 8), ID_Capture, Colors);
         end;
         Y := Y + DBS.Panel_Row;
      end Remote_Section;

      ------------------------------------------------------------------
      --  The System section: a bar per CPU (busy since the last reading)
      --  and memory in use, from the kernel's CPU-times page.
      ------------------------------------------------------------------
      procedure System_Section is
         R : Rect;
         X : Natural;
         Label_X : constant Natural := Pad;
         Bars_X : constant Natural := Pad + Label_W;
         Bar_Ink : constant Color := Colors.accent;

         function Load_Text (L : CL.Load) return String is
           (if L.Known then Count_Text (L.Busy) & "%" else "--");
      begin
         Section (C, Row_Rect (DBS.Panel_Section), "System", Colors);
         Y := Y + DBS.Panel_Section;
         R := Row_Rect (DBS.Panel_Row);
         Draw_UI_Text_Transparent (C, Label_X, Text_Y (R), "CPU", Colors.text);
         X := Bars_X;
         for CPU in 0 .. V.CPU_Count - 1 loop
            exit when X + DLB.CPU_Bar_Width > Width - Pad - UI_Text_Width ("100%") - DBS.Status_Item_Gap;
            declare
               Top : constant Natural := R.y + (R.h - DLB.CPU_Bar_Height) / 2;
               L : constant CL.Load := V.CPUs (CPU);
               Fill : constant Natural :=
                 (if L.Known then DLB.Filled (DLB.CPU_Bar_Height, L.Busy) else 0);
            begin
               Fill_Rect (C, (X, Top, DLB.CPU_Bar_Width, DLB.CPU_Bar_Height), Colors.edge);
               if Fill > 0 then
                  Fill_Rect (C, (X, Top + DLB.CPU_Bar_Height - Fill, DLB.CPU_Bar_Width, Fill),
                             (if L.Busy > DLB.High_Percent then Colors.danger else Bar_Ink));
               end if;
            end;
            X := X + DLB.CPU_Bar_Width + DLB.CPU_Bar_Gap;
         end loop;
         Draw_UI_Text_Transparent
           (C, Width - Pad - UI_Text_Width (Load_Text (V.CPU_All)), Text_Y (R),
            Load_Text (V.CPU_All), Colors.text);
         Y := Y + DBS.Panel_Row;
         R := Row_Rect (DBS.Panel_Row);
         Line (R);
         Draw_UI_Text_Transparent (C, Label_X, Text_Y (R), "Memory", Colors.text);
         declare
            Top : constant Natural := R.y + (R.h - DLB.Memory_Bar_Height) / 2;
            Fill : constant Natural :=
              (if V.Memory_Known then DLB.Filled (DLB.Memory_Bar_Width, V.Memory_Used) else 0);
            Text : constant String := (if V.Memory_Known then Count_Text (V.Memory_Used) & "%" else "--");
         begin
            Fill_Rect (C, (Bars_X, Top, DLB.Memory_Bar_Width, DLB.Memory_Bar_Height), Colors.edge);
            if Fill > 0 then
               Fill_Rect (C, (Bars_X, Top, Fill, DLB.Memory_Bar_Height),
                          (if V.Memory_Used > DLB.High_Percent then Colors.danger else Bar_Ink));
            end if;
            Draw_UI_Text_Transparent (C, Width - Pad - UI_Text_Width (Text), Text_Y (R), Text, Colors.text);
         end;
         Y := Y + DBS.Panel_Row;
      end System_Section;

      ------------------------------------------------------------------
      --  The Network section (Desktop_Network_Rows order).
      ------------------------------------------------------------------
      Net_Indent : constant := M.Base_Em - M.Space_XS;
      Box_Inset : constant := M.Space_S;
      Toggle_W : constant := 5 * M.Base_Em + M.Space_S;
      Button_W : constant := 5 * M.Base_Em + M.Space_M;

      function Box_Of (R : Rect) return Rect is
        ((R.x + Pad + Label_W, R.y + Box_Inset, R.w - 2 * Pad - Label_W, R.h - 2 * Box_Inset));

      function Status_Of (S : NMR.Interface_Status) return String is
        ((case S.Condition is
            when CuBit.Net_Link.Up =>
              (if S.Settings.Address /= 0
               then DNA.Image (S.Settings.Address) & "/" & Count_Text (S.Settings.Prefix)
               elsif S.Setting = NMR.Automatic then "Waiting for DHCP"
               elsif S.Setting = NMR.Disabled then "Off" else "No address"),
            when CuBit.Net_Link.Dormant => "Connecting",
            when CuBit.Net_Link.Down =>
              (if S.Kind = CuBit.Net_Link.Ethernet then "Cable unplugged" else "Not connected"))
         & (case S.Setting is when NMR.Automatic => "  DHCP", when NMR.Static => "  Static",
                              when NMR.Disabled => ""));

      function Network_Status (N : DNM.Network) return String is
        (if Join.Current /= DNM.Idle and then DNM.Same (Join.Target, N.Name)
         then (if Join.Current = DNM.Connected and then V.Wifi_Address /= 0
               then "Connected " & DNA.Image (V.Wifi_Address) else DNM.Status_Text (Join))
         elsif N.Current and then V.Wifi_State = CuBit.Wifi_Control.Connected
         then (if V.Wifi_Address /= 0 then "Connected " & DNA.Image (V.Wifi_Address) else "Connected")
         elsif N.Current then "Joining"
         elsif N.Kind = DNM.Unsupported then "WEP/WPA1"
         elsif N.Remembered then "Saved" else "");

      procedure Net_Row (K : Positive; Row : DNR.Row) is
         R : constant Rect := Row_Rect (DNR.Height_Of (Row.Kind));
         Left : constant Natural := Pad;
         Glyph_Y : constant Natural := R.y + (R.h - Glyph_Size) / 2;
         Text_X : constant Natural := Left + Glyph_Size + DBS.Icon_Label_Gap;
         Right : constant Natural := Width - Pad;
         ID : constant Controls.Control_ID := ID_Row_Base + K;

         procedure Right_Text (S : String; Ink : Color; Before : Natural := 0) is
         begin
            if S'Length > 0 and then Right > UI_Text_Width (S) + Before then
               Draw_UI_Text_Transparent (C, Right - Before - UI_Text_Width (S), Text_Y (R), S, Ink);
            end if;
         end Right_Text;

         procedure Label (S : String) is
         begin
            Draw_UI_Text_Transparent (C, Left, Text_Y (R), S, Colors.muted);
         end Label;
      begin
         if Row.Kind in DNR.Wired_Row | DNR.Wifi_Row then
            Line (R);
         end if;
         case Row.Kind is
            when DNR.Wired_Row =>
               declare
                  S : NMR.Interface_Status renames Report.Interfaces (Row.Index);
               begin
                  if Row_Pressed (R, ID) then
                     Add (Acts, (Kind => Toggle_Editor, Index => Row.Index, others => <>));
                  end if;
                  Draw_Port (C, Left, Glyph_Y, S.Condition = CuBit.Net_Link.Up, Colors.text, Colors.muted);
                  Draw_Emphasis_Text_Transparent (C, Text_X, Text_Y (R), "Wired", Colors.text);
                  Draw_UI_Text_Transparent (C, Text_X + Emphasis_Text_Width ("Wired") + DBS.Icon_Label_Gap,
                                            Text_Y (R), S.Name.Text (1 .. S.Name.Length), Colors.muted);
                  Right_Text (Status_Of (S), Colors.text);
               end;
            when DNR.Wifi_Row =>
               if Row_Pressed (R, ID) and then Row.Index /= 0 then
                  Add (Acts, (Kind => Toggle_Editor, Index => Row.Index, others => <>));
               end if;
               Draw_Signal (C, Left, Glyph_Y, V.Wifi_Bars, Colors.text, Colors.edge);
               Draw_Emphasis_Text_Transparent (C, Text_X, Text_Y (R), "Wi-Fi", Colors.text);
               Right_Text ((if V.Wifi_State = CuBit.Wifi_Control.Connected
                            then Image (V.Joined)
                                 & (if V.Wifi_Address /= 0 then "  " & DNA.Image (V.Wifi_Address) else "")
                            elsif V.Wifi_State in CuBit.Wifi_Control.Joining | CuBit.Wifi_Control.Handshaking
                            then "Joining " & Image (V.Joined)
                            else "Not connected"), Colors.text);
            when DNR.Network_Row =>
               declare
                  N : constant DNM.Network := Networks (Row.Index);
                  X : constant Natural := Left + Net_Indent;
                  Name_X : constant Natural := X + Glyph_Size + DBS.Icon_Label_Gap;
                  Status : constant String := Network_Status (N);
                  Forget_Rect : constant Rect :=
                    (Right - Button_W, R.y + 2, Button_W, (if R.h > 4 then R.h - 4 else 0));
                  Forget_W : constant Natural := (if N.Remembered then Button_W + DBS.Status_Item_Gap else 0);
                  Room : constant Natural :=
                    Natural'Max (0, Integer (Right) - Integer (Name_X) - Forget_W - UI_Text_Width (Status)
                                    - Lock_Width - 3 * DBS.Status_Item_Gap);
                  Name : constant String := Fit_UI_Text (DNM.Text_Of (N.Name), Room);
                  Ignore : Boolean;
               begin
                  if N.Remembered then
                     W.Button (C, UI, Map, ID_Forget_Base + K, Forget_Rect, Full, Colors, "Forget", Result,
                               quiet => True);
                     if Result.activated or else Keyed (ID_Forget_Base + K) then
                        Add (Acts, (Kind => Forget_Network, Index => Row.Index, others => <>));
                     end if;
                  end if;
                  if Row_Pressed ((R.x, R.y, (if N.Remembered then Forget_Rect.x else R.w), R.h), ID) then
                     Add (Acts, (Kind => Choose_Network, Index => Row.Index, others => <>));
                  end if;
                  Draw_Signal (C, X, Glyph_Y, DNM.Bars_Of (N.Signal), Colors.text, Colors.edge);
                  if N.Current then
                     Draw_Emphasis_Text_Transparent (C, Name_X, Text_Y (R), Name, Colors.text);
                  else
                     Draw_UI_Text_Transparent (C, Name_X, Text_Y (R), Name,
                                               (if N.Kind = DNM.Unsupported then Colors.muted else Colors.text));
                  end if;
                  if N.Kind = DNM.Secured then
                     Draw_Lock (C, Name_X + (if N.Current then Emphasis_Text_Width (Name) else UI_Text_Width (Name))
                                + DBS.Status_Item_Gap, Text_Y (R) + 2, Colors.muted);
                  end if;
                  Right_Text (Status,
                              (if Join.Current in DNM.Failed | DNM.Entering_Passphrase
                                 and then Join.Why /= DNM.None and then DNM.Same (Join.Target, N.Name)
                               then Colors.danger else Colors.muted),
                              Forget_W);
                  if N.Remembered then
                     Ring (C, Forget_Rect, ID_Forget_Base + K, Colors);
                  end if;
               end;
            when DNR.Passphrase_Row =>
               Label ("Passphrase");
               Field (C, Box_Of (R), ID_Passphrase, Secret, True, True, Colors);
            when DNR.Method_Row =>
               Label ("Addressing");
               declare
                  Chosen : Natural := DAE.Method'Pos (Editor.Chosen);
                  Keyed_Here : constant Boolean := Keyed (ID_Method);
               begin
                  for M in DAE.Method loop
                     W.Radio_Button
                       (C, UI, Map, ID_Method + 1 + DAE.Method'Pos (M),
                        (R.x + Pad + Label_W + DAE.Method'Pos (M) * Toggle_W, R.y + Box_Inset, Toggle_W,
                         R.h - 2 * Box_Inset), Full, Colors,
                        (case M is when DAE.Automatic => "DHCP", when DAE.Static => "Static",
                                   when DAE.Disabled => "Off"),
                        DAE.Method'Pos (M), Chosen, Result);
                  end loop;
                  if Focus.Is_Focused (Keys, ID_Method) and then (Pending_Step /= 0 or else Keyed_Here) then
                     Chosen := (DAE.Method'Pos (Editor.Chosen) + (if Pending_Step < 0 then 2 else 1))
                       mod (DAE.Method'Pos (DAE.Method'Last) + 1);
                  end if;
                  if Chosen /= DAE.Method'Pos (Editor.Chosen) then
                     Add (Acts, (Kind => Choose_Method, Method => DAE.Method'Val (Chosen), others => <>));
                  end if;
                  Ring (C, (R.x + Pad + Label_W, R.y, 3 * Toggle_W, R.h), ID_Method, Colors);
               end;
            when DNR.Address_Row | DNR.Gateway_Row | DNR.DNS_Row =>
               declare
                  F : constant DAE.Field_Name :=
                    (case Row.Kind is when DNR.Gateway_Row => DAE.Gateway_Field,
                                      when DNR.DNS_Row => DAE.DNS_Field, when others => DAE.Address_Field);
               begin
                  Label ((case F is when DAE.Address_Field => "Address", when DAE.Gateway_Field => "Gateway",
                                    when DAE.DNS_Field => "DNS (all)"));
                  Fields_Editable (F) := DAE.Editable (Editor, F);
                  Field (C, Box_Of (R), Field_ID (F), Fields (F), Fields_Editable (F), False, Colors);
               end;
            when DNR.CCL_Row =>
               declare
                  CCL : constant String := DAE.Shown_CCL (Editor);
                  Room : constant Natural := Width - 2 * Pad;
                  Kept : constant Natural := Kept_Code_Text (CCL, Room);
                  Gap : constant := 2;
               begin
                  if CCL'Length = 0 then
                     Draw_UI_Text_Transparent (C, Left, Text_Y (R), DAE.Result_Text (Editor), Colors.danger);
                  else
                     --  Two lines: as much as fits, then the rest (cut).
                     declare
                        First_Last : constant Natural :=
                          (if Kept >= CCL'Length then CCL'Last else CCL'First + Natural'Max (Kept, 1) - 1);
                     begin
                        Draw_Code_Text_Transparent
                          (C, Left, R.y + Gap, CCL (CCL'First .. First_Last), Colors.muted);
                        if First_Last < CCL'Last then
                           Draw_Code_Text_Transparent
                             (C, Left, R.y + Gap + Code_Text_Height + Gap,
                              Fit_Code_Text (CCL (First_Last + 1 .. CCL'Last), Room), Colors.muted);
                        end if;
                     end;
                  end if;
               end;
            when DNR.Action_Row =>
               declare
                  A : constant Rect := (R.x + Pad + Label_W, R.y + Box_Inset, Button_W, R.h - 2 * Box_Inset);
                  B : constant Rect := (A.x + Button_W + DBS.Button_Gap, A.y, Button_W, A.h);
                  Text_X0 : constant Natural := B.x + B.w + DBS.Status_Item_Gap;
               begin
                  W.Button (C, UI, Map, ID_Apply, A, Full, Colors, "Apply", Result);
                  if Result.activated or else Keyed (ID_Apply) then
                     Add (Acts, (Kind => Apply, others => <>));
                  end if;
                  Ring (C, A, ID_Apply, Colors);
                  if Editor.Can_Revert then
                     W.Button (C, UI, Map, ID_Revert, B, Full, Colors, "Revert", Result);
                     if Result.activated or else Keyed (ID_Revert) then
                        Add (Acts, (Kind => Revert, others => <>));
                     end if;
                     Ring (C, B, ID_Revert, Colors);
                  else
                     W.Disabled_Button (C, B, Colors, "Revert");
                  end if;
                  if Right > Text_X0 then
                     Draw_UI_Text_Transparent
                       (C, Text_X0, Text_Y (R), Fit_UI_Text (DAE.Result_Text (Editor), Right - Text_X0),
                        (if Editor.Problem /= DNA.None or else Editor.Result = DAE.Refused
                         then Colors.danger else Colors.muted));
                  end if;
               end;
         end case;
         if Row.Kind in DNR.Wired_Row | DNR.Wifi_Row | DNR.Network_Row then
            Ring (C, R, ID, Colors);
         end if;
         Y := Y + R.h;
      end Net_Row;

      procedure Network is
      begin
         if Net.Count = 0 then
            return;
         end if;
         Section (C, Row_Rect (DBS.Panel_Section), "Network", Colors);
         Y := Y + DBS.Panel_Section;
         for K in 1 .. Net.Count loop
            Net_Row (K, Net.Rows (K));
         end loop;
      end Network;

      ------------------------------------------------------------------
      procedure Notifications is
         Total : constant Natural := Natural (DN.Length (Notes));
         H : constant Rect := Row_Rect (DBS.Panel_Section);
         Clear_W : constant := 6 * M.Base_Em;
      begin
         Section (C, H, (if Total = 0 then "Notifications" else "Notifications (" & Count_Text (Total) & ")"),
                  Colors);
         if Total > 0 then
            declare
               B : constant Rect := (Width - Pad - Clear_W, H.y + 2, Clear_W, H.h - 4);
            begin
               W.Button (C, UI, Map, ID_Clear, B, Full, Colors, "Clear all", Result, quiet => True);
               if Result.activated or else Keyed (ID_Clear) then
                  Add (Acts, (Kind => Clear_Notes, others => <>));
               end if;
               Ring (C, B, ID_Clear, Colors);
            end;
         end if;
         Y := Y + DBS.Panel_Section;
         if V.Note_Rows = 0 then
            Draw_UI_Text_Transparent (C, Pad, Text_Y (Row_Rect (DBS.Panel_Row)), "No notifications", Colors.muted);
            Y := Y + DBS.Panel_Row;
            return;
         end if;
         for K in 1 .. Natural'Min (V.Note_Rows, Total) loop
            declare
               N : constant DN.Notification := DN.Item (Notes, DN.Position (K));
               R : constant Rect := Row_Rect (DBS.Panel_Note_Row);
               Close : constant Rect :=
                 (Width - Pad - DBS.Close_Box - 6, R.y + (R.h - DBS.Close_Box - 6) / 2,
                  DBS.Close_Box + 6, DBS.Close_Box + 6);
               Text_X : constant Natural := Pad + Glyph_Size + DBS.Icon_Label_Gap;
               Stamp : constant String := (if N.Posted_At.Known then Minute_Text (N.Posted_At.Minute) else "");
               Room : constant Natural :=
                 Natural'Max (0, Integer (Close.x) - Integer (Text_X) - DBS.Status_Item_Gap);
               Title_Room : constant Natural :=
                 Natural'Max (0, Integer (Room) - UI_Text_Width (Stamp) - DBS.Status_Item_Gap);
               Top : constant Natural := R.y + (R.h - 2 * UI_Text_Height - M.Space_XS) / 2;
               ID : constant Controls.Control_ID := ID_Note_Base + K;
            begin
               Line (R);
               Fill_Rect (C, (R.x, R.y + 1, DBS.Stripe_Height, R.h - 1),
                          (if N.Accent.Declared then Color (N.Accent.Value) else Colors.accent));
               Draw_Icon (C, N.Icon, Pad, Top, Colors.face);
               Draw_Emphasis_Text_Transparent (C, Text_X, Top, Fit_UI_Text (DN.Title_Of (N), Title_Room),
                                               (if N.Level = DN.Critical then Colors.danger else Colors.text));
               if Stamp'Length > 0 then
                  Draw_UI_Text_Transparent (C, Close.x - DBS.Status_Item_Gap - UI_Text_Width (Stamp), Top, Stamp,
                                            Colors.muted);
               end if;
               Draw_UI_Text_Transparent (C, Text_X, Top + UI_Text_Height + M.Space_XS, Fit_UI_Text (DN.Detail_Of (N), Room),
                                         Colors.muted);
               W.Button (C, UI, Map, ID, Close, Full, Colors, "x", Result, quiet => True);
               if Result.activated or else Keyed (ID) then
                  Add (Acts, (Kind => Dismiss_Note, Note => N.Id, others => <>));
               end if;
               Ring (C, Close, ID, Colors);
               Y := Y + DBS.Panel_Note_Row;
            end;
         end loop;
      end Notifications;
   begin
      Reserve (Rows, OK);
      if not OK then
         Rows_Drawn := 0;
         return;
      end if;
      Rows_Drawn := Rows;
      C := (addr => Store, width => Width, height => Rows, pitch => Width * Bytes_Per_Pixel, others => <>);
      Full := (0, 0, Width, Rows);
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      Focus.Begin_Order (Keys);

      Fill_Rect (C, Full, Colors.face);
      Stroke_Rect (C, Full, Colors.edge, Colors.edge);
      Fill_Rect (C, (0, 0, Width, DBS.Panel_Stripe), Colors.accent);
      Y := DBS.Panel_Stripe;
      Header;
      if V.Remote_Shown then
         Remote_Section;
      end if;
      Quick_Settings;
      if V.System_Shown then
         System_Section;
      end if;
      Network;
      Notifications;
      Draw_UI_Text_Transparent (C, Pad, Text_Y ((0, Y, Width, DBS.Panel_Footer)),
                                "Esc closes   Tab moves   Enter acts", Colors.muted);

      Focus.End_Order (Keys);
      if Pending_Focus /= Controls.NO_CONTROL then
         Focus.Set (Keys, Pending_Focus);
         Pending_Focus := Controls.NO_CONTROL;
      end if;
      CuBit.UI.State.Finish_Frame (UI);
      Pending_Activate := Controls.NO_CONTROL;
      Pending_Step := 0;
   end Render;

   procedure Pointer (X, Y : Natural; Down, Pressed, Released : Boolean) is
   begin
      CuBit.UI.State.Set_Pointer (UI, X, Y, Down, Pressed, Released);
   end Pointer;

   procedure Opened is
   begin
      Focus.Clear (Keys);
   end Opened;

   procedure Focus_Passphrase is
   begin
      Pending_Focus := ID_Passphrase;
   end Focus_Passphrase;

   ---------------------------------------------------------------------------
   --  Keys
   ---------------------------------------------------------------------------
   KEY_ESCAPE    : constant Unsigned_8 := 16#01#;
   KEY_BACKSPACE : constant Unsigned_8 := 16#0E#;
   KEY_TAB       : constant Unsigned_8 := 16#0F#;
   KEY_ENTER     : constant Unsigned_8 := 16#1C#;
   KEY_SPACE     : constant Unsigned_8 := 16#39#;
   KEY_UP        : constant Unsigned_8 := 16#48#;
   KEY_LEFT      : constant Unsigned_8 := 16#4B#;
   KEY_RIGHT     : constant Unsigned_8 := 16#4D#;
   KEY_DOWN      : constant Unsigned_8 := 16#50#;
   KEY_DELETE    : constant Unsigned_8 := 16#53#;

   procedure Key (Code : Unsigned_8; Ch : Character; Shift : Boolean;
                  Acts : in out Action_List; Handled : out Boolean) is
      Focused : constant Controls.Control_ID := Keys.Focused;
      Changed : Boolean := False;

      function Field_Of (ID : Controls.Control_ID; F : out DAE.Field_Name) return Boolean is
      begin
         F := DAE.Address_Field;
         for Name in DAE.Field_Name loop
            if Field_ID (Name) = ID then
               F := Name;
               return True;
            end if;
         end loop;
         return False;
      end Field_Of;

      F : DAE.Field_Name;
      In_Field : constant Boolean := Field_Of (Focused, F);
   begin
      Handled := True;
      if Code = KEY_TAB or else Code = KEY_DOWN or else Code = KEY_UP then
         Focus.Move (Keys, Forward => (Code = KEY_DOWN or else (Code = KEY_TAB and then not Shift)));
      elsif Focused = ID_Passphrase then
         if Code = KEY_ESCAPE then
            Add (Acts, (Kind => Cancel_Passphrase, others => <>));
         elsif Code = KEY_ENTER then
            Add (Acts, (Kind => Enter_Passphrase, others => <>));
         elsif Code = KEY_BACKSPACE then
            Editor.Backspace (Secret, Changed);
         elsif Code = KEY_LEFT then
            Editor.Move (Secret, Editor.Move_Left);
         elsif Code = KEY_RIGHT then
            Editor.Move (Secret, Editor.Move_Right);
         elsif Ch in ' ' .. '~' then
            Editor.Insert (Secret, [Ch], Changed);
         end if;
      elsif In_Field then
         if Code = KEY_ESCAPE then
            Add (Acts, (Kind => Close_Editor, others => <>));
         elsif Code = KEY_ENTER then
            Add (Acts, (Kind => Apply, others => <>));
         elsif Code = KEY_LEFT then
            Editor.Move (Fields (F), Editor.Move_Left);
         elsif Code = KEY_RIGHT then
            Editor.Move (Fields (F), Editor.Move_Right);
         elsif not Fields_Editable (F) then
            null;
         elsif Code = KEY_BACKSPACE then
            Editor.Backspace (Fields (F), Changed);
         elsif DAE.Accepts (Ch) and then Editor.Length (Fields (F)) < DNA.Maximum_Field then
            Editor.Insert (Fields (F), [Ch], Changed);
         end if;
         if Changed then
            Add (Acts, (Kind => Field_Edited, Field => F, others => <>));
         end if;
      elsif Code = KEY_ESCAPE then
         if Focused in ID_Method | ID_Apply | ID_Revert then
            Add (Acts, (Kind => Close_Editor, others => <>));
         else
            Handled := False;
         end if;
      elsif Code in KEY_ENTER | KEY_SPACE then
         Pending_Activate := Focused;
      elsif Code = KEY_LEFT or else Code = KEY_RIGHT then
         Pending_Step := (if Code = KEY_LEFT then -1 else 1);
      elsif Code = KEY_DELETE and then Focused > ID_Row_Base and then Focused <= ID_Row_Base + DNR.Maximum_Rows then
         --  Delete on a network row forgets it (its Forget button's ID).
         Pending_Activate := Focused - ID_Row_Base + ID_Forget_Base;
         Focus.Set (Keys, Pending_Activate);
      else
         Handled := False;
      end if;
   end Key;

   ---------------------------------------------------------------------------
   --  Fields
   ---------------------------------------------------------------------------
   function Passphrase return access CuBit.UI.Editor.Edit_State is
   begin
      if not Secret_Ready then
         declare
            Accepted : Boolean;
         begin
            Editor.Initialize (Secret, "", Accepted, Kind => Editor.Sensitive);
            Secret_Ready := True;
         end;
      end if;
      return Secret'Access;
   end Passphrase;

   procedure Wipe_Passphrase is
   begin
      Editor.Wipe (Passphrase.all);
   end Wipe_Passphrase;

   procedure Load_Fields (E : DAE.Editor) is
      Accepted : Boolean;
   begin
      for F in DAE.Field_Name loop
         Editor.Initialize (Fields (F), DAE.Text (E.Fields (F)), Accepted);
         Fields_Editable (F) := DAE.Editable (E, F);
      end loop;
      Fields_Ready := True;
   end Load_Fields;

   function Field_Content (F : DAE.Field_Name) return String is
     (if Fields_Ready then Editor.Content (Fields (F)) else "");
end Desktop_Status_Panel;
