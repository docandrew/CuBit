with System.Storage_Elements; use System.Storage_Elements;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Editor;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Widgets;
with CuBit.UI.Input;
with CuBit.UI.Menus;
with Client_Canvas_Geometry;
with Servo_Frame_Copy;
with Client_Input_Budget;
with Servo_Input_Geometry;
with CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Config;
with Servo_Tabs;
with Servo_Bookmarks;
with Penny_Artwork;
with Servo_Tab_Geometry;

package body Servo_Session is
   package App renames CuBit.UI.App;
   package Editor renames CuBit.UI.Editor;
   package Geometry renames Client_Canvas_Geometry;
   use type System.Address;
   Win : App.Window;
   Address_Edit : Editor.Edit_State;
   Current_URL : String (1 .. Editor.MAX_TEXT_LENGTH) := [others => ' '];
   URL_Last : Natural := 0;
   Focused, Chrome_Dirty, Busy, Can_Back, Can_Forward : Boolean := False;
   Address_Too_Long, Invalid_Address, Chrome_Press : Boolean := False;
   Address_Input_Lost : Boolean := False;
   Focus_On_State : Boolean := False;
   Paint_Open : Boolean := False;
   First_Shown : Natural := 0;
   First_Tab : Natural := 0;
   Window_Limit : Boolean := False;
   package Menus renames CuBit.UI.Menus;
   Menu_State : Menus.Menu_State;
   Menu_Controls : CuBit.UI.Controls.Control_Map;
   Menu_UI : CuBit.UI.State.UI_State;
   Menu_Interaction : App.Pointer_Interaction;
   Menu_Block_Release, Menu_Key_Held : Boolean := False;
   Menu_Height : constant := 24;
   Bookmark_Dialog : Servo_Bookmarks.Dialog;
   Settings_Open : Boolean := False;
   About_Open : Boolean := False;
   Settings_Focus : Positive range 1 .. 2 := 1;
   Settings_Controls : CuBit.UI.Controls.Control_Map;
   Settings_UI : CuBit.UI.State.UI_State;
   Settings_Interaction : App.Pointer_Interaction;
   Held_Buttons : Unsigned_64 := 0;
   package Controls renames CuBit.UI.Controls;
   package Widgets renames CuBit.UI.Widgets;
   Chrome_Controls : Controls.Control_Map;
   Chrome_UI : CuBit.UI.State.UI_State;
   Chrome_Interaction : App.Pointer_Interaction;
   Controls_Stale : Boolean := False;
   package Tabs renames Servo_Tabs;
   use type Tabs.Phase;
   Tab_State : Tabs.State;
   Vertical_Tabs, Tab_Limit, Preference_Error : Boolean := False;
   Tab_Titles : array (Tabs.Slot) of String (1 .. 64) := [others => [others => ' ']];
   Tab_Lengths : array (Tabs.Slot) of Natural range 0 .. 64 := [others => 0];
   Preference_Key : constant String := "browser.servo.vertical-tabs";
   Toolbar_Height : constant := 40;
   Status_Height : constant := 24;
   Max_Bytes : constant := 16 * 1_024 * 1_024;
   Input_Batch : Client_Input_Budget.Batch := Client_Input_Budget.Open (0);

   M_0 : aliased constant String := "File";
   M_1 : aliased constant String := "Edit";
   M_2 : aliased constant String := "View";
   M_3 : aliased constant String := "New tab";
   M_4 : aliased constant String := "New window";
   M_5 : aliased constant String := "Close tab";
   M_6 : aliased constant String := "Close window";
   M_7 : aliased constant String := "Select address";
   M_8 : aliased constant String := "Settings...";
   M_9 : aliased constant String := "Back";
   M_10 : aliased constant String := "Forward";
   M_11 : aliased constant String := "Reload";
   M_12 : aliased constant String := "Previous tab";
   M_13 : aliased constant String := "Next tab";
   M_14 : aliased constant String := "Ctrl+T";
   M_15 : aliased constant String := "Ctrl+N";
   M_16 : aliased constant String := "Ctrl+W";
   M_17 : aliased constant String := "Ctrl+Shift+W";
   M_18 : aliased constant String := "Ctrl+L";
   M_19 : aliased constant String := "Alt+Left";
   M_20 : aliased constant String := "Alt+Right";
   M_21 : aliased constant String := "Ctrl+R";
   M_22 : aliased constant String := "Ctrl+Shift+Tab";
   M_23 : aliased constant String := "Ctrl+Tab";
   BM_Menu : aliased constant String := "Bookmarks";
   BM_Add : aliased constant String := "Bookmark this page...";
   BM_Manage : aliased constant String := "Manage bookmarks...";
   BM_Add_Key : aliased constant String := "Ctrl+D";
   BM_Manage_Key : aliased constant String := "Ctrl+Shift+B";
   Help_Menu : aliased constant String := "Help";
   About_Item : aliased constant String := "About Penny...";
   function Menu_Model return Menus.Model is
      M : Menus.Model;
   begin
      M.Menu_Count := 5; M.Item_Count := 17;
      M.Menus (1) := (M_0'Access, 'f');
      M.Menus (2) := (M_1'Access, 'e');
      M.Menus (3) := (M_2'Access, 'v');
      M.Menus (4) := (BM_Menu'Access, 'b');
      M.Menus (5) := (Help_Menu'Access, 'h');
      M.Items (1) := (1, M_3'Access, M_14'Access, 'n', 5, True, False, False);
      M.Items (2) := (1, M_4'Access, M_15'Access, 'w', 9, True, False, False);
      M.Items (3) := (1, null, null, ' ', 0, True, False, True);
      M.Items (4) := (1, M_5'Access, M_16'Access, 't', 10, True, False, False);
      M.Items (5) := (1, M_6'Access, M_17'Access, 'c', 11, True, False, False);
      M.Items (6) := (2, M_7'Access, M_18'Access, 'a', 12, True, False, False);
      M.Items (7) := (2, null, null, ' ', 0, True, False, True);
      M.Items (8) := (2, M_8'Access, null, 's', 6, True, False, False);
      M.Items (9) := (3, M_9'Access, M_19'Access, 'b', 1, True, False, False);
      M.Items (10) := (3, M_10'Access, M_20'Access, 'f', 2, True, False, False);
      M.Items (11) := (3, M_11'Access, M_21'Access, 'r', 3, True, False, False);
      M.Items (12) := (3, null, null, ' ', 0, True, False, True);
      M.Items (13) := (3, M_12'Access, M_22'Access, 'p', 7, True, False, False);
      M.Items (14) := (3, M_13'Access, M_23'Access, 'n', 8, True, False, False);
      M.Items (15) := (4, BM_Add'Access, BM_Add_Key'Access, 'b', 13, True, False, False);
      M.Items (16) := (4, BM_Manage'Access, BM_Manage_Key'Access, 'm', 14, True, False, False);
      M.Items (17) := (5, About_Item'Access, null, 'a', 15, True, False, False);
      M.Items (9).Enabled := Can_Back;
      M.Items (10).Enabled := Can_Forward;
      return M;
   end Menu_Model;

   function Now return Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));

   procedure Begin_Input is
   begin Input_Batch := Client_Input_Budget.Open (Now); end Begin_Input;

   function Toolbar return Rect is ((0, Menu_Height, App.Width (Win), Toolbar_Height));
   function Address_Area return Rect is
     ((228, 31, (if App.Width (Win) > 236 then App.Width (Win) - 236 else 0), 26));
   function Page_Area return Rect is
      P : constant Servo_Tab_Geometry.Rectangle := Servo_Tab_Geometry.Page
        (Natural'Min (App.Width (Win), 65_535), Natural'Min (App.Height (Win), 65_535), Vertical_Tabs);
   begin return (P.X, P.Y, P.W, P.H); end Page_Area;
   function Page_Height return Natural is (Page_Area.h);
   function New_Button return Rect is ((8, 68, 28, 28));
   function Previous_Tab_Button return Rect is
     ((if Vertical_Tabs then 116 else App.Width (Win) - 64), 68, 26, 28);
   function Next_Tab_Button return Rect is
     ((if Vertical_Tabs then 146 else App.Width (Win) - 34), 68, 26, 28);
   function Visible_Tabs return Natural is
     (Servo_Tab_Geometry.Visible
       (Natural'Max (800, Natural'Min (65_535, App.Width (Win))),
        Natural'Min (65_535, App.Height (Win)), Vertical_Tabs,
        Servo_Tab_Geometry.Tab_Count (Natural'Max (1, Tabs.Count (Tab_State)))));
   function Tab_Rank (I : Tabs.Slot) return Natural is
      Rank : Natural := 0;
   begin
      for J in Tabs.Slot loop
         exit when J = I;
         if Tab_State.Items (J) = Tabs.Live then Rank := Rank + 1; end if;
      end loop;
      return Rank;
   end Tab_Rank;
   function Tab_Box (I : Tabs.Slot) return Rect is
      B : constant Servo_Tab_Geometry.Rectangle := Servo_Tab_Geometry.Tab
        (Natural'Max (800, Natural'Min (65_535, App.Width (Win))),
         Tab_Rank (I) - First_Tab, Vertical_Tabs,
         Servo_Tab_Geometry.Tab_Count (Visible_Tabs));
   begin return (B.X, B.Y, B.W, B.H); end Tab_Box;
   procedure Read_Preference is
      Data : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.get (Preference_Key, Data, Length, Status);
      if Status = CuBit.Config.OK and then Length = 1 and then Data /= System.Null_Address then
         declare Value : Character with Import, Address => Data;
         begin Vertical_Tabs := Value = '1'; end;
      end if;
   end Read_Preference;
   procedure Toggle_Layout is
      Value : aliased Character;
      Status : CuBit.Config.ConfigStatus;
      use type CuBit.Config.ConfigStatus;
   begin
      Vertical_Tabs := not Vertical_Tabs;
      Value := (if Vertical_Tabs then '1' else '0');
      CuBit.Config.set (Preference_Key, Value'Address, 1, Status);
      Preference_Error := Status /= CuBit.Config.OK;
      Chrome_Dirty := True;
   end Toggle_Layout;

   procedure Metrics (Result : access Viewport) is
      C : constant Canvas := App.Canvas (Win);
   begin
      if Result = null then return; end if;
      Result.all := (others => 0);
      if not App.Is_Open (Win) or else C.width > Geometry.Logical_Edge'Last or else
        C.height > Geometry.Logical_Edge'Last or else Page_Height = 0
      then return; end if;
      Result.all :=
        (Unsigned_32 (Geometry.Relative (Page_Area.x, Page_Area.w, C.densityNumerator, C.densityDenominator)),
         Unsigned_32 (Geometry.Relative (Page_Area.y, Page_Height,
           C.densityNumerator, C.densityDenominator)),
         Unsigned_32 (C.densityNumerator), Unsigned_32 (C.densityDenominator));
   end Metrics;

   function Open return Unsigned_32 is
      OK : Boolean;
      Fresh_Interaction : App.Pointer_Interaction;
   begin
      App.Open (Win, 800, 600,
        App.WINDOW_FLAG_DECORATED or App.WINDOW_FLAG_RESIZABLE or
        App.WINDOW_FLAG_MINIMIZABLE or App.WINDOW_FLAG_MAXIMIZABLE or
        App.WINDOW_FLAG_CLOSEABLE or CuBit.Desktop_Protocol.Feature_Bits
          ([CuBit.Desktop_Protocol.Graceful_Close => True, others => False]),
        OK, title => "Penny", protected_frames => True);
      if not OK then return 0; end if;
      Editor.Initialize (Address_Edit, "", OK);
      Read_Preference;
      Tab_State := (others => <>); First_Tab := 0;
      Tab_Lengths := (others => 0); Tab_Titles := (others => (others => ' '));
      Chrome_Controls := (others => <>); Chrome_UI := (others => <>);
      Chrome_Interaction := Fresh_Interaction; Controls_Stale := False;
      Window_Limit := False; Tab_Limit := False; Preference_Error := False;
      Menus.Dismiss (Menu_State); Menu_Controls := (others => <>);
      Menu_UI := (others => <>); Menu_Interaction := Fresh_Interaction;
      Menu_Block_Release := False; Menu_Key_Held := False;
      Settings_Open := False; About_Open := False; Settings_Focus := 1;
      Settings_Controls := (others => <>); Settings_UI := (others => <>);
      Settings_Interaction := Fresh_Interaction;
      Busy := False; Can_Back := False; Can_Forward := False;
      URL_Last := 0; First_Shown := 0; Current_URL := (others => ' ');
      Address_Too_Long := False; Invalid_Address := False;
      Address_Input_Lost := False; Focus_On_State := False; Chrome_Press := False;
      Focused := True; Chrome_Dirty := True; Held_Buttons := 0;
      return 1;
   end Open;

   procedure Load_Address is
      OK : Boolean;
   begin
      Editor.Initialize (Address_Edit, Current_URL (1 .. URL_Last), OK);
      First_Shown := 0; Address_Input_Lost := False;
   end Load_Address;

   procedure Focus_Address is
   begin
      if not Focused or else Address_Input_Lost then Load_Address; end if;
      Focused := True;
      Editor.Select_All (Address_Edit);
      Chrome_Dirty := True;
   end Focus_Address;

   procedure Navigation_Error is
   begin
      Invalid_Address := True; Focused := True; Chrome_Dirty := True;
   end Navigation_Error;

   procedure Reveal_Cursor is
      Text : constant String := Editor.Content (Address_Edit);
      Cursor : constant Natural := Editor.Cursor (Address_Edit) - 1;
      Room : constant Natural := Natural'Max (1, Address_Area.w - 20);
   begin
      First_Shown := Natural'Min (First_Shown, Cursor);
      while First_Shown < Cursor and then
        UI_Text_Width (Text (1 + First_Shown .. Cursor)) > Room
      loop First_Shown := First_Shown + 1; end loop;
   end Reveal_Cursor;

   function Location (Text : System.Address; Capacity : Unsigned_32) return Unsigned_32 is
      Value : constant String := Editor.Content (Address_Edit);
   begin
      if Text = System.Null_Address or else Capacity < Unsigned_32 (Value'Length)
      then return 0; end if;
      declare
         Output : String (1 .. Value'Length) with Import, Address => Text;
      begin Output := Value; end;
      return Unsigned_32 (Value'Length);
   end Location;

   procedure State
     (URL : System.Address; URL_Length : Unsigned_32;
      Title : System.Address; Title_Length : Unsigned_32;
      Flags : Unsigned_32)
   is
   begin
      if not App.Is_Open (Win) then return; end if;
      -- Fixed bounds: the trusted Rust caller clamps at a UTF-8 boundary.
      -- UI's current editor is ASCII; displayed non-ASCII bytes are replaced.
      Address_Too_Long := URL_Length > Current_URL'Length;
      if Address_Too_Long then
         URL_Last := 0;
         if not Focused then Load_Address; end if;
      elsif URL /= System.Null_Address then
         declare
            Input : String (1 .. Natural (URL_Length)) with Import, Address => URL;
         begin
            URL_Last := Input'Length;
            for I in Input'Range loop
               Current_URL (I) := (if Input (I) in ' ' .. '~' then Input (I) else '?');
            end loop;
         end;
         if not Focused then Load_Address; end if;
      end if;
      if Title /= System.Null_Address and then Title_Length <= 256 then
         declare
            Input : String (1 .. Natural (Title_Length)) with Import, Address => Title;
            Safe : String (Input'Range);
         begin
            for I in Input'Range loop
               Safe (I) := (if Input (I) in ' ' .. '~' then Input (I) else '?');
            end loop;
            App.Set_Title (Win, Safe & " - Penny");
         end;
      end if;
      Busy := (Flags and 1) /= 0;
      Invalid_Address := False;
      Can_Back := (Flags and 2) /= 0;
      Can_Forward := (Flags and 4) /= 0;
      if Focus_On_State then Focus_On_State := False; Focus_Address; end if;
      Chrome_Dirty := True;
   end State;

   procedure Tab_Title (Index : Unsigned_32; Text : System.Address; Length : Unsigned_32) is
      I : Tabs.Slot;
   begin
      if Index not in 1 .. Tabs.Capacity or else Length > 64 or else Text = System.Null_Address then return; end if;
      I := Tabs.Slot (Index);
      declare
         Input : String (1 .. Natural (Length)) with Import, Address => Text;
         Safe : String (Input'Range);
      begin
         for J in Input'Range loop
            Safe (J) := (if Input (J) in ' ' .. '~' then Input (J) else '?');
         end loop;
         if Tab_Lengths (I) = Safe'Length and then Tab_Titles (I) (1 .. Safe'Length) = Safe then return; end if;
         Tab_Lengths (I) := Safe'Length;
         Tab_Titles (I) (1 .. Safe'Length) := Safe;
      end;
      Chrome_Dirty := True;
   end Tab_Title;
   procedure Tab_Parked (Index : Unsigned_32) is
   begin
      if Index in 1 .. Tabs.Capacity then
         Tabs.Parked (Tab_State, Tabs.Slot (Index));
         Tab_Limit := False; Chrome_Dirty := True;
      end if;
   end Tab_Parked;
   procedure Tab_Action (Control : Natural; Result : access Event) is
      Added : Tabs.Selection;
      Old : constant Tabs.Selection := Tab_State.Active;
   begin
      Result.Kind := 22;
      if Control = 5 then
         Tabs.Open_Tab (Tab_State, Added);
         Tab_Limit := Added = 0;
         if Added /= 0 then
            Tab_Lengths (Added) := 0;
            Result.all := (24, Unsigned_64 (Added), Unsigned_64 (Old));
         end if;
      elsif Control in 7 .. 8 then
         Tabs.Cycle (Tab_State, Control = 7);
         Result.all := (25, Unsigned_64 (Tab_State.Active), Unsigned_64 (Old));
      elsif Control = 9 then
         Window_Limit := False;
         Result.Kind := 27;
      elsif Control = 6 then
         Menus.Dismiss (Menu_State);
         Settings_Open := True; About_Open := False; Settings_Focus := 1;
         Settings_Controls := (others => <>);
         Result.Kind := 28;
      elsif Control in 101 .. 100 + Tabs.Capacity then
         Tabs.Select_Tab (Tab_State, Tabs.Slot (Control - 100));
         Result.all := (25, Unsigned_64 (Tab_State.Active), Unsigned_64 (Old));
      elsif Control in 201 .. 200 + Tabs.Capacity then
         Tabs.Close_Tab (Tab_State, Tabs.Slot (Control - 200));
         Result.all := (26, Unsigned_64 (Control - 200), Unsigned_64 (Tab_State.Active));
      end if;
      if Result.Kind in 24 .. 26 then
         Focused := False; Address_Input_Lost := False;
         Focus_On_State := Result.Kind = 24;
         Held_Buttons := 0;
      end if;
      Chrome_Dirty := True; Controls_Stale := True;
   end Tab_Action;
   procedure Reset_Chrome_Input (Input : App.Input_Event) is
      Reset : App.Input_Event := Input;
      Dirty : Rect := (others => 0);
   begin
      if Input.kind in App.INPUT_CONFIGURE | App.INPUT_RESYNC then
         Reset.kind := App.INPUT_RESYNC;
         Reset.payload0 := 0; Reset.payload1 := 0;
         App.Apply_Pointer_Event
           (Chrome_Interaction, Chrome_UI, Chrome_Controls, Win, Reset, Dirty);
         if Settings_Open then
            App.Apply_Pointer_Event
              (Settings_Interaction, Settings_UI, Settings_Controls, Win, Reset, Dirty);
         end if;
         Menus.Dismiss (Menu_State); Menu_Block_Release := False; Menu_Key_Held := False;
         App.Apply_Pointer_Event
           (Menu_Interaction, Menu_UI, Menu_Controls, Win, Reset, Dirty);
         Controls_Stale := True;
      end if;
   end Reset_Chrome_Input;

   procedure Open_Bookmarks (Add_Page : Boolean; Result : access Event) is
   begin
      Menus.Dismiss (Menu_State); Focused := False; Held_Buttons := 0;
      Servo_Bookmarks.Open (Bookmark_Dialog, Current_URL (1 .. URL_Last),
        Tab_Titles (Tab_State.Active) (1 .. Tab_Lengths (Tab_State.Active)), Add_Page);
      Result.Kind := 28; Chrome_Dirty := True; Controls_Stale := True;
   end Open_Bookmarks;

   procedure Menu_Command (Command : Natural; Result : access Event) is
   begin
      case Command is
         when 1 => if Can_Back then Result.Kind := 17; end if;
         when 2 => if Can_Forward then Result.Kind := 18; end if;
         when 3 => Result.Kind := 19;
         when 5 .. 9 => Tab_Action (Command, Result);
         when 10 => Tab_Action (200 + Tab_State.Active, Result);
         when 11 => Result.Kind := 21;
         when 12 => Focus_Address;
         when 13 .. 14 => Open_Bookmarks (Command = 13, Result);
         when 15 =>
            Tab_Action (6, Result);
            About_Open := True; Settings_Focus := 2;
         when others => null;
      end case;
   end Menu_Command;

   function Menu_Input (Input : App.Input_Event; Result : access Event) return Boolean is
      Was_Open : constant Boolean := Menus.Is_Open (Menu_State);
      M : constant Menus.Model := Menu_Model;
      Command : Natural := 0;
      Handled : Boolean := False;
      K : Menus.Key := Menus.Mnemonic;
      Letter : Character := ' ';
      Target : Natural;
      Action : Controls.Pointer_Action;
      Dirty : Rect := (others => 0);
   begin
      if Menu_Key_Held and then Input.kind in App.INPUT_KEY_UP | App.INPUT_TEXT then
         if Input.kind = App.INPUT_KEY_UP then Menu_Key_Held := False; end if;
         Result.Kind := 22; return True;
      end if;
      if Input.kind = App.INPUT_KEY_DOWN then
         case Input.payload0 is
            when 16#44# => K := Menus.Activate;
            when 16#4B# => K := Menus.Left;
            when 16#4D# => K := Menus.Right;
            when 16#48# => K := Menus.Up;
            when 16#50# => K := Menus.Down;
            when 16#47# => K := Menus.Home;
            when 16#4F# => K := Menus.End_Key;
            when 16#1C# => K := Menus.Enter;
            when 16#39# => K := Menus.Space;
            when 16#01# => K := Menus.Escape;
            when 16#0F# => K := Menus.Tab_Key;
            when 16#23# => Letter := 'h';
            when 16#21# => Letter := 'f';
            when 16#12# => Letter := 'e';
            when 16#2F# => Letter := 'v';
            when 16#1F# => Letter := 's';
            when 16#1E# => Letter := 'a';
            when 16#31# => Letter := 'n';
            when 16#11# => Letter := 'w';
            when 16#14# => Letter := 't';
            when 16#2E# => Letter := 'c';
            when 16#30# => Letter := 'b';
            when 16#13# => Letter := 'r';
            when 16#19# => Letter := 'p';
            when others => null;
         end case;
         if Was_Open or else Input.payload0 = 16#44# or else
           ((Input.payload1 and App.KEYMOD_ALT) /= 0 and then Letter in 'f' | 'e' | 'v' | 'b' | 'h')
         then
            Menus.Handle_Key (Menu_State, M, K, Command, Handled, Letter);
            Handled := Handled or Was_Open;
            Menu_Key_Held := Handled;
         end if;
      elsif Input.kind in App.INPUT_POINTER_MOVE | App.INPUT_POINTER_DOWN | App.INPUT_POINTER_UP then
         Target := Controls.Hit (Menu_Controls,
           CuBit.UI.Input.Pointer_X (Input), CuBit.UI.Input.Pointer_Y (Input));
         if Was_Open or else Menu_Block_Release or else Menus.Is_Menu_Control (500, Target) then
            Action := (if Input.kind = App.INPUT_POINTER_DOWN then Controls.Pointer_Press
              elsif Input.kind = App.INPUT_POINTER_UP then Controls.Pointer_Release
              else Controls.Pointer_Move);
            -- Outside presses dismiss before capture; their release is swallowed too.
            if Input.kind = App.INPUT_POINTER_DOWN and then
              not Menus.Is_Menu_Control (500, Target)
            then
               Menu_Block_Release := True;
            else
               App.Apply_Pointer_Event
                 (Menu_Interaction, Menu_UI, Menu_Controls, Win, Input, Dirty);
            end if;
            if Input.kind = App.INPUT_POINTER_MOVE or else
              (Input.kind = App.INPUT_POINTER_DOWN and then (Input.payload1 and 1) /= 0) or else
              (Input.kind = App.INPUT_POINTER_UP and then (Input.payload1 and 1) = 0)
            then
               Menus.Handle_Pointer (Menu_State, M, Menu_Controls, 500,
                 Target, Action, Command, Handled);
            end if;
            if Input.kind = App.INPUT_POINTER_UP then Menu_Block_Release := False; end if;
            Handled := True;
         end if;
      elsif Was_Open then Handled := True;
      end if;
      if not Handled then return False; end if;
      Result.Kind := (if not Was_Open and then Menus.Is_Open (Menu_State) then 20 else 22);
      Chrome_Dirty := True; Controls_Stale := True;
      if not Was_Open and then Menus.Is_Open (Menu_State) then
         declare
            Reset : App.Input_Event := Input;
         begin
            Reset.kind := App.INPUT_RESYNC; Reset.payload0 := 0; Reset.payload1 := 0;
            App.Apply_Pointer_Event
              (Chrome_Interaction, Chrome_UI, Chrome_Controls, Win, Reset, Dirty);
         end;
         Held_Buttons := 0; Chrome_Press := False;
      end if;
      Menu_Command (Command, Result);
      -- The dialog consumes the mnemonic's release. It now owns keyboard
      -- input, so no menu suppression may remain after mouse dismissal.
      if Settings_Open then Menu_Key_Held := False; end if;
      return True;
   end Menu_Input;

   procedure Settings_Action (ID : Natural; Result : access Event) is
   begin
      if ID = 401 and then not About_Open then
         Settings_Focus := 1; Toggle_Layout; Result.Kind := 20;
      elsif ID = 402 then
         Settings_Open := False; Result.Kind := 20;
      end if;
      Chrome_Dirty := True; Controls_Stale := True;
   end Settings_Action;

   procedure Settings_Input (Input : App.Input_Event; Result : access Event) is
      X, Y, Target : Natural;
      Dirty : Rect := (others => 0);
   begin
      -- A separate native registry blocks all underlying chrome/page input.
      Result.Kind := 22;
      if Input.kind = App.INPUT_KEY_DOWN then
         case Input.payload0 is
            when 16#01# => Settings_Action (402, Result);
            when 16#0F# =>
               Settings_Focus := (if About_Open or else Settings_Focus = 1 then 2 else 1);
               Chrome_Dirty := True;
            when 16#39# | 16#1C# =>
               Settings_Action (400 + Settings_Focus, Result);
            when others => null;
         end case;
      elsif Input.kind in App.INPUT_POINTER_MOVE | App.INPUT_POINTER_DOWN |
        App.INPUT_POINTER_UP | App.INPUT_POINTER_WHEEL
      then
         X := CuBit.UI.Input.Pointer_X (Input);
         Y := CuBit.UI.Input.Pointer_Y (Input);
         Target := Controls.Hit (Settings_Controls, X, Y);
         App.Apply_Pointer_Event (Settings_Interaction, Settings_UI,
           Settings_Controls, Win, Input, Dirty);
         if not Is_Empty (Dirty) then Chrome_Dirty := True; end if;
         if Input.kind = App.INPUT_POINTER_MOVE then Result.Kind := 23; end if;
         if Input.kind = App.INPUT_POINTER_DOWN and then Target in 401 .. 402 then
            Settings_Focus := Target - 400; Chrome_Dirty := True;
         elsif Input.kind = App.INPUT_POINTER_UP and then (Input.payload1 and 1) = 0 then
            for ID in 401 .. 402 loop
               if Controls.Take_Activated (Settings_Controls, ID) and then ID = Target then
                  Settings_Action (ID, Result);
               end if;
            end loop;
         end if;
      end if;
   end Settings_Input;

   function Settings_Box return Rect is
     ((App.Width (Win) - 360) / 2, (App.Height (Win) - 176) / 2, 360, 176);

   procedure Draw_Settings (C : Canvas; Colors : Theme) is
      B : constant Rect := Settings_Box;
      Check : constant Rect := (B.x + 16, B.y + 56, B.w - 32, 32);
      Done : constant Rect := (B.x + B.w - 92, B.y + B.h - 42, 76, 28);
      Widget : Widget_Result;
      Hot : Boolean;
   begin
      Controls.Clear (Settings_Controls);
      Controls.Add_Surface (Settings_Controls, 400, App.Full_Rect (Win));
      Fill_Rect (C, B, Colors.face);
      Stroke_Rect (C, B, Colors.shadow, Colors.shadow);
      Widgets.Label (C, (B.x + 16, B.y + 12, B.w - 32, 28), Colors,
        (if About_Open then "About Penny" else "Penny settings"));
      if About_Open then
         Draw_Bitmap (C, B.x + 16, B.y + 50, Penny_Artwork.Globe_32);
         Widgets.Label (C, (B.x + 60, B.y + 52, B.w - 76, 28), Colors,
           "Penny - Web browser");
         Widgets.Label (C, (B.x + 16, B.y + 92, B.w - 32, 24), Colors,
           "Powered by Servo. Built for CuBit.");
      else
      Controls.Add_Button (Settings_Controls, 401, Check, B);
      Hot := Settings_UI.pointer.enabled and then
        Point_In_Rect (Settings_UI.pointer.x, Settings_UI.pointer.y, Check);
      Draw_Checkbox (C, (Check.x, Check.y + 6, 20, 20), Colors,
        Vertical_Tabs, Hot, Controls.Is_Active (Settings_Controls, 401));
      Widgets.Label (C, (Check.x + 28, Check.y, Check.w - 28, Check.h),
        Colors, "Vertical tabs");
      Widgets.Label (C, (B.x + 16, B.y + 92, B.w - 32, 24), Colors,
        (if Preference_Error then "Could not save this setting."
         else "Changes are saved automatically."));
      end if;
      Widgets.Button (C, Settings_UI, Settings_Controls, 402, Done, B,
        Colors, "Done", Widget, retainedInput => True);
      Stroke_Rect (C, Inflate_Rect ((if Settings_Focus = 1 then Check else Done), 2),
        Colors.accent, Colors.accent);
      CuBit.UI.State.Finish_Frame (Settings_UI);
   end Draw_Settings;

   function Poll (Result : access Event) return Unsigned_32 is
      Input : App.Input_Event;
      Found, Changed : Boolean;
      X, Y, Control : Natural;
      Ctrl, Shift, Alt : Boolean;
      C : Canvas;
      PX, PY : Integer_64;
      function Signed (Word : Unsigned_32) return Integer_32 is
        (Integer_32 (if Word <= 16#7FFF_FFFF# then Integer_64 (Word)
         else Integer_64 (Word) - 2**32));
      function Wire (Value : Integer_64) return Unsigned_64 is
        (Unsigned_64 (Value mod 2**32));
   begin
      if Result = null or else Controls_Stale then return 0; end if;
      Result.all := (others => 0);
      if not Client_Input_Budget.Can_Poll (Input_Batch, Now) then return 0; end if;
      Client_Input_Budget.Charge (Input_Batch);
      App.Poll_Input (Win, Input, Found);
      if not Found then return 0; end if;
      Result.all := (Input.kind, Input.payload0, Input.payload1);
      if Input.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Result.Kind := 21; return 1;
      end if;
      Reset_Chrome_Input (Input);
      if Input.kind = App.INPUT_CONFIGURE or else Input.kind = App.INPUT_RESYNC then
         if Input.kind = App.INPUT_RESYNC and then Focused then
            -- Never submit an address whose input history has a gap. A new
            -- Ctrl+L/address-field activation restores the authoritative URL.
            Address_Input_Lost := True;
         end if;
         Chrome_Dirty := True; Held_Buttons := 0; Chrome_Press := False;
         Result.Kind := 20; return 1;
      end if;
      if Servo_Bookmarks.Is_Open (Bookmark_Dialog) then
         Result.Kind := 22;
         if Menu_Key_Held and then Input.kind in App.INPUT_KEY_UP | App.INPUT_TEXT then
            if Input.kind = App.INPUT_KEY_UP then Menu_Key_Held := False; end if;
            return 1;
         end if;
         declare
            Outcome : Servo_Bookmarks.Outcome; OK : Boolean;
            use type Servo_Bookmarks.Outcome;
         begin
            Servo_Bookmarks.Handle (Bookmark_Dialog, Input, Outcome);
            if Outcome /= Servo_Bookmarks.Unchanged then Chrome_Dirty := True; Controls_Stale := True; end if;
            if Outcome = Servo_Bookmarks.Navigate then
               Editor.Initialize (Address_Edit, Servo_Bookmarks.Location (Bookmark_Dialog), OK);
               Result.Kind := 16;
            elsif Outcome = Servo_Bookmarks.Closed then Result.Kind := 20;
            end if;
         end;
         return 1;
      end if;
      if Settings_Open then
         Settings_Input (Input, Result); return 1;
      end if;
      if Menu_Input (Input, Result) then return 1; end if;
      Ctrl := (Input.payload1 and App.KEYMOD_CTRL) /= 0;
      Shift := (Input.payload1 and App.KEYMOD_SHIFT) /= 0;
      Alt := (Input.payload1 and App.KEYMOD_ALT) /= 0;
      if Input.kind = App.INPUT_KEY_DOWN then
         if Ctrl and then Input.payload0 = 16#20# then
            Open_Bookmarks (True, Result); Menu_Key_Held := True; return 1;
         elsif Ctrl and then Shift and then Input.payload0 = 16#30# then
            Open_Bookmarks (False, Result); Menu_Key_Held := True; return 1;
         elsif Ctrl and then Input.payload0 = 16#26# then
            Focus_Address; Result.Kind := 22; return 1;
         elsif Ctrl and then Input.payload0 = 16#11# then
            if Shift then Result.Kind := 21;
            else Tab_Action (200 + Tab_State.Active, Result); end if;
            return 1;
         elsif Ctrl and then Input.payload0 = 16#31# then
            Window_Limit := False; Chrome_Dirty := True;
            Result.Kind := 27; return 1;
         elsif Ctrl and then Input.payload0 = 16#14# then
            Tab_Action (5, Result); return 1;
         elsif Ctrl and then Input.payload0 = 16#0F# then
            Tabs.Cycle (Tab_State, Shift);
            Tab_Action (100 + Tab_State.Active, Result); return 1;
         elsif (Ctrl and then Input.payload0 = 16#13#) or else Input.payload0 = 16#3F# then
            Result.Kind := 19; return 1;
         elsif Alt and then Input.payload0 = 16#4B# and then Can_Back then
            Result.Kind := 17; return 1;
         elsif Alt and then Input.payload0 = 16#4D# and then Can_Forward then
            Result.Kind := 18; return 1;
         end if;
      end if;
      if Focused and then Input.kind in App.INPUT_KEY_DOWN | App.INPUT_KEY_UP | App.INPUT_TEXT then
         Result.Kind := 22;
         -- Key release cannot change the editor. Printable presses are
         -- handled by the following text event, so neither needs a repaint.
         if Input.kind = App.INPUT_KEY_UP then return 1; end if;
         if Input.kind = App.INPUT_TEXT then
            if Input.payload0 in 32 .. 126 then
               Editor.Insert (Address_Edit,
                 String'(1 => Character'Val (Input.payload0)), Changed);
               if not Changed then return 1; end if;
            else return 1;
            end if;
         elsif Input.kind = App.INPUT_KEY_DOWN then
            case Input.payload0 is
               when 16#1C# =>
                  if not Address_Input_Lost then
                     Result.Kind := 16; Focused := False;
                  end if;
               when 16#01# => Load_Address; Focused := False;
               when 16#0E# => Editor.Backspace (Address_Edit, Changed);
               when 16#53# => Editor.Delete_Forward (Address_Edit, Changed);
               when 16#4B# => Editor.Move (Address_Edit,
                 (if Ctrl then Editor.Move_Word_Left else Editor.Move_Left), Shift);
               when 16#4D# => Editor.Move (Address_Edit,
                 (if Ctrl then Editor.Move_Word_Right else Editor.Move_Right), Shift);
               when 16#47# => Editor.Move (Address_Edit, Editor.Move_Start, Shift);
               when 16#4F# => Editor.Move (Address_Edit, Editor.Move_End, Shift);
               when 16#1E# =>
                  if Ctrl then Editor.Select_All (Address_Edit);
                  else return 1;
                  end if;
               when others => return 1;
            end case;
         end if;
         Reveal_Cursor; Chrome_Dirty := True; return 1;
      end if;
      if Input.kind in App.INPUT_POINTER_MOVE | App.INPUT_POINTER_DOWN |
        App.INPUT_POINTER_UP | App.INPUT_POINTER_WHEEL
      then
         X := CuBit.UI.Input.Pointer_X (Input); Y := CuBit.UI.Input.Pointer_Y (Input);
         Control := Controls.Hit (Chrome_Controls, X, Y);
         declare
            Dirty : Rect := (others => 0);
            Pointer : App.Input_Event := Input;
         begin
            -- Native controls use bounded natural coordinates; page delivery
            -- below preserves the original signed device coordinates.
            Pointer.payload0 := Unsigned_64 (X) or Shift_Left (Unsigned_64 (Y), 32);
            if Input.kind not in App.INPUT_POINTER_DOWN | App.INPUT_POINTER_UP
              or else (Input.kind = App.INPUT_POINTER_DOWN and then
                (Input.payload1 and 1) /= 0)
              or else (Input.kind = App.INPUT_POINTER_UP and then Chrome_UI.pointer.down
                and then (Input.payload1 and 1) = 0)
            then
               App.Apply_Pointer_Event
                 (Chrome_Interaction, Chrome_UI, Chrome_Controls, Win, Pointer, Dirty);
               Chrome_Dirty := Chrome_Dirty or else not Is_Empty (Dirty);
            end if;
         end;
         if Held_Buttons = 0 and then
           (Chrome_Press or else not Point_In_Rect (X, Y, Page_Area))
         then
            Result.Kind := (if Input.kind = App.INPUT_POINTER_MOVE then 23 else 22);
            if Input.kind = App.INPUT_POINTER_DOWN and then (Input.payload1 and 1) /= 0 then
               Chrome_Press := True;
               if Control = 4 then Focus_Address; end if;
            elsif Input.kind = App.INPUT_POINTER_UP and then (Input.payload1 and 1) = 0 then
               -- Drain every action, including a parent released over a child.
               -- Only the topmost release target may activate; close never
               -- falls through to the containing tab's selection action.
               for ID in 1 .. 200 + Tabs.Capacity loop
                  if Controls.Take_Activated (Chrome_Controls, ID) and then ID = Control then
                     case ID is
                        when 1 => if Can_Back then Result.Kind := 17; end if;
                        when 2 => if Can_Forward then Result.Kind := 18; end if;
                        when 3 => Result.Kind := 19;
                        when 5 .. 9 | 101 .. 100 + Tabs.Capacity | 201 .. 200 + Tabs.Capacity => Tab_Action (ID, Result);
                        when others => null;
                     end case;
                  end if;
               end loop;
               Chrome_Press := False;
            end if;
            return 1;
         end if;
         if Input.kind = App.INPUT_POINTER_DOWN then
            Focused := False; Chrome_Dirty := True; Held_Buttons := Input.payload1;
         elsif Input.kind = App.INPUT_POINTER_UP then Held_Buttons := Input.payload1;
         end if;
         C := App.Canvas (Win);
         -- Servo input positions are device pixels. Preserve the same phase
         -- as the page's physical origin; never round page height separately.
         PX := Integer_64 (Servo_Input_Geometry.Relative
           (Signed (Unsigned_32 (Input.payload0 and 16#FFFF_FFFF#)), Page_Area.x,
            C.densityNumerator, C.densityDenominator));
         PY := Integer_64 (Servo_Input_Geometry.Relative
           (Signed (Unsigned_32 (Shift_Right (Input.payload0, 32))), Page_Area.y,
            C.densityNumerator, C.densityDenominator));
         Result.A := Wire (PX) or Shift_Left (Wire (PY), 32);
      end if;
      return 1;
   end Poll;

   procedure Draw_Chrome (C : Canvas) is
      Widget : Widget_Result;
      Colors : constant Theme := Current_Theme;
      Text : constant String := Editor.Content (Address_Edit);
      function Position (P : Editor.Text_Position) return Natural is
        (if P - 1 < First_Shown then 0 else P - 1 - First_Shown);
   begin
      Fill_Rect (C, Toolbar, Colors.face);
      if Vertical_Tabs then Fill_Rect (C, (0, 64, 192, App.Height (Win) - 88), Colors.panel);
      else Fill_Rect (C, (0, 64, App.Width (Win), 40), Colors.panel); end if;
      if Tab_State.Active /= 0 and then Visible_Tabs > 0 then
         First_Tab := Natural'Min (First_Tab, Tab_Rank (Tab_State.Active));
         if Tab_Rank (Tab_State.Active) >= First_Tab + Visible_Tabs then
            First_Tab := Tab_Rank (Tab_State.Active) - Visible_Tabs + 1;
         end if;
         First_Tab := Natural'Min (First_Tab,
           Tabs.Count (Tab_State) - Natural'Min (Tabs.Count (Tab_State), Visible_Tabs));
      end if;
      Controls.Clear (Chrome_Controls);
      Controls.Add_Surface (Chrome_Controls, 300, Page_Area);
      Widgets.Button (C, Chrome_UI, Chrome_Controls, 5, New_Button,
        App.Full_Rect (Win), Colors, "+", Widget, retainedInput => True);
      if Tabs.Count (Tab_State) > Visible_Tabs then
         Widgets.Button (C, Chrome_UI, Chrome_Controls, 7, Previous_Tab_Button,
           App.Full_Rect (Win), Colors, "<", Widget, retainedInput => True);
         Widgets.Button (C, Chrome_UI, Chrome_Controls, 8, Next_Tab_Button,
           App.Full_Rect (Win), Colors, ">", Widget, retainedInput => True);
      end if;
      for I in Tabs.Slot loop
         if Tab_State.Items (I) = Tabs.Live and then
           Tab_Rank (I) >= First_Tab and then Tab_Rank (I) < First_Tab + Visible_Tabs
         then
            declare
               B : constant Rect := Tab_Box (I);
               Content : Canvas;
               Child_Colors : Theme;
            begin
               Widgets.Tab (C, Chrome_UI, Chrome_Controls, 100 + I,
                 B, App.Full_Rect (Win), Colors, Tab_State.Active = I,
                 Content, Child_Colors, Widget,
                 orientation => (if Vertical_Tabs then Vertical else Horizontal));
               Widgets.Label (Content, (B.x + 8, B.y + 2, B.w - 30, B.h - 4),
                 Child_Colors,
                 (if Tab_Lengths (I) = 0 then "New" else Tab_Titles (I) (1 .. Tab_Lengths (I))));
               Widgets.Button (Content, Chrome_UI, Chrome_Controls, 200 + I,
                 (B.x + B.w - 20, B.y + 2, 18, B.h - 4), Content.clip,
                 Child_Colors, "x", Widget, retainedInput => True, quiet => True);
            end;
         end if;
      end loop;
      Widgets.Navigation_Button (C, Chrome_UI, Chrome_Controls, 1,
        (8, 31, 60, 26), App.Full_Rect (Win), Colors,
        Widgets.Navigate_Back, "Back", Can_Back, Widget);
      Widgets.Navigation_Button (C, Chrome_UI, Chrome_Controls, 2,
        (76, 31, 72, 26), App.Full_Rect (Win), Colors,
        Widgets.Navigate_Forward, "Forward", Can_Forward, Widget);
      Widgets.Button (C, Chrome_UI, Chrome_Controls, 3, (156, 31, 64, 26),
        App.Full_Rect (Win), Colors, "Reload", Widget, retainedInput => True);
      Controls.Add (Chrome_Controls, 4, Address_Area, Toolbar);
      Draw_Text_Edit_Field (C, Address_Area, Colors,
        Text (1 + Natural'Min (First_Shown, Text'Length) .. Text'Last),
        Position (Editor.Cursor (Address_Edit)), Position (Editor.Selection_First (Address_Edit)),
        Position (Editor.Selection_Last (Address_Edit)), Focused, False);
      Draw_Status_Bar (C, (0, App.Height (Win) - Status_Height, App.Width (Win), Status_Height),
        Colors, (if Address_Input_Lost then "Input interrupted; press Ctrl+L to retry"
          elsif Window_Limit then "Window limit reached; close a window or wait for cleanup"
          elsif Tab_Limit then "Tab limit reached; close a tab or wait for cleanup"
          elsif Preference_Error then "Layout changed; Config could not save preference"
          elsif Invalid_Address then "Enter an HTTP or HTTPS address"
          elsif Address_Too_Long then "Address exceeds editor limit; Ctrl+L enters a new address"
          elsif Busy then "Loading..." else "Ready"), "    Penny");
      Draw_Bitmap (C, App.Width (Win) - 8 - UI_Text_Width ("    Penny"),
        App.Height (Win) - 20, Penny_Artwork.Globe_16);
      Controls.Clear (Menu_Controls);
      Menus.Draw (C, Menu_Controls, Menu_State, Menu_Model, 500,
        (0, 0, App.Width (Win), Menu_Height), Colors);
      CuBit.UI.State.Finish_Frame (Menu_UI);
      if Settings_Open then Draw_Settings (C, Colors); end if;
      if Servo_Bookmarks.Is_Open (Bookmark_Dialog) then Servo_Bookmarks.Draw (C, Bookmark_Dialog, Colors); end if;
      Controls_Stale := False;
      CuBit.UI.State.Finish_Frame (Chrome_UI);
   end Draw_Chrome;

   function Prepare return Unsigned_32 is
      Repair : Rect;
      Ready : Boolean;
   begin
      if Paint_Open or else not App.Is_Open (Win) then return 0; end if;
      App.Begin_Paint (Win, App.Full_Rect (Win), Repair, Ready);
      Paint_Open := Ready;
      return (if Ready then 1 else 0);
   end Prepare;

   procedure Cancel is
   begin
      if Paint_Open then
         Paint_Open := False;
         App.Cancel_Paint (Win);
      end if;
   end Cancel;

   function Present
     (RGBA : System.Address; Length : Unsigned_64;
      Width, Height : Unsigned_32) return Unsigned_32
   is
      C : Canvas;
      V : aliased Viewport;
      Left, Top, Physical_Height, Target_Bytes : Natural;
      Matched : Boolean;
   begin
      if not Paint_Open then return 0; end if;
      -- A current complete page is retained by SWGL. Reconstruct the complete
      -- candidate from it + authoritative chrome, covering both slots' debt.
      C := App.Canvas (Win);
      Metrics (V'Access);
      Left := Geometry.Edge (Page_Area.x, C.densityNumerator, C.densityDenominator);
      Top := Geometry.Edge (Page_Area.y, C.densityNumerator, C.densityDenominator);
      Physical_Height := Geometry.Edge (C.height, C.densityNumerator, C.densityDenominator);
      -- Configured Frame_Pair storage is capped at 16 MiB. These guards also
      -- avoid unchecked foreign-length/address overflow before imported views.
      Matched := Servo_Frame_Copy.Accepts
        (Length, Width, Height, V.Width, V.Height, C.pitch, Physical_Height, Top, Left) and then
        RGBA /= System.Null_Address and then Length <= Max_Bytes and then
        To_Integer (RGBA) <= Integer_Address'Last - Integer_Address (Length);
      if Matched then
         Target_Bytes := C.pitch * Physical_Height;
         declare
            Source : Servo_Frame_Copy.Bytes (0 .. Natural (Length) - 1)
              with Import, Address => RGBA;
            Target : Servo_Frame_Copy.Pixels (0 .. Target_Bytes / 4 - 1)
              with Import, Address => C.addr;
         begin
            Servo_Frame_Copy.Paint (Source, Target, C.pitch / 4,
              (Left, Top, Positive (Width), Positive (Height)));
         end;
      else
         Fill_Rect (C, App.Full_Rect (Win), 16#FFFFFF#);
      end if;
      Draw_Chrome (C);
      Paint_Open := False;
      App.Present (Win, App.Full_Rect (Win));
      Chrome_Dirty := False;
      if not Matched then return 2;
      elsif App.Frame_Pending (Win) then return 0;
      else return 1; end if;
   end Present;

   function Pending return Unsigned_32 is
     (if Chrome_Dirty or else App.Frame_Pending (Win) then 1 else 0);

   procedure Window_Error is
   begin Window_Limit := True; Chrome_Dirty := True; end Window_Error;

   function Is_Open return Boolean is (App.Is_Open (Win));

   procedure Close is
   begin
      Cancel;
      App.Close (Win);
      -- UI.App retains uncertain loans; repeating Close retries retirement.
   end Close;
end Servo_Session;
