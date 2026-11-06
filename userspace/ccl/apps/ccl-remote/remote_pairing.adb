with Interfaces; use Interfaces;
with CuBit.Hardware_Random;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Input;
with Remote_Activity;

package body Remote_Pairing is
   package W renames Remote_Wire;
   package UI renames CuBit.UI;
   package A renames CuBit.UI.App;

   Secret : W.Token := [others => 0];
   Ready : Boolean := False;

   --  The code as shown: 26 symbols in groups of five ("XXXXX-...-X").
   Group : constant := 5;
   Shown_Length : constant := W.Token_Text_Length + (W.Token_Text_Length - 1) / Group;
   function Grouped return String is
      Text : constant W.Token_Text := W.Token_Image (Secret);
      Result : String (1 .. Shown_Length);
      At_Index : Natural := 0;
   begin
      for I in Text'Range loop
         if I > 1 and then (I - 1) mod Group = 0 then
            At_Index := At_Index + 1;
            Result (At_Index) := '-';
         end if;
         At_Index := At_Index + 1;
         Result (At_Index) := Text (I);
      end loop;
      return Result;
   end Grouped;

   procedure Generate (Success : out Boolean) is
      Bytes : CuBit.Hardware_Random.Byte_Array (1 .. W.Token_Bytes);
   begin
      CuBit.Hardware_Random.Fill (Bytes, Success);
      if Success then
         for I in W.Token_Index loop
            Secret (I) := Bytes (I);
         end loop;
         Bytes := [others => 0];
      end if;
      Ready := Success;
   end Generate;

   function Matches (Credential : W.Token) return Boolean is
     (Ready and then W.Same_Token (Credential, Secret));

   ---------------------------------------------------------------------------
   --  The window
   ---------------------------------------------------------------------------
   Width  : constant := 640;
   Margin : constant := 20;
   Line   : constant := 26;
   --  The activity list: rows shown at once, each Row tall, with the time
   --  in a column Time_Column wide and the outcome in one Outcome_Column wide.
   Visible_Rows : constant := 12;
   Row : constant := 20;
   Time_Column : constant := 76;
   Outcome_Column : constant := 72;
   List_Top : constant := Margin + 6 * Line;
   List_Height : constant := Visible_Rows * Row;
   Height : constant := List_Top + List_Height + Margin + Line;
   Maximum_Text : constant := 160;
   --  Set-1 scan codes of the keys that scroll the list.
   KEY_UP : constant := 16#48#;
   KEY_DOWN : constant := 16#50#;
   KEY_PAGE_UP : constant := 16#49#;
   KEY_PAGE_DOWN : constant := 16#51#;
   KEY_HOME : constant := 16#47#;
   KEY_END : constant := 16#4F#;

   Win : A.Window;
   Open : Boolean := False;
   Dirty : Boolean := False;
   Address_Text, Grant_Text, Peer_Text : String (1 .. Maximum_Text) := [others => ' '];
   Address_Length, Grant_Length, Peer_Length : Natural range 0 .. Maximum_Text := 0;
   Is_Paired : Boolean := False;
   --  Rows scrolled up from the newest (0: the list follows new entries).
   Scrolled : Remote_Activity.Entry_Count := 0;

   procedure Keep (Text : String; Into : out String; Length : out Natural) is
   begin
      Length := Natural'Min (Text'Length, Into'Length);
      Into := [others => ' '];
      Into (Into'First .. Into'First + Length - 1) := Text (Text'First .. Text'First + Length - 1);
   end Keep;

   function Most_Scrolled return Remote_Activity.Entry_Count is
     (if Remote_Activity.Count > Visible_Rows then Remote_Activity.Count - Visible_Rows else 0);

   procedure Scroll (By : Integer) is
   begin
      Scrolled := Remote_Activity.Entry_Count
        (Integer'Max (0, Integer'Min (Most_Scrolled, Integer (Scrolled) + By)));
      Dirty := True;
   end Scroll;

   procedure Paint_Activity (C : UI.Canvas; Theme : UI.Theme) is
      package RA renames Remote_Activity;
      Box : constant UI.Rect := (Margin, List_Top, Width - 2 * Margin, List_Height);
      Shown_Rows : constant Natural := Natural'Min (Visible_Rows, RA.Count);
      Last : constant Natural := RA.Count - Natural (Scrolled);
      Text_X : constant Natural := Margin + Time_Column;
      Text_Width : constant Natural := Width - 2 * Margin - Time_Column - Outcome_Column;
   begin
      UI.Fill_Rect (C, Box, Theme.panel);
      if RA.Count = 0 then
         UI.Draw_UI_Text (C, Margin + Margin / 2, List_Top + Row / 4,
           "Nothing yet: what the host does is listed here.", Theme.muted, Theme.panel);
         return;
      end if;
      for I in 1 .. Shown_Rows loop
         declare
            Item : constant RA.Activity := RA.Entry_At (Last - Shown_Rows + I);
            Y : constant Natural := List_Top + (I - 1) * Row + Row / 8;
            What : constant String := RA.Kind_Name (Item.Op) &
              (if Item.Argument_Last = 0 then "" else " " & RA.Argument_Of (Item));
         begin
            UI.Draw_Code_Text (C, Margin + Margin / 2, Y, RA.Clock_Image (Item.Time_Ms), Theme.muted, Theme.panel);
            UI.Draw_Code_Text (C, Text_X, Y, UI.Fit_Code_Text (What, Text_Width - Margin / 2),
                               Theme.text, Theme.panel);
            UI.Draw_UI_Text (C, Width - Margin - Outcome_Column + Margin / 2, Y, RA.Outcome_Name (Item.Result),
              (case Item.Result is
                  when RA.OK => Theme.good, when RA.Refused => Theme.accent, when RA.Failed => Theme.danger),
              Theme.panel);
         end;
      end loop;
   end Paint_Activity;

   procedure Paint (Repair : UI.Rect) is
      C : constant UI.Canvas := A.Canvas (Win, Repair);
      Theme : constant UI.Theme := UI.Current_Theme;
   begin
      UI.Fill_Rect (C, A.Full_Rect (Win), Theme.face);
      UI.Draw_UI_Text (C, Margin, Margin, "Remote access is on.", Theme.text, Theme.face);
      if Is_Paired then
         --  The code is not needed any more, and never shown again.
         UI.Draw_UI_Text (C, Margin, Margin + Line,
           "A host is paired; what it does is listed below.", Theme.muted, Theme.face);
         UI.Draw_UI_Text (C, Margin, Margin + 5 * Line / 2,
           "Paired with " & (if Peer_Length = 0 then "a host" else Peer_Text (1 .. Peer_Length)),
           Theme.text, Theme.face);
      else
         UI.Draw_UI_Text (C, Margin, Margin + Line,
           "On the host: cubit-remote pair <address>, then type this code:", Theme.muted, Theme.face);
         UI.Draw_Code_Text (C, Margin, Margin + 5 * Line / 2, Grouped, Theme.text, Theme.face);
      end if;
      UI.Draw_UI_Text (C, Margin, Margin + 4 * Line,
        "Listening on: " & (if Address_Length = 0 then "(waiting for a network address)"
                            else Address_Text (1 .. Address_Length)), Theme.text, Theme.face);
      UI.Draw_UI_Text (C, Margin, Margin + 5 * Line,
        "The host may: " & Grant_Text (1 .. Grant_Length), Theme.text, Theme.face);
      Paint_Activity (C, Theme);
      UI.Draw_UI_Text (C, Margin, List_Top + List_Height + Margin / 2,
        "Stop ccl-remote.app to revoke; a new start makes a new code.", Theme.muted, Theme.face);
   end Paint;

   procedure Close_Window is
   begin
      if Open then
         A.Close (Win);
         Open := False;
      end if;
   end Close_Window;

   procedure Service is
      Event : A.Input_Event;
      Found, OK : Boolean;
      Repair : UI.Rect;
   begin
      if not Open then
         return;
      end if;
      if not A.Is_Open (Win) then
         Open := False;
         return;
      end if;
      loop
         A.Poll_Input (Win, Event, Found);
         exit when not Found;
         A.Begin_Input_Event (Win, Event);
         if Event.kind = A.INPUT_CONFIGURE or else Event.kind = A.INPUT_RESYNC then
            Dirty := True;
         elsif Event.kind = A.INPUT_POINTER_WHEEL then
            --  Up (positive) shows older entries.
            Scroll (CuBit.UI.Input.Pointer_Wheel_Delta (Event));
         elsif Event.kind = A.INPUT_KEY_DOWN then
            case Event.payload0 is
               when KEY_UP => Scroll (1);
               when KEY_DOWN => Scroll (-1);
               when KEY_PAGE_UP => Scroll (Visible_Rows);
               when KEY_PAGE_DOWN => Scroll (-Visible_Rows);
               when KEY_HOME => Scroll (Remote_Activity.Maximum_Entries);
               when KEY_END => Scroll (-Remote_Activity.Maximum_Entries);
               when others => null;
            end case;
         end if;
         A.Finish_Input_Event (Win, Event);
         if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST
           or else (Event.kind = A.INPUT_KEY_DOWN and then Event.payload0 = A.KEY_ESC)
         then
            --  Hidden, not revoked: the code stays valid until the endpoint stops.
            debugPrint ("ccl-remote: pairing window closed" & ASCII.LF);
            Close_Window;
            return;
         end if;
      end loop;
      if Dirty or else A.Frame_Pending (Win) then
         A.Begin_Paint (Win, (if Dirty then A.Full_Rect (Win) else (0, 0, 0, 0)), Repair, OK);
         if OK then
            Paint (Repair);
            A.Present (Win, Repair);
            Dirty := False;
         end if;
      end if;
   end Service;

   procedure Show (Where : Display; Addresses, Grants : String; Shown : out Boolean) is
      OK : Boolean;
   begin
      Shown := False;
      if not Ready then
         return;
      end if;
      case Where is
         when Kernel_Console =>
            --  The console is this machine's screen (serial): not a log.
            debugPrint ("ccl-remote: pairing code " & Grouped & " (grants: " & Grants & ")" & ASCII.LF);
            Shown := True;
         when Window =>
            Keep (Addresses, Address_Text, Address_Length);
            Keep (Grants, Grant_Text, Grant_Length);
            A.Open (Win, Width, Height,
                    A.WINDOW_FLAG_DECORATED or A.WINDOW_FLAG_CLOSEABLE or A.WINDOW_FLAG_FIXED_SIZE, OK,
                    title => "Remote access", protected_frames => True, preferred_size => True);
            if OK then
               Open := True;
               Dirty := True;
               Service;
               debugPrint ("ccl-remote: pairing window open" & ASCII.LF);
            end if;
            Shown := OK;
      end case;
   end Show;

   procedure Update (Addresses : String) is
   begin
      if Open then
         Keep (Addresses, Address_Text, Address_Length);
         Dirty := True;
         Service;
      end if;
   end Update;

   procedure Paired (Peer : String) is
   begin
      Is_Paired := True;
      Keep (Peer, Peer_Text, Peer_Length);
      Dirty := True;
      Service;
   end Paired;

   procedure Activity_Changed is
   begin
      --  Following the newest unless scrolled back; a scrolled view keeps
      --  showing the same entries while new ones arrive below.
      if Scrolled > 0 then
         Scrolled := Remote_Activity.Entry_Count'Min (Most_Scrolled, Scrolled + 1);
      end if;
      Dirty := True;
      Service;
   end Activity_Changed;

   function Window_Open return Boolean is (Open);
end Remote_Pairing;
