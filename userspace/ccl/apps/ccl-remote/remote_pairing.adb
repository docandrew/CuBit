with Interfaces; use Interfaces;
with CuBit.Hardware_Random;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with Ada.Unchecked_Deallocation;
with CuBit.UI.Boxes;
with CuBit.UI.Clipboard;
with CuBit.UI.Input;
with CuBit.UI.Text_Views;
with CuBit.UI_Metrics;
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
   --  The window: a column of lines over the activity log, a read-only
   --  text field (CuBit.UI.Text_Views) that scrolls and selects, laid out
   --  by CuBit.UI.Boxes from the window's size (UI-033).
   ---------------------------------------------------------------------------
   package B renames CuBit.UI.Boxes;
   package TV renames CuBit.UI.Text_Views;
   package M renames CuBit.UI_Metrics;
   Maximum_Text : constant := 160;
   --  The log field shows at least this many columns and lines, and
   --  prefers a typical entry's width and a dozen lines.
   Log_Columns : constant := 40;
   Log_Lines : constant := 6;
   Log_Preferred_Columns : constant := 72;
   Log_Preferred_Lines : constant := 12;
   --  Between an entry's time and what the host did.
   Time_Gap : constant String := "  ";
   Log_Id : constant B.Widget_Id := 1;
   --  The tree: under a dozen widgets, each line at most Maximum_Text.
   Layout_Nodes : constant := 16;
   Layout_Text : constant := 8 * Maximum_Text;
   subtype Layout_Tree is B.Tree (Layout_Nodes, Layout_Text);

   Win : A.Window;
   Open : Boolean := False;
   Dirty : Boolean := False;
   Address_Text, Grant_Text, Peer_Text : String (1 .. Maximum_Text) := [others => ' '];
   Address_Length, Grant_Length, Peer_Length : Natural range 0 .. Maximum_Text := 0;
   Is_Paired : Boolean := False;
   Log_View : TV.View;
   Log_Focused : Boolean := True;

   --  The log as text: one entry per line, oldest first; rebuilt (on the
   --  heap, CuAlloc) when an entry is added.
   type Text_Access is access String;
   procedure Release is new Ada.Unchecked_Deallocation (String, Text_Access);
   Log : Text_Access := new String'("");
   Log_Added : Interfaces.Unsigned_64 := 0;

   function Entry_Line (Item : Remote_Activity.Activity) return String is
     (Remote_Activity.Clock_Image (Item.Time_Ms) & Time_Gap & Remote_Activity.Line (Item));

   procedure Rebuild_Log is
      package RA renames Remote_Activity;
      Total : Natural := 0;
      At_Index : Natural := 0;
   begin
      if Log_Added = RA.Added then
         return;
      end if;
      for I in 1 .. RA.Count loop
         Total := Total + Entry_Line (RA.Entry_At (I))'Length + (if I < RA.Count then 1 else 0);
      end loop;
      Release (Log);
      Log := new String (1 .. Total);
      for I in 1 .. RA.Count loop
         declare
            Line : constant String := Entry_Line (RA.Entry_At (I));
         begin
            Log (At_Index + 1 .. At_Index + Line'Length) := Line;
            At_Index := At_Index + Line'Length;
            if I < RA.Count then
               At_Index := At_Index + 1;
               Log (At_Index) := ASCII.LF;
            end if;
         end;
      end loop;
      Log_Added := RA.Added;
   end Rebuild_Log;

   procedure Keep (Text : String; Into : out String; Length : out Natural) is
   begin
      Length := Natural'Min (Text'Length, Into'Length);
      Into := [others => ' '];
      Into (Into'First .. Into'First + Length - 1) := Text (Text'First .. Text'First + Length - 1);
   end Keep;

   Placeholder : constant String := "Nothing yet: what the host does is listed here.";

   procedure Describe (T : in out Layout_Tree) is
   begin
      B.Clear (T);
      B.Begin_Column (T, Margin => M.Space_XL, Width_Growth => B.Fill, Height_Growth => B.Fill);
      B.Add_Label (T, "Remote access is on.");
      if Is_Paired then
         --  The code is not needed any more, and never shown again.
         B.Add_Wrapping_Label (T, "A host is paired; what it does is listed below.", B.Muted_Text);
         B.Add_Wrapping_Label
           (T, "Paired with " & (if Peer_Length = 0 then "a host" else Peer_Text (1 .. Peer_Length)));
      else
         B.Add_Wrapping_Label (T, "On the host: cubit-remote pair <address>, then type this code:", B.Muted_Text);
         B.Add_Label (T, Grouped, B.Code_Text);
      end if;
      B.Add_Wrapping_Label
        (T, "Listening on: " & (if Address_Length = 0 then "(waiting for a network address)"
                                else Address_Text (1 .. Address_Length)));
      B.Add_Wrapping_Label (T, "The host may: " & Grant_Text (1 .. Grant_Length));
      B.Add_Block (T, Log_Id, TV.Width_Hint (Log_Columns, Log_Preferred_Columns),
                   TV.Height_Hint (Log_Lines, Log_Preferred_Lines), Scrolls => True);
      B.Add_Wrapping_Label (T, "Stop ccl-remote.app to revoke; a new start makes a new code.", B.Muted_Text);
      B.End_Box (T);
   end Describe;

   function Log_Box return UI.Rect is
      T : Layout_Tree;
   begin
      Describe (T);
      B.Arrange (T, A.Full_Rect (Win));
      return B.Bounds (T, Log_Id);
   end Log_Box;

   --  UI-035: the wheel goes to the scrolling widget under the pointer.
   function Wheel_Target (X, Y : Natural) return B.Widget_Id is
      T : Layout_Tree;
   begin
      Describe (T);
      B.Arrange (T, A.Full_Rect (Win));
      return B.Scroll_Target (T, X, Y);
   end Wheel_Target;

   --  What the log field shows: the entries, or a word that there are none.
   function Log_Text return String is (if Log'Length = 0 then Placeholder else Log.all);

   procedure Paint (Repair : UI.Rect) is
      C : constant UI.Canvas := A.Canvas (Win, Repair);
      Theme : constant UI.Theme := UI.Current_Theme;
      T : Layout_Tree;
      procedure Text (X, Y : Natural; Value : String; Style : B.Text_Style; Item : B.Widget_Id) is
         pragma Unreferenced (Item);
         use type B.Text_Style;
      begin
         if Style = B.Code_Text then
            UI.Draw_Code_Text (C, X, Y, Value, Theme.text, Theme.face);
         else
            UI.Draw_UI_Text (C, X, Y, Value, (if Style = B.Muted_Text then Theme.muted else Theme.text), Theme.face);
         end if;
      end Text;
      procedure No_Button (R : UI.Rect; Label : String; Item : B.Widget_Id) is null;
      procedure Draw_Lines is new B.Draw (Text, No_Button);
   begin
      Describe (T);
      B.Arrange (T, A.Full_Rect (Win));
      UI.Fill_Rect (C, A.Full_Rect (Win), Theme.face);
      Draw_Lines (T);
      Rebuild_Log;
      TV.Sync (Log_View, Log_Text, B.Bounds (T, Log_Id));
      TV.Draw (C, B.Bounds (T, Log_Id), Theme, Log_View, Log_Text, Log_Focused);
   end Paint;

   --  The window's least and natural client size, from its layout.
   procedure Window_Size (Least, Natural_Size : out B.Size) is
      T : Layout_Tree;
   begin
      Describe (T);
      B.Measure (T, Least, Natural_Size);
   end Window_Size;

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
      package UI_Input renames CuBit.UI.Input;
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
         Rebuild_Log;
         declare
            Box : constant UI.Rect := Log_Box;
            Before : constant TV.View := Log_View;
            use type TV.View;
         begin
            if Event.kind = A.INPUT_CONFIGURE or else Event.kind = A.INPUT_RESYNC then
               Dirty := True;
            elsif Event.kind = A.INPUT_POINTER_WHEEL then
               if Wheel_Target (UI_Input.Pointer_X (Event), UI_Input.Pointer_Y (Event)) = Log_Id then
                  TV.Wheel (Log_View, Log_Text, Box, UI_Input.Pointer_Wheel_Delta (Event));
               end if;
            elsif Event.kind = A.INPUT_POINTER_DOWN or else Event.kind = A.INPUT_POINTER_UP
              or else Event.kind = A.INPUT_POINTER_MOVE
            then
               declare
                  Primary_Button : constant Unsigned_64 := 1;
                  Down : constant Boolean := (UI_Input.Pointer_Buttons (Event) and Primary_Button) /= 0;
                  Shift : constant Boolean := (UI_Input.Pointer_Modifiers (Event) and A.KEYMOD_SHIFT) /= 0;
                  X : constant Natural := UI_Input.Pointer_X (Event);
                  Y : constant Natural := UI_Input.Pointer_Y (Event);
               begin
                  if Event.kind = A.INPUT_POINTER_DOWN then
                     Log_Focused := UI.Point_In_Rect (X, Y, Box);
                     Dirty := True;
                  end if;
                  TV.Pointer (Log_View, Log_Text, Box, X, Y, Down,
                              Pressed => Event.kind = A.INPUT_POINTER_DOWN,
                              Released => Event.kind = A.INPUT_POINTER_UP, Shift => Shift);
               end;
            elsif Event.kind = A.INPUT_KEY_DOWN then
               declare
                  Modifiers : constant Unsigned_64 := UI_Input.Key_Modifiers (Event);
                  Handled : Boolean;
                  Copied : CuBit.UI.Clipboard.Outcome;
                  use type CuBit.UI.Clipboard.Chord;
               begin
                  if CuBit.UI.Clipboard.Chord_Of (Event.payload0, Modifiers) = CuBit.UI.Clipboard.Copy_Chord then
                     --  The user's own chord: copy the selected lines.
                     if Log'Length > 0 then
                        CuBit.UI.Clipboard.Copy_Text
                          (A.Surface_ID (Win), TV.Selection (Log_View, Log.all), CuBit.UI.Clipboard.Ordinary,
                           Copied);
                     end if;
                  elsif Event.payload0 < Unsigned_64 (Natural'Last) then
                     TV.Key (Log_View, Log_Text, Box, Natural (Event.payload0),
                             Shift => (Modifiers and A.KEYMOD_SHIFT) /= 0,
                             Ctrl => (Modifiers and A.KEYMOD_CTRL) /= 0, Handled => Handled);
                  end if;
               end;
            end if;
            if Log_View /= Before then
               Dirty := True;
            end if;
         end;
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
            declare
               Least, Natural_Size : B.Size;
            begin
               Window_Size (Least, Natural_Size);
               A.Open (Win, Natural_Size.Width, Natural_Size.Height,
                       A.WINDOW_FLAG_DECORATED or A.WINDOW_FLAG_CLOSEABLE, OK,
                       title => "Remote access", protected_frames => True,
                       minimum_width => Least.Width, minimum_height => Least.Height, preferred_size => True);
            end;
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
      --  The log follows the newest entry unless scrolled back (the text
      --  view's Follow_End); a scrolled view keeps its place.
      Dirty := True;
      Service;
   end Activity_Changed;

   function Window_Open return Boolean is (Open);
end Remote_Pairing;
