--  Hosted tests of the desktop clipboard: store, paste-once secrets, expiry,
--  gesture authority and the wire codec. Run by tests/clipboard/run.sh.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Clipboard_Store; use Clipboard_Store;
with Clipboard_Gestures;
with Compositor_Clipboard_Protocol;
with CuBit.Desktop_Protocol;
with Clipboard_Chords;
with Clipboard_Text;

procedure Clipboard_Tests is
   package G renames Clipboard_Gestures;
   package P renames Compositor_Clipboard_Protocol;
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Status_Code;
   use type G.Surface_Name, G.Millisecond;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Staging : Payload_Bytes := [others => 0];

   function Text_Shape (Length : Payload_Length) return Layout is
     ((Type_Length => 0, Text_Length => Length, CCL_Length => 0));

   procedure Put_Text (Text : String) is
   begin
      Staging := [others => 0];
      for I in Text'Range loop
         Staging (Payload_Index (I - Text'First + 1)) := Character'Pos (Text (I));
      end loop;
   end Put_Text;

   function Got (Data : Payload_Bytes; Length : Payload_Length) return String is
      Result : String (1 .. Natural (Length));
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Data (Payload_Index (I)));
      end loop;
      return Result;
   end Got;

   procedure Copy
     (S : in out Store; Text : String; Kind : Sensitivity; Now : Millisecond;
      Name : out Entry_Name)
   is
      Outcome : Offer_Outcome;
   begin
      Put_Text (Text);
      Offer (S, 7, Now, Kind, Text_Shape (Text'Length), Staging, Name, Outcome);
      Check (Outcome = Stored, "offer stored " & Text);
   end Copy;

   procedure Paste_Text
     (S : in out Store; Now : Millisecond; Target : Sensitivity;
      Expect : String; Expect_Outcome : Paste_Outcome := Delivered)
   is
      Meta : Metadata;
      Data : Payload_Bytes;
      Outcome : Paste_Outcome;
   begin
      Paste (S, Now, Target, [Text_Form => True, CCL_Form => False],
             Meta, Data, Outcome);
      Check (Outcome = Expect_Outcome,
             "paste outcome " & Expect_Outcome'Image & " got " & Outcome'Image);
      if Outcome = Delivered then
         Check (Got (Data, Meta.Shape.Text_Length) = Expect,
                "paste text " & Expect);
      end if;
   end Paste_Text;

   procedure Store_Tests is
      S : Store;
      N1, N2, Secret : Entry_Name;
      Slot : Slot_Count;
   begin
      Check (Current (S) = 0, "empty store has no current entry");
      Paste_Text (S, 10, Ordinary, "", Nothing_Available);

      Copy (S, "hello", Ordinary, 100, N1);
      Copy (S, "world", Ordinary, 200, N2);
      Check (N2 > N1, "names grow");
      Paste_Text (S, 300, Ordinary, "world");
      Paste_Text (S, 400, Ordinary, "world");   --  ordinary pastes repeat

      --  Sensitive: paste once.
      Copy (S, "hunter2", Sensitive, 1_000, Secret);
      Slot := Find (S, Secret);
      Check (Slot /= 0, "secret stored");
      Check (Preview_Length (S, Slot, 40) = 0, "secret has no preview");
      Check (Info (S, Slot).Life.Kind = Until_Deadline, "secret has deadline");
      Paste_Text (S, 1_500, Ordinary, "hunter2");
      Check (Info (S, Slot).Phase = Consumed, "secret consumed");
      Check (Wiped (S, Slot), "secret wiped after paste");
      --  The second paste must not return the secret: it falls back to the
      --  newest remaining ordinary entry.
      Paste_Text (S, 1_600, Ordinary, "world");

      --  Expiry: a secret not pasted in time is wiped and never delivered.
      Copy (S, "token", Sensitive, 2_000, Secret);
      Slot := Find (S, Secret);
      Paste_Text (S, 2_000 + Sensitive_Lifetime_Ms, Ordinary, "world");
      Check (Info (S, Slot).Phase = Expired, "secret expired");
      Check (Wiped (S, Slot), "expired secret wiped");

      --  Pasting into a sensitive target consumes even an ordinary entry.
      Copy (S, "plainpw", Ordinary, 3_000, N1);
      Paste_Text (S, 3_100, Sensitive, "plainpw");
      Slot := Find (S, N1);
      Check (Info (S, Slot).Phase = Consumed and then Wiped (S, Slot),
             "paste into secret field leaves no copy");
      Paste_Text (S, 3_200, Ordinary, "world");

      --  Pin, promote, delete, clear.
      Slot := Find (S, N2);
      declare
         Done : Boolean;
      begin
         Set_Pinned (S, Slot, True, Done);
         Check (Done and then Info (S, Slot).Pinned, "pin ordinary");
         Copy (S, "secret2", Sensitive, 4_000, Secret);
         Set_Pinned (S, Find (S, Secret), True, Done);
         Check (not Done, "a secret cannot be pinned");
         Copy (S, "later", Ordinary, 4_100, N1);
         Promote (S, Find (S, N2), Done);
         Check (Done, "promote");
         Paste_Text (S, 4_200, Ordinary, "world");
         Clear (S);
         Check (Current (S) /= 0 and then Info (S, Current (S)).Pinned,
                "clear keeps pinned");
         Delete (S, Current (S));
         Check (Current (S) = 0, "delete last");
      end;

      --  History limit: oldest unpinned is evicted, all pinned refuses.
      declare
         Last : Entry_Name := No_Entry;
         Done : Boolean;
         Outcome : Offer_Outcome;
         Name : Entry_Name;
      begin
         for I in 1 .. Capacity + 3 loop
            Copy (S, "item" & I'Image, Ordinary, Millisecond (5_000 + I), Last);
         end loop;
         Check (Find (S, Last) /= 0, "newest kept under pressure");
         for K in Slot_Index loop
            Set_Pinned (S, K, True, Done);
         end loop;
         Put_Text ("x");
         Offer (S, 7, 9_000, Ordinary, Text_Shape (1), Staging, Name, Outcome);
         Check (Outcome = History_Full and then Name = No_Entry,
                "all pinned refuses a new entry");
      end;
   end Store_Tests;

   procedure Typed_Tests is
      S : Store;
      Name : Entry_Name;
      Outcome : Offer_Outcome;
      Meta : Metadata;
      Data : Payload_Bytes;
      Pasted : Paste_Outcome;
      --  A CCL value: type, text form, CCL form.
      Type_Name : constant String := "Integer";
      Text : constant String := "42";
      Source : constant String := "42";
      All_Bytes : constant String := Type_Name & Text & Source;
   begin
      Put_Text (All_Bytes);
      Offer (S, 9, 10, Ordinary,
             (Type_Length => Type_Name'Length, Text_Length => Text'Length,
              CCL_Length => Source'Length), Staging, Name, Outcome);
      Check (Outcome = Stored, "typed offer");
      Paste (S, 11, Ordinary, [others => True], Meta, Data, Pasted);
      Check (Pasted = Delivered and then Meta.Shape.CCL_Length = 2 and then
             Got (Data, 11) = All_Bytes, "typed paste keeps type and forms");
      Paste (S, 12, Ordinary, [Text_Form => True, CCL_Form => False],
             Meta, Data, Pasted);
      Check (Pasted = Delivered and then Meta.Shape.Type_Length = 0 and then
             Meta.Shape.CCL_Length = 0 and then Got (Data, 2) = Text,
             "text-only paste of a typed value gets the text form");
      Paste (S, 13, Ordinary, [Text_Form => False, CCL_Form => True],
             Meta, Data, Pasted);
      Check (Pasted = Format_Refused, "text form is required");
   end Typed_Tests;

   procedure Gesture_Tests is
      S : G.State;
      Granted : Boolean;
   begin
      G.Redeem (S, G.Paste_Gesture, 5, 100, Granted);
      Check (not Granted, "paste denied without a gesture");
      G.Issue (S, G.Paste_Gesture, 5, 100);
      G.Redeem (S, G.Paste_Gesture, 6, 100, Granted);
      Check (not Granted, "another surface cannot redeem");
      G.Redeem (S, G.Copy_Gesture, 5, 100, Granted);
      Check (not Granted, "a paste chord is not a copy grant");
      G.Redeem (S, G.Paste_Gesture, 5, 150, Granted);
      Check (Granted, "the focused surface redeems its chord");
      G.Redeem (S, G.Paste_Gesture, 5, 160, Granted);
      Check (not Granted, "a ticket is single use");
      G.Issue (S, G.Paste_Gesture, 5, 1_000);
      G.Redeem (S, G.Paste_Gesture, 5, 1_000 + G.Window_Ms + 1, Granted);
      Check (not Granted, "a stale chord expires");
      G.Issue (S, G.Copy_Gesture, 5, 2_000);
      G.Revoke (S);
      G.Redeem (S, G.Copy_Gesture, 5, 2_001, Granted);
      Check (not Granted, "focus change revokes");
      G.Issue (S, G.Copy_Gesture, 5, 3_000);
      G.Redeem (S, G.Copy_Gesture, 5, 2_999, Granted);
      Check (not Granted, "a clock running backwards grants nothing");
   end Gesture_Tests;

   --  The chord table Desktop and the toolkit share, checked against the
   --  chords the gesture tickets were issued for before it existed.
   procedure Chord_Tests is
      package K renames Clipboard_Chords;
      use type K.Command, K.Ticket, K.Key_Code, K.Modifier_Bits;
      function Old_Ticket (Code : K.Key_Code; Mods : K.Modifier_Bits) return K.Ticket is
        (if K.Has (Mods, K.Alt_Bit) then K.No_Ticket
         elsif (K.Has (Mods, K.Ctrl_Bit) and then Code in K.Key_C | K.Key_X | K.Key_Insert)
           or else (K.Has (Mods, K.Shift_Bit) and then not K.Has (Mods, K.Ctrl_Bit) and then Code = K.Key_Delete)
         then K.Copy_Ticket
         elsif (K.Has (Mods, K.Ctrl_Bit) and then Code = K.Key_V)
           or else (K.Has (Mods, K.Shift_Bit) and then not K.Has (Mods, K.Ctrl_Bit) and then Code = K.Key_Insert)
         then K.Paste_Ticket
         else K.No_Ticket);
      Mismatches : Natural := 0;
      Activations : Natural := 0;
   begin
      for Code in K.Key_Code range 0 .. 16#7F# loop
         for Mods in K.Modifier_Bits range 0 .. 15 loop
            if K.Ticket_Of (Code, Mods) /= Old_Ticket (Code, Mods) then
               if K.Is_Activation_Key (Code, Mods) and then K.Ticket_Of (Code, Mods) = K.Copy_Ticket then
                  Activations := Activations + 1;
               else
                  Mismatches := Mismatches + 1;
               end if;
            end if;
         end loop;
      end loop;
      Check (Mismatches = 0, "tickets match the chords, Enter aside");
      --  Enter with Shift and/or Caps: four combinations, never with Ctrl or Alt.
      Check (Activations = 4, "Enter is the only activation key");
      Check (K.Command_Of (K.Key_A, K.Ctrl_Bit) = K.Select_All_Command, "Ctrl+A selects all");
      Check (K.Ticket_Of (K.Key_A, K.Ctrl_Bit) = K.No_Ticket, "Select All issues no ticket");
      Check (K.Command_Of (K.Key_C, K.Ctrl_Bit or K.Shift_Bit) = K.Copy_Command, "Ctrl+Shift+C copies");
      Check (K.Command_Of (K.Key_Delete, K.Ctrl_Bit or K.Shift_Bit) = K.No_Command,
             "Ctrl+Shift+Delete is not a cut");
      Check (K.Ticket_Of (K.Key_Enter, K.Ctrl_Bit) = K.No_Ticket, "Ctrl+Enter is not an activation");
      Check (K.Activation_Ticket = K.Copy_Ticket, "a pointer press never pastes");
   end Chord_Tests;

   procedure Text_Tests is
      package CT renames Clipboard_Text;
      Q : constant Character := '"';
      BS : constant Character := '\';
      B : CT.Builder (16);
      Line : String := "a" & ASCII.LF & "b" & Character'Val (7) & "c";
      Kept : Natural;
   begin
      CT.Append_Quoted (B, "x" & Q & BS);
      Check (B.Fits and then CT.Image (B) = Q & "x" & BS & Q & BS & BS & Q,
             "quoting escapes quotes and backslashes");
      CT.Clear (B);
      CT.Append_Quoted (B, "t" & ASCII.HT & ASCII.LF & ASCII.CR);
      Check (CT.Image (B) = Q & "t" & BS & "t" & BS & "n" & BS & "r" & Q,
             "quoting escapes tabs and line breaks");
      CT.Clear (B);
      CT.Append_Quoted (B, "bell" & Character'Val (7));
      Check (not B.Fits, "an unreadable byte makes the literal unusable");
      CT.Clear (B);
      CT.Append (B, "0123456789");
      CT.Append (B, "0123456789");
      Check (not B.Fits and then B.Length = 10, "an append that does not fit leaves what fitted");
      CT.Clear (B);
      Check (B.Fits and then B.Length = 0, "clear starts over");
      CT.Append (B, "0123456789012345");
      Check (B.Fits and then B.Length = 16, "an exact fit fits");
      CT.Single_Line (Line, Kept);
      Check (Line (1 .. Kept) = "a bc", "single line: break to space, control dropped");
   end Text_Tests;

   procedure Codec_Tests is
      use type P.Layout;
      use type P.Sensitivity;
      Shape : constant P.Layout := (Type_Length => 7, Text_Length => 2,
                                    CCL_Length => 5);
      Offer : constant P.Offer_Request :=
        (Surface => 3, Window => (slot => 9, generation => 4),
         Shape => Shape, Kind => P.Sensitive);
      Decoded : constant P.Offer_Decoding :=
        P.Decode_Offer (P.Encode (Offer));
      Paste : constant P.Paste_Request :=
        (Surface => 3, Window => (slot => 9, generation => 4),
         Want_CCL => True, Target => P.Sensitive);
      Paste_Decoded : constant P.Paste_Decoding :=
        P.Decode_Paste (P.Encode (Paste));
      Receipt : constant P.Paste_Receipt :=
        P.Decode_Paste_Receipt (P.Encode (P.Paste_Receipt'(DP.Success, 12, Shape, P.Ordinary)));
      Denied : constant P.Paste_Receipt :=
        P.Decode_Paste_Receipt (P.Encode (P.Paste_Receipt'(Status => DP.Denied)));
      Bad : DP.Wire_Message := P.Encode (Offer);
   begin
      Check (Decoded.Accepted and then Decoded.Value.Shape = Shape and then
             Decoded.Value.Kind = P.Sensitive and then
             Decoded.Value.Window.slot = 9, "offer round trip");
      Check (Paste_Decoded.Accepted and then Paste_Decoded.Value.Want_CCL and then
             Paste_Decoded.Value.Target = P.Sensitive, "paste round trip");
      Check (Receipt.Status = DP.Success and then Receipt.Shape = Shape,
             "paste receipt round trip");
      Check (Denied.Status = DP.Denied, "denied receipt");
      --  A layout claiming more than the window, or a text-less entry.
      Bad.Words (2) := P.Pack ((0, 65_536, 1), P.Ordinary);
      Check (not P.Decode_Offer (Bad).Accepted, "oversized offer rejected");
      Bad.Words (2) := P.Pack ((0, 0, 0), P.Ordinary);
      Check (not P.Decode_Offer (Bad).Accepted, "empty offer rejected");
      Bad.Words (2) := P.Pack ((0, 1, 0), P.Ordinary) + 2 ** 57;
      Check (not P.Decode_Offer (Bad).Accepted, "unknown sensitivity rejected");
   end Codec_Tests;
begin
   Store_Tests;
   Typed_Tests;
   Gesture_Tests;
   Chord_Tests;
   Text_Tests;
   Codec_Tests;
   if Failures = 0 then
      Put_Line ("clipboard tests: PASS" & Checks'Image & " checks");
   else
      Put_Line ("clipboard tests: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Clipboard_Tests;
