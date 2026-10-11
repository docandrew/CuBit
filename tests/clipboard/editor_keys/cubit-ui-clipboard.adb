--  Host body of CuBit.UI.Clipboard for the editor-keys test: the calls go
--  to Host_Clipboard instead of Desktop.
with Host_Clipboard;

package body CuBit.UI.Clipboard is
   package H renames Host_Clipboard;
   use type Clipboard_Chords.Ticket;
   use type P.Sensitivity;

   function Chord_Of (Code : Unsigned_64; Modifiers : Unsigned_64) return Chord is
     (case Command_Of (Code, Modifiers) is
        when Clipboard_Chords.Copy_Command  => Copy_Chord,
        when Clipboard_Chords.Cut_Command   => Cut_Chord,
        when Clipboard_Chords.Paste_Command => Paste_Chord,
        when Clipboard_Chords.No_Command | Clipboard_Chords.Select_All_Command =>
          No_Chord);

   function Spend (Wanted : Clipboard_Chords.Ticket) return Boolean is
      Granted : constant Boolean := H.Ticket = Wanted;
   begin
      if Granted then
         H.Ticket := Clipboard_Chords.No_Ticket;
      end if;
      return Granted;
   end Spend;

   procedure Copy_Text
     (Surface : Unsigned_64; Text : String; Kind : Sensitivity;
      Result : out Outcome)
   is
      pragma Unreferenced (Surface);
   begin
      if not Spend (Clipboard_Chords.Copy_Ticket) then
         Result := Denied;
      elsif Text'Length > H.CAPACITY then
         Result := Too_Large;
      else
         H.Put (Text, Kind);
         H.Copies := H.Copies + 1;
         Result := Done;
      end if;
   end Copy_Text;

   procedure Copy_Value
     (Surface : Unsigned_64; Type_Name, Text, Literal : String;
      Result : out Outcome)
   is
      pragma Unreferenced (Surface);
   begin
      if not Spend (Clipboard_Chords.Copy_Ticket) then
         Result := Denied;
      elsif Text'Length > H.CAPACITY or else Literal'Length > H.CAPACITY
        or else Type_Name'Length > H.Type_Name'Length
      then
         Result := Too_Large;
      else
         H.Put (Text);
         H.Type_Name (1 .. Type_Name'Length) := Type_Name;
         H.Type_Length := Type_Name'Length;
         H.Literal (1 .. Literal'Length) := Literal;
         H.Literal_Length := Literal'Length;
         H.Copies := H.Copies + 1;
         Result := Done;
      end if;
   end Copy_Value;

   procedure Copy_Built
     (Surface : Unsigned_64; Type_Name : String;
      Text, Literal : Clipboard_Text.Builder; Result : out Outcome) is
   begin
      if not Text.Fits then
         Result := Too_Large;
      elsif Text.Length = 0 then
         Result := Empty;
      elsif Literal.Fits and then Literal.Length > 0 and then Type_Name'Length > 0 then
         Copy_Value (Surface, Type_Name, Clipboard_Text.Image (Text),
                     Clipboard_Text.Image (Literal), Result);
      else
         Copy_Text (Surface, Clipboard_Text.Image (Text), Ordinary, Result);
      end if;
   end Copy_Built;

   procedure Paste_Text
     (Surface : Unsigned_64; Target : Sensitivity;
      Text : out String; Last : out Natural; Kind : out Sensitivity;
      Result : out Outcome)
   is
      pragma Unreferenced (Surface);
      Count : Natural;
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
      Kind := Ordinary;
      if not Spend (Clipboard_Chords.Paste_Ticket) then
         Result := Denied;
      elsif H.Length = 0 then
         Result := Empty;
      else
         Count := Natural'Min (H.Length, Text'Length);
         Text (Text'First .. Text'First + Count - 1) := H.Text (1 .. Count);
         Last := Text'First + Count - 1;
         Kind := H.Kind;
         if H.Kind = Sensitive or else Target = Sensitive then
            H.Text := [others => ' '];
            H.Length := 0;
         end if;
         H.Pastes := H.Pastes + 1;
         Result := Done;
      end if;
   end Paste_Text;

   procedure Paste_Value
     (Surface : Unsigned_64;
      Type_Name : out String; Type_Last : out Natural;
      Text : out String; Text_Last : out Natural;
      Literal : out String; Literal_Last : out Natural;
      Result : out Outcome)
   is
      Kind : Sensitivity;
   begin
      Type_Name := [others => ' '];
      Literal := [others => ' '];
      Type_Last := Type_Name'First - 1;
      Literal_Last := Literal'First - 1;
      Paste_Text (Surface, Ordinary, Text, Text_Last, Kind, Result);
   end Paste_Value;
end CuBit.UI.Clipboard;
