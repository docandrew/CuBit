with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Desktop_Messages;
with CuBit.Desktop_Protocol;

package body CuBit.UI.Clipboard is
   package MG renames CuBit.Memory_Grants;
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Status_Code;
   use type P.Payload_Length;

   type Window_State is (Unset, Ready, Disabled);
   State  : Window_State := Unset;
   Base   : Unsigned_64 := 0;
   Window_Grant : MG.Grant_Reference;

   subtype Window_Index is Positive range 1 .. P.Maximum_Payload;
   type Window_Bytes is array (Window_Index) of Unsigned_8;

   function Chord_Of (Code : Unsigned_64; Modifiers : Unsigned_64) return Chord is
     (case Command_Of (Code, Modifiers) is
        when Clipboard_Chords.Copy_Command  => Copy_Chord,
        when Clipboard_Chords.Cut_Command   => Cut_Chord,
        when Clipboard_Chords.Paste_Command => Paste_Chord,
        when Clipboard_Chords.No_Command | Clipboard_Chords.Select_All_Command =>
          No_Chord);

   --  One window per process, set up on first use.
   function Ensure_Window return Boolean is
      Created : Boolean := False;
   begin
      if State = Unset then
         Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, P.Maximum_Payload);
         if Base /= 0 and then Base mod P.Page_Bytes = 0 and then
           Base <= Unsigned_64'Last - P.Maximum_Payload
         then
            MG.Create_Via_Capability
              (CAP_SLOT_DESKTOP, To_Address (Integer_Address (Base)),
               P.Window_Pages, True, Window_Grant, Created);
         end if;
         State := (if Created then Ready else Disabled);
      end if;
      return State = Ready;
   end Ensure_Window;

   --  Zero what a transfer used: a secret must not stay in the window.
   procedure Scrub (Count : Natural) is
      Window : Window_Bytes
        with Import, Volatile, Address => To_Address (Integer_Address (Base));
   begin
      for I in 1 .. Natural'Min (Count, P.Maximum_Payload) loop
         Window (I) := 0;
      end loop;
   end Scrub;

   function Call (Request : DP.Wire_Message) return DP.Wire_Message is
      Message : CuBit.Messages.Message := CuBit.Desktop_Messages.From_Wire (Request);
   begin
      Message.tag := capCall (CAP_SLOT_DESKTOP, Message,
                              Deadline_After (Call_Deadline_Ms));
      return CuBit.Desktop_Messages.To_Wire (Message);
   end Call;

   function From_Status (Status : DP.Status_Code) return Outcome is
     (case Status is
        when DP.Success => Done,
        when DP.Denied => Denied,
        when DP.Bad_State => Empty,
        when DP.Resources_Exhausted => Too_Large,
        when others => Unavailable);

   procedure Offer
     (Surface : Unsigned_64; Parts : String; Shape : P.Layout;
      Kind : Sensitivity; Result : out Outcome)
   is
      Receipt : P.Offer_Receipt;
   begin
      Result := Unavailable;
      if Surface = 0 or else not Ensure_Window then
         return;
      end if;
      declare
         Window : Window_Bytes
           with Import, Volatile, Address => To_Address (Integer_Address (Base));
      begin
         for I in Parts'Range loop
            Window (I - Parts'First + 1) := Character'Pos (Parts (I));
         end loop;
      end;
      Receipt := P.Decode_Offer_Receipt
        (Call (P.Encode (P.Offer_Request'
           (P.Surface_Name (Surface), Window_Grant, Shape, Kind))));
      Scrub (Parts'Length);
      if Receipt.Status = DP.Invalid_Request then
         --  A malformed or missing reply may hide a retained mapping:
         --  never reuse the window.
         State := Disabled;
      end if;
      Result := From_Status (Receipt.Status);
   end Offer;

   procedure Copy_Text
     (Surface : Unsigned_64; Text : String; Kind : Sensitivity;
      Result : out Outcome) is
   begin
      if Text'Length = 0 or else Text'Length > P.Maximum_Payload then
         Result := (if Text'Length = 0 then Empty else Too_Large);
         return;
      end if;
      Offer (Surface, Text,
             (Type_Length => 0, Text_Length => P.Payload_Length (Text'Length),
              CCL_Length => 0), Kind, Result);
   end Copy_Text;

   procedure Copy_Value
     (Surface : Unsigned_64; Type_Name, Text, Literal : String;
      Result : out Outcome) is
   begin
      if Text'Length = 0 or else Literal'Length = 0 or else
        Type_Name'Length = 0 or else Type_Name'Length > P.Maximum_Type_Name
        or else Type_Name'Length + Text'Length + Literal'Length > P.Maximum_Payload
      then
         Result := (if Text'Length = 0 then Empty else Too_Large);
         return;
      end if;
      Offer (Surface, Type_Name & Text & Literal,
             (Type_Length => P.Type_Name_Length (Type_Name'Length),
              Text_Length => P.Payload_Length (Text'Length),
              CCL_Length => P.Payload_Length (Literal'Length)),
             P.Ordinary, Result);
   end Copy_Value;

   procedure Request_Paste
     (Surface : Unsigned_64; Want_CCL : Boolean; Target : Sensitivity;
      Receipt : out P.Paste_Receipt; Result : out Outcome) is
   begin
      Receipt := (Status => DP.Invalid_Request);
      Result := Unavailable;
      if Surface = 0 or else not Ensure_Window then
         return;
      end if;
      Receipt := P.Decode_Paste_Receipt
        (Call (P.Encode (P.Paste_Request'
           (P.Surface_Name (Surface), Window_Grant, Want_CCL, Target))));
      if Receipt.Status = DP.Invalid_Request then
         State := Disabled;
      end if;
      Result := From_Status (Receipt.Status);
   end Request_Paste;

   --  Copy Count window bytes from Offset into Into (Into'First ..).
   procedure Take
     (Offset, Count : Natural; Into : out String; Last : out Natural)
   is
      Window : Window_Bytes
        with Import, Volatile, Address => To_Address (Integer_Address (Base));
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
      for I in 1 .. Natural'Min (Count, Into'Length) loop
         Into (Into'First + I - 1) := Character'Val (Window (Offset + I));
         Last := Into'First + I - 1;
      end loop;
   end Take;

   procedure Paste_Text
     (Surface : Unsigned_64; Target : Sensitivity;
      Text : out String; Last : out Natural; Kind : out Sensitivity;
      Result : out Outcome)
   is
      Receipt : P.Paste_Receipt;
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
      Kind := Ordinary;
      Request_Paste (Surface, False, Target, Receipt, Result);
      if Result /= Done or else Receipt.Status /= DP.Success then
         return;
      end if;
      Kind := Receipt.Kind;
      --  Longer than the caller's buffer: keep its first Text'Length bytes,
      --  as a field with a maximum length does.
      Take (0, Natural (Receipt.Shape.Text_Length), Text, Last);
      Scrub (Natural (P.Total (Receipt.Shape)));
   end Paste_Text;

   procedure Paste_Value
     (Surface : Unsigned_64;
      Type_Name : out String; Type_Last : out Natural;
      Text : out String; Text_Last : out Natural;
      Literal : out String; Literal_Last : out Natural;
      Result : out Outcome)
   is
      Receipt : P.Paste_Receipt;
   begin
      Type_Name := [others => ' '];
      Text := [others => ' '];
      Literal := [others => ' '];
      Type_Last := Type_Name'First - 1;
      Text_Last := Text'First - 1;
      Literal_Last := Literal'First - 1;
      Request_Paste (Surface, True, Ordinary, Receipt, Result);
      if Result /= Done or else Receipt.Status /= DP.Success then
         return;
      end if;
      declare
         T : constant Natural := Natural (Receipt.Shape.Type_Length);
         X : constant Natural := Natural (Receipt.Shape.Text_Length);
         L : constant Natural := Natural (Receipt.Shape.CCL_Length);
      begin
         if T > Type_Name'Length or else X > Text'Length or else
           L > Literal'Length
         then
            Result := Too_Large;
         else
            Take (0, T, Type_Name, Type_Last);
            Take (T, X, Text, Text_Last);
            Take (T + X, L, Literal, Literal_Last);
         end if;
         Scrub (T + X + L);
      end;
   end Paste_Value;
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
end CuBit.UI.Clipboard;
