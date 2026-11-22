package body Host_Clipboard is
   procedure Key (Code, Modifiers : Unsigned_64) is
   begin
      Ticket := Clipboard_Chords.Ticket_Of
        (Clipboard_Chords.Key_Code (Code), Clipboard_Chords.Modifier_Bits (Modifiers));
   end Key;

   procedure Put (Item : String; Item_Kind : P.Sensitivity := P.Ordinary) is
   begin
      Text (1 .. Item'Length) := Item;
      Length := Item'Length;
      Type_Length := 0;
      Literal_Length := 0;
      Kind := Item_Kind;
   end Put;
end Host_Clipboard;
