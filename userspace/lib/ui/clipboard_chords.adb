package body Clipboard_Chords with SPARK_Mode is
   function Command_Of
     (Code : Key_Code; Modifiers : Modifier_Bits) return Command
   is
      Ctrl  : constant Boolean := Has (Modifiers, Ctrl_Bit);
      Shift : constant Boolean := Has (Modifiers, Shift_Bit);
   begin
      if Has (Modifiers, Alt_Bit) then
         return No_Command;
      elsif Ctrl and then (Code = Key_C or else Code = Key_Insert) then
         return Copy_Command;
      elsif (Ctrl and then Code = Key_X) or else
        (Shift and then not Ctrl and then Code = Key_Delete)
      then
         return Cut_Command;
      elsif (Ctrl and then Code = Key_V) or else
        (Shift and then not Ctrl and then Code = Key_Insert)
      then
         return Paste_Command;
      elsif Ctrl and then not Shift and then Code = Key_A then
         return Select_All_Command;
      end if;
      return No_Command;
   end Command_Of;

   function Ticket_Of
     (Code : Key_Code; Modifiers : Modifier_Bits) return Ticket is
   begin
      case Command_Of (Code, Modifiers) is
         when Copy_Command | Cut_Command =>
            return Copy_Ticket;
         when Paste_Command =>
            return Paste_Ticket;
         when No_Command | Select_All_Command =>
            return (if Is_Activation_Key (Code, Modifiers) then Copy_Ticket
                    else No_Ticket);
      end case;
   end Ticket_Of;
end Clipboard_Chords;
