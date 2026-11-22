package body Fs_Handle_Codes with SPARK_Mode is

   procedure Lemma_Decode_Encode (Slot : Handle_Slot; Current : Generation) is
      Low  : constant Handle_Word := Handle_Word (Slot) + 1;
      High : constant Handle_Word := Shift_Left (Handle_Word (Current), Slot_Code_Bits);
   begin
      pragma Assert (Low in 1 .. Maximum_Handles);
      pragma Assert ((High and Slot_Code_Mask) = 0);
      pragma Assert (((High or Low) and Slot_Code_Mask) = (Low and Slot_Code_Mask));
      pragma Assert ((Low and Slot_Code_Mask) = Low);
      pragma Assert (Slot_Code (Encode (Slot, Current)) = Low);
      pragma Assert (Slot_Code (Encode (Slot, Current)) - 1 = Handle_Word (Slot));
      pragma Assert (Handle_Slot (Slot_Code (Encode (Slot, Current)) - 1) = Slot);
   end Lemma_Decode_Encode;

   procedure Lemma_Encode_Decode (Word : Handle_Word) is
      Code : constant Handle_Word := Slot_Code (Word);
      High : constant Handle_Word :=
        Shift_Left (Handle_Word (Generation_Of (Word)), Slot_Code_Bits);
   begin
      pragma Assert (Code in 1 .. Maximum_Handles);
      pragma Assert (Handle_Word (Slot_Of (Word)) = Code - 1);
      pragma Assert (Handle_Word (Slot_Of (Word)) + 1 = Code);
      pragma Assert (High = (Word and not Slot_Code_Mask));
      pragma Assert ((High or Code) = Word);
   end Lemma_Encode_Decode;

   procedure Lemma_Tag_Slot (Slot : Handle_Slot; Current : Generation) is
   begin
      pragma Assert ((Tag_Of (Slot, Current) and Tag_Slot_Mask) = Dirty_Tag (Slot));
   end Lemma_Tag_Slot;

   function Check_Of
     (Inode : Unsigned_32; Volume : Volume_List.Volume_Index;
      Generation_Mix : Unsigned_32) return Directory_Check
   is
      Mixed : constant Unsigned_32 :=
        (Inode * Directory_Golden xor
         Shift_Left (Unsigned_32 (Volume), Directory_Volume_Shift)) xor Generation_Mix;
   begin
      return Directory_Check (if Mixed = 0 then Directory_Golden else Mixed);
   end Check_Of;

   procedure Lemma_Token (Check : Directory_Check; Position : Unsigned_64) is
   begin
      pragma Assert (Check_In (Token_Of (Check, Position)) = Check);
      pragma Assert (Position_In (Token_Of (Check, Position)) = Position);
   end Lemma_Token;

end Fs_Handle_Codes;
