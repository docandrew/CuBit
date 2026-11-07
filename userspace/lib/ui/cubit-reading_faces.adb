package body CuBit.Reading_Faces is
   use type CuBit.Fonts.Register_Status;

   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME;
   type Entry_Record is record
      Length : Name_Length := 0;
      Text   : String (1 .. MAXIMUM_NAME) := [others => ' '];
   end record;
   Entries : array (Face_Index) of Entry_Record;
   Offered_Count : Face_Count := 0;
   Choice : Face_Count := 0;

   function Slot_Of (Index : Face_Index) return CuBit.Fonts.Reading_Face is
     (CuBit.Fonts.Reading_Face'Val (CuBit.Fonts.Reading_Face'Pos (CuBit.Fonts.Reading_Face'First) + Index - 1));

   procedure Offer (Name : String; Bytes : System.Address; Length : Natural; Index : out Face_Index;
                    Status : out Offer_Status)
   is
      Registered : CuBit.Fonts.Register_Status;
   begin
      Index := Face_Index'First;
      if Name'Length not in 1 .. MAXIMUM_NAME then
         Status := Bad_Name;
         return;
      elsif Find (Name) > 0 then
         Index := Find (Name);
         Status := Offered;
         return;
      elsif Offered_Count = MAXIMUM_FACES then
         Status := No_Slot;
         return;
      end if;
      CuBit.Fonts.Register (Slot_Of (Offered_Count + 1), Bytes, Length, Registered);
      case Registered is
         when CuBit.Fonts.Registered =>
            Offered_Count := Offered_Count + 1;
            Entries (Offered_Count).Length := Name'Length;
            Entries (Offered_Count).Text (1 .. Name'Length) := Name;
            Index := Offered_Count;
            if Choice = 0 then
               Choice := Offered_Count;
            end if;
            Status := Offered;
         when CuBit.Fonts.Refused => Status := Refused;
         when CuBit.Fonts.Not_A_Font => Status := Not_A_Font;
      end case;
   end Offer;

   function Count return Face_Count is (Offered_Count);
   function Name (Index : Face_Index) return String is (Entries (Index).Text (1 .. Entries (Index).Length));

   function Find (Name : String) return Face_Count is
   begin
      for I in 1 .. Offered_Count loop
         if Entries (I).Text (1 .. Entries (I).Length) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Choose (Index : Face_Index) is
   begin
      Choice := Index;
   end Choose;

   function Chosen return Face_Count is (Choice);
   function Chosen_Face return CuBit.Fonts.Reading_Face is
     (if Choice = 0 then CuBit.Fonts.Reading_Face'First else Slot_Of (Choice));

end CuBit.Reading_Faces;
