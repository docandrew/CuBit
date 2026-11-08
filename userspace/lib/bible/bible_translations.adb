with Ada.Unchecked_Deallocation;

package body Bible_Translations is
   use type Bible_Images.Open_Status;
   use type Bible_Images.Index_Status;

   procedure Free is new Ada.Unchecked_Deallocation (Bible_Images.Byte_Array, Image_Access);

   function Default_Translation (Preferred : Language_Preference; Remembered : String) return Translation is
      Have_Remembered : Boolean := False;
      Remembered_Item : Translation := DEFAULT;
   begin
      for T in Translation loop
         if Abbreviation (T) = Remembered then
            Have_Remembered := True;
            Remembered_Item := T;
         end if;
      end loop;
      if Have_Remembered and then (not Preferred.Known or else Language_Of (Remembered_Item) = Preferred.Value) then
         return Remembered_Item;
      end if;
      if Preferred.Known then
         for T in Translation loop
            if Language_Of (T) = Preferred.Value then
               return T;
            end if;
         end loop;
      end if;
      return (if Have_Remembered then Remembered_Item else DEFAULT);
   end Default_Translation;

   --  The about table's text of Kind ("" when it has none).
   function About (Item : Edition; Kind : Bible_Images.About_Kind) return String is
      Slice : Bible_Images.Text_Slice;
      Valid : Boolean;
   begin
      Bible_Images.Read_About (Item.Data.all, Item.Layout, Kind, Slice, Valid);
      if not Valid or else Slice.Length = 0 then
         return "";
      end if;
      declare
         Result : String (1 .. Slice.Length);
      begin
         for I in Result'Range loop
            Result (I) := Character'Val (Item.Data (Slice.First + I - 1));
         end loop;
         return Result;
      end;
   end About;

   procedure Open (Item : in out Edition; Which : Translation) is
      Opened : Bible_Images.Open_Status;
      Read : Bible_Images.Index_Status;
   begin
      if Item.Data = null or else Item.Data'First /= 0 then
         Item.Status := Not_An_Image;
         Free (Item.Data);
         return;
      end if;
      Bible_Images.Open (Item.Data.all, Item.Layout, Opened);
      if Opened /= Bible_Images.Opened then
         Item.Status := Not_An_Image;
         Free (Item.Data);
         return;
      end if;
      Bible_Images.Index (Item.Data.all, Item.Layout, Item.Index, Read);
      if Read /= Bible_Images.Index_Read then
         Item.Status := Not_Indexed;
         Free (Item.Data);
         return;
      end if;
      if About (Item, Bible_Images.Abbreviation) /= Abbreviation (Which)
        or else About (Item, Bible_Images.Language_Code) /= ISO_639 (Language_Of (Which))
      then
         Item.Status := Wrong_Image;
         Free (Item.Data);
         return;
      end if;
      Item.Status := Loaded;
   end Open;

end Bible_Translations;
