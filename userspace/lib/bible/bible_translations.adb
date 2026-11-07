with Ada.Unchecked_Deallocation;

package body Bible_Translations is
   use type Bible_Images.Open_Status;
   use type Bible_Images.Index_Status;

   procedure Free is new Ada.Unchecked_Deallocation (Bible_Images.Byte_Array, Image_Access);

   procedure Open (Item : in out Edition) is
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
      Item.Status := Loaded;
   end Open;

end Bible_Translations;
