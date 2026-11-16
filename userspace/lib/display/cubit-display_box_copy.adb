pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
package body CuBit.Display_Box_Copy is
   use type Interfaces.Unsigned_64, FD.DP.Pixel_Extent;
   function memcpy (Dest, Src : System.Address; N : Storage_Count) return System.Address
     with Import, Convention => C, External_Name => "memcpy";

   procedure Copy_Box
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      Box : FD.DP.Rectangle)
   is
      Ignore : System.Address;
      X : constant Natural := Natural (Box.X);
      Y : constant Natural := Natural (Box.Y);
      Row_Bytes : constant Storage_Count := Storage_Count (Box.Width) * 4;
   begin
      if Box.Width = 0 or else Box.Height = 0 then
         return;
      end if;
      --  Full-pitch spans are one copy (equal pitches, the whole row).
      if Target_Pitch = Source_Pitch and then X = 0 and then
        Storage_Count (Target_Pitch) = Row_Bytes
      then
         Ignore := memcpy (Target + Storage_Offset (Y * Target_Pitch),
                           Source + Storage_Offset (Y * Source_Pitch),
                           Row_Bytes * Storage_Count (Box.Height));
         return;
      end if;
      for Row in Y .. Y + Natural (Box.Height) - 1 loop
         Ignore := memcpy (Target + Storage_Offset (Row * Target_Pitch + X * 4),
                           Source + Storage_Offset (Row * Source_Pitch + X * 4),
                           Row_Bytes);
      end loop;
   end Copy_Box;

   procedure Copy_Boxes
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      Item : FD.Damage; Copied : out Interfaces.Unsigned_64) is
   begin
      Copied := 0;
      for I in 1 .. Item.Count loop
         Copy_Box (Target, Target_Pitch, Source, Source_Pitch, Item.Boxes (I));
         Copied := Copied + Bytes (Item.Boxes (I));
      end loop;
   end Copy_Boxes;
end CuBit.Display_Box_Copy;
