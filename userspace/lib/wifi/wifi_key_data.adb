------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_Key_Data with SPARK_Mode is

   OUI_0 : constant Unsigned_8 := 16#00#;
   OUI_1 : constant Unsigned_8 := 16#0F#;
   OUI_2 : constant Unsigned_8 := 16#AC#;
   Element_Header : constant := 2;

   procedure Decode (Data : Octets; Item : out Contents; Result : out Status) is
      Position : Natural := 0;
      ID : Unsigned_8;
      Length : Natural;
   begin
      Item := (others => <>);
      Result := Decoded;
      while Position < Data'Length loop
         pragma Loop_Invariant
           (Result = Decoded and then Item.RSN_Element.First + Item.RSN_Element.Length <= Position);
         pragma Loop_Variant (Increases => Position);
         ID := Data (Position);
         --  Padding: 0xDD then zeros to the end.
         if ID = KDE_ID and then (Data'Length - Position = 1 or else Data (Position + 1) = 0) then
            return;
         elsif Data'Length - Position < Element_Header then
            Result := Overrun;
            return;
         end if;
         Length := Natural (Data (Position + 1));
         if Length > Data'Length - Position - Element_Header then
            Result := Overrun;
            return;
         end if;
         if ID = RSN_ID and then Item.RSN_Element.Length = 0 then
            Item.RSN_Element := (Position, Element_Header + Length);
         elsif ID = KDE_ID and then Length >= KDE_Header - Element_Header
           and then Data (Position + 2) = OUI_0 and then Data (Position + 3) = OUI_1
           and then Data (Position + 4) = OUI_2 and then Data (Position + 5) = KDE_Type_GTK
         then
            declare
               Key_Bytes : constant Integer := Length - (KDE_Header - Element_Header) - GTK_Prefix;
            begin
               if Key_Bytes < 1 or else Key_Bytes > Maximum_GTK or else Item.Has_GTK then
                  Result := GTK_Malformed;
                  return;
               end if;
               Item.Has_GTK := True;
               Item.GTK_Key_ID := Key_ID (Data (Position + KDE_Header) and Key_ID_Mask);
               Item.GTK_Bytes_Count := Key_Bytes;
               for I in 1 .. Key_Bytes loop
                  Item.GTK (I) := Data (Position + KDE_Header + GTK_Prefix + I - 1);
               end loop;
            end;
         end if;
         Position := Position + Element_Header + Length;
      end loop;
   end Decode;

end Wifi_Key_Data;
