------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_RSN with SPARK_Mode is

   Version_Bytes : constant := 2;
   Count_Bytes   : constant := 2;

   function Suite_At (Data : Octets; At_Byte : Octet_Index) return Suite is
     ((IEEE => Data (At_Byte) = OUI_0 and then Data (At_Byte + 1) = OUI_1
               and then Data (At_Byte + 2) = OUI_2,
       Kind => Data (At_Byte + 3)))
   with Pre => Data'First = 0 and then At_Byte + Suite_Bytes <= Data'Length;

   function U16 (Data : Octets; At_Byte : Octet_Index) return Unsigned_16 is
     (Unsigned_16 (Data (At_Byte)) or Shift_Left (Unsigned_16 (Data (At_Byte + 1)), 8))
   with Pre => Data'First = 0 and then At_Byte + 2 <= Data'Length;

   procedure List (Data : Octets; Position : in out Natural; Items : out Suite_List;
                   Count : out Suite_Count; Result : out Status)
   with Pre => Data'First = 0 and then Data'Length <= 255 and then Position <= Data'Length,
        Post => Position <= Data'Length
   is
      N : Natural;
   begin
      Items := [others => (others => <>)];
      Count := 0;
      if Data'Length - Position < Count_Bytes then
         Result := Too_Short;
         return;
      end if;
      N := Natural (U16 (Data, Position));
      Position := Position + Count_Bytes;
      if N > Maximum_Suites then
         Result := Too_Many_Suites;
         return;
      elsif N * Suite_Bytes > Data'Length - Position then
         Result := Overrun;
         return;
      end if;
      for I in 1 .. N loop
         pragma Loop_Invariant (Position + (N - I + 1) * Suite_Bytes <= Data'Length);
         Items (I) := Suite_At (Data, Position);
         Position := Position + Suite_Bytes;
      end loop;
      Count := N;
      Result := Decoded;
   end List;

   procedure Decode (Data : Octets; Item : out Offer; Result : out Status) is
      Position : Natural;
   begin
      Item := (others => <>);
      if Data'Length < Version_Bytes + Suite_Bytes then
         Result := Too_Short;
         return;
      elsif U16 (Data, 0) /= RSN_Version then
         Result := Wrong_Version;
         return;
      end if;
      Item.Group := Suite_At (Data, Version_Bytes);
      Position := Version_Bytes + Suite_Bytes;
      List (Data, Position, Item.Pairwise, Item.Pairwise_Count, Result);
      if Result /= Decoded then
         return;
      end if;
      List (Data, Position, Item.AKMs, Item.AKM_Count, Result);
      if Result /= Decoded then
         return;
      end if;
      --  Capabilities are optional (absent means 0).
      if Data'Length - Position >= 2 then
         Item.Capabilities := U16 (Data, Position);
      end if;
      Result := Decoded;
   end Decode;

end Wifi_RSN;
