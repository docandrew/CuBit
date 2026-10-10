pragma Ada_2022;

package body CuBit.Filesystem_Events with SPARK_Mode is

   Byte_Bits : constant := 8;

   function Valid_Relative (Name : Name_Bytes; Length : Name_Length) return Boolean is
      Component_First : Positive := 1;
   begin
      if Length = 0 then
         return False;
      end if;
      for I in 1 .. Length + 1 loop
         pragma Loop_Invariant (Component_First <= I);
         if I = Length + 1 or else Name (I) = Slash then
            --  The component Component_First .. I - 1.
            if I = Component_First
              or else (I - Component_First = 1 and then Name (Component_First) = Dot)
              or else (I - Component_First = 2 and then Name (Component_First) = Dot
                       and then Name (Component_First + 1) = Dot)
            then
               return False;
            end if;
            Component_First := I + 1;
         elsif Name (I) = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Relative;

   procedure Put (Into : in out Record_Bytes; At_Byte : Natural; Value : Unsigned_64; Width : Positive)
   with Pre => Width <= 8 and then At_Byte <= Header_Bytes - Width;
   procedure Put (Into : in out Record_Bytes; At_Byte : Natural; Value : Unsigned_64; Width : Positive) is
   begin
      for K in 0 .. Width - 1 loop
         Into (At_Byte + K) := Unsigned_8 (Shift_Right (Value, K * Byte_Bits) and 16#FF#);
      end loop;
   end Put;

   function Get (From : Record_Bytes; At_Byte : Natural; Width : Positive) return Unsigned_64
   with Pre => Width <= 8 and then At_Byte <= Header_Bytes - Width;
   function Get (From : Record_Bytes; At_Byte : Natural; Width : Positive) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for K in reverse 0 .. Width - 1 loop
         Value := Shift_Left (Value, Byte_Bits) or Unsigned_64 (From (At_Byte + K));
      end loop;
      return Value;
   end Get;

   procedure Encode
     (Item : Event; Name : Name_Bytes; Length : Name_Length;
      Into : out Record_Bytes; Used : out Record_Length) is
   begin
      Into := [others => 0];
      Put (Into, Watch_At, Unsigned_64 (Item.Watch), 4);
      Put (Into, Kind_At, Unsigned_64 (Kind_Codes (Item.Kind)), 1);
      Put (Into, Flags_At, Unsigned_64 (Item.Flags), 1);
      Put (Into, Name_Length_At, Unsigned_64 (Length), 2);
      Put (Into, Object_At, Item.Object, 8);
      Put (Into, Cookie_At, Item.Cookie, 8);
      Put (Into, Stamp_At, Item.Stamp, 8);
      for I in 1 .. Length loop
         Into (Name_At + I - 1) := Name (I);
      end loop;
      Used := Header_Bytes + Length;
   end Encode;

   procedure Decode
     (From : Record_Bytes; Used : Natural; Item : out Event;
      Name : out Name_Bytes; Length : out Name_Length; OK : out Boolean)
   is
      Watch : constant Unsigned_64 := Get (From, Watch_At, 4);
      Code  : constant Unsigned_8 := From (Kind_At);
      Flags : constant Unsigned_8 := From (Flags_At);
      Stored : constant Unsigned_64 := Get (From, Name_Length_At, 2);
      Kind : Event_Kind := Rescan_Needed;
      Known : Boolean := False;
   begin
      Item := (others => <>);
      Name := [others => 0];
      Length := 0;
      OK := False;
      for K in Event_Kind loop
         if Kind_Codes (K) = Code then
            Kind := K;
            Known := True;
         end if;
      end loop;
      if not Known or else Watch not in 1 .. Maximum_Watches
        or else (Flags and not Is_Directory) /= 0
        or else Stored > Maximum_Name_Bytes
        or else Used /= Header_Bytes + Natural (Stored)
      then
         return;
      end if;
      Length := Natural (Stored);
      for I in 1 .. Length loop
         Name (I) := From (Name_At + I - 1);
      end loop;
      if (if Named (Kind) then not Valid_Relative (Name, Length) else Length /= 0) then
         Length := 0;
         Name := [others => 0];
         return;
      end if;
      Item := (Watch => Natural (Watch), Kind => Kind, Flags => Flags,
               Object => Get (From, Object_At, 8), Cookie => Get (From, Cookie_At, 8),
               Stamp => Get (From, Stamp_At, 8));
      OK := True;
   end Decode;

end CuBit.Filesystem_Events;
