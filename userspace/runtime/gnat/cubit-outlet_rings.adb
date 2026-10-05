pragma Ada_2022;

package body CuBit.Outlet_Rings with SPARK_Mode is

   function U16 (Item : Bytes; At_Byte : Positive) return Natural is
     (Natural (Item (At_Byte)) + 256 * Natural (Item (At_Byte + 1)))
   with Pre => Item'First = 1 and then At_Byte < Item'Last;

   function U64 (Item : Bytes; At_Byte : Positive) return Unsigned_64 is
     (Unsigned_64 (Item (At_Byte))
      or Shift_Left (Unsigned_64 (Item (At_Byte + 1)), 8)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 2)), 16)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 3)), 24)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 4)), 32)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 5)), 40)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 6)), 48)
      or Shift_Left (Unsigned_64 (Item (At_Byte + 7)), 56))
   with Pre => Item'First = 1 and then Item'Length >= 8 and then At_Byte <= Item'Last - 7;

   procedure Put_64 (Item : in out Bytes; At_Byte : Positive; Value : Unsigned_64)
   with Pre => Item'First = 1 and then Item'Length >= 8 and then At_Byte <= Item'Last - 7;

   procedure Put_64 (Item : in out Bytes; At_Byte : Positive; Value : Unsigned_64) is
   begin
      for K in 0 .. 7 loop
         Item (At_Byte + K) := Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#);
      end loop;
   end Put_64;

   procedure Encode (T : Table; Item : out Bytes; Length : out Table_Length) is
   begin
      Item := [others => 0];
      Length := Length_Of (T);
      if T.Count = 0 then
         return;
      end if;
      Item (1 .. 4) := [Magic_0, Magic_1, Magic_2, Magic_3];
      Item (5) := Version;
      Item (7) := Unsigned_8 (T.Count);
      Put_64 (Item, 9, T.Owner);
      for E in 1 .. T.Count loop
         pragma Loop_Invariant (Item'First = 1);
         Item (Header_Bytes + (E - 1) * Entry_Bytes + 1) := Unsigned_8 (T.Entries (E).Outlet);
         Put_64 (Item, Header_Bytes + (E - 1) * Entry_Bytes + 9, T.Entries (E).Grant);
      end loop;
   end Encode;

   procedure Measure (Item : Bytes; Present : out Boolean; Length : out Table_Length) is
   begin
      Present := False;
      Length := 0;
      if Item'Length >= Header_Bytes
        and then Item (1) = Magic_0 and then Item (2) = Magic_1
        and then Item (3) = Magic_2 and then Item (4) = Magic_3
        and then U16 (Item, 7) in 1 .. Maximum_Entries
        and then Header_Bytes + U16 (Item, 7) * Entry_Bytes <= Item'Length
      then
         Present := True;
         Length := Header_Bytes + U16 (Item, 7) * Entry_Bytes;
      end if;
   end Measure;

   procedure Decode (Item : Bytes; T : out Table; Accepted : out Boolean) is
      Count : Natural;
   begin
      T := (Owner => 0, Entries => [others => (others => <>)], Count => 0);
      Accepted := False;
      if Item'Length < Header_Bytes
        or else Item (1) /= Magic_0 or else Item (2) /= Magic_1
        or else Item (3) /= Magic_2 or else Item (4) /= Magic_3
        or else U16 (Item, 5) /= Version
      then
         return;
      end if;
      Count := U16 (Item, 7);
      if Count not in 1 .. Maximum_Entries
        or else Item'Length /= Header_Bytes + Count * Entry_Bytes
      then
         return;
      end if;
      for E in 1 .. Count loop
         pragma Loop_Invariant (T.Count = 0);
         pragma Loop_Invariant
           (for all I in 1 .. E - 1 =>
              (for all J in 1 .. E - 1 =>
                 (if I /= J then T.Entries (I).Outlet /= T.Entries (J).Outlet)));
         declare
            Base : constant Natural := Header_Bytes + (E - 1) * Entry_Bytes;
         begin
            if (for some K in Base + 2 .. Base + 8 => Item (K) /= 0)
              or else (for some I in 1 .. E - 1 =>
                         T.Entries (I).Outlet = Natural (Item (Base + 1)))
            then
               T.Entries := [others => (others => <>)];
               return;
            end if;
            T.Entries (E) := (Outlet => Natural (Item (Base + 1)), Grant => U64 (Item, Base + 9));
         end;
      end loop;
      T.Owner := U64 (Item, 9);
      T.Count := Count;
      Accepted := True;
   end Decode;

end CuBit.Outlet_Rings;
