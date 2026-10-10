------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Directory_Entries with SPARK_Mode is

   procedure Encode
     (P : DP.Page; Offset, Limit : Natural; Buffer : in out Bytes; Used : in out Natural;
      Fits, OK : out Boolean; Next : out Natural)
   is
      Item : DP.Facts;
      Name : DP.Name_Bytes;
      Length : DP.Name_Length;
      Size : Positive;
      Base : Natural;

      procedure Put_64 (At_Byte : Natural; Value : Unsigned_64)
      with Pre => Buffer'First = 0 and then Buffer'Last >= 7
                  and then At_Byte <= Buffer'Last - 7;
      procedure Put_64 (At_Byte : Natural; Value : Unsigned_64) is
      begin
         for K in 0 .. 7 loop
            Buffer (At_Byte + K) := Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#);
         end loop;
      end Put_64;
   begin
      Fits := False;
      DP.Get (P, Offset, Limit, Item, Name, Length, Next, OK);
      if not OK then
         return;
      end if;
      Size := Record_Bytes (Length);
      if Size > Buffer'Last + 1 - Used then
         return;
      end if;
      Fits := True;
      Base := Used;
      Buffer (Base .. Base + Size - 1) := [others => 0];
      Put_64 (Base, (if Item.Object = 0 then 1 else Item.Object));     --  d_ino
      Put_64 (Base + 8, Unsigned_64 (Base + Size));                    --  d_off
      Buffer (Base + 16) := Unsigned_8 (Size mod 256);                 --  d_reclen
      Buffer (Base + 17) := Unsigned_8 (Size / 256);
      Buffer (Base + 18) :=                                            --  d_type
        (case Item.Kind is
           when DP.Kind_File      => DT_REG,
           when DP.Kind_Directory => DT_DIR,
           when DP.Kind_Symlink   => DT_LNK,
           when others            => DT_UNKNOWN);
      for K in 1 .. Length loop
         Buffer (Base + Dirent_Name_Offset + K - 1) := Name (K);
      end loop;
      Used := Used + Size;
   end Encode;

end CuBit.Libc_Directory_Entries;
