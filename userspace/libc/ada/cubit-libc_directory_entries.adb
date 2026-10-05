------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Directory_Entries with SPARK_Mode is

   procedure Header
     (P : Page; Valid : out Boolean; Count : out Entry_Count; Ended : out Boolean)
   is
      Declared : constant Unsigned_16 := U16 (P, Entry_Count_Offset);
   begin
      Valid := U16 (P, Header_Bytes_Offset) = Page_Header_Bytes
        and then U16 (P, Entry_Bytes_Offset) = Entry_Bytes
        and then Declared <= Maximum_Entries;
      Count := (if Valid then Natural (Declared) else 0);
      Ended := (P (Flags_Offset) and Page_End) /= 0;
   end Header;

   function Record_Bytes (P : Page; Index : Entry_Index) return Positive is
      Raw : constant Positive := Dirent_Name_Offset + Name_Length (P, Index) + 1;
   begin
      return (Raw + Record_Alignment - 1) / Record_Alignment * Record_Alignment;
   end Record_Bytes;

   procedure Encode
     (P : Page; Index : Entry_Index; Buffer : in out Bytes; Used : in out Natural;
      Fits : out Boolean)
   is
      Start : constant Page_Index := Entry_Start (Index);
      Length : constant Natural := Name_Length (P, Index);
      Size : constant Positive := Record_Bytes (P, Index);
      Hint : Unsigned_64 := 0;
      Kind : constant Unsigned_8 := P (Start + Kind_Offset);
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
      if Size > Buffer'Last + 1 - Used then
         Fits := False;
         return;
      end if;
      Fits := True;
      Base := Used;
      for K in reverse 0 .. 7 loop
         Hint := Shift_Left (Hint, 8) or Unsigned_64 (P (Start + K));
      end loop;
      Buffer (Base .. Base + Size - 1) := [others => 0];
      Put_64 (Base, (if Hint = 0 then 1 else Hint));                   --  d_ino
      Put_64 (Base + 8, Unsigned_64 (Base + Size));                    --  d_off
      Buffer (Base + 16) := Unsigned_8 (Size mod 256);                 --  d_reclen
      Buffer (Base + 17) := Unsigned_8 (Size / 256);
      Buffer (Base + 18) :=                                            --  d_type
        (case Kind is
           when Kind_File      => DT_REG,
           when Kind_Directory => DT_DIR,
           when Kind_Symlink   => DT_LNK,
           when others         => DT_UNKNOWN);
      for K in 0 .. Length - 1 loop
         Buffer (Base + Dirent_Name_Offset + K) := P (Start + Name_Offset + K);
      end loop;
      Used := Used + Size;
   end Encode;

end CuBit.Libc_Directory_Entries;
