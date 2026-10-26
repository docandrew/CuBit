------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Installer_Protocol with SPARK_Mode is

   Byte_Bits : constant := 8;

   --  Disk_Record.
   Blocks_At     : constant := 0;
   Block_Size_At : constant := 8;
   Identity_At   : constant := 16;
   --  Install_Request.
   Disk_At          : constant := 0;
   Ratio_At         : constant := 4;
   ESP_At           : constant := 8;
   Source_Length_At : constant := 12;
   Confirm_Blocks_At : constant := 16;
   Confirm_Size_At  : constant := 24;
   Confirmation_At  : constant := 32;
   Source_At        : constant := Confirmation_At + DId.Record_Bytes;

   pragma Compile_Time_Error (Source_At + Maximum_Source > Page_Bytes, "install request too big");

   procedure Put (Data : in out Page; At_Byte : Natural; Value : Unsigned_64; Width : Positive)
     with Pre => Width <= 8 and then At_Byte <= Page_Bytes - Width;
   procedure Put (Data : in out Page; At_Byte : Natural; Value : Unsigned_64; Width : Positive)
   is
   begin
      for I in 0 .. Width - 1 loop
         Data (At_Byte + 1 + I) := Unsigned_8 (Shift_Right (Value, Byte_Bits * I) and 16#FF#);
      end loop;
   end Put;

   function Get (Data : Page; At_Byte : Natural; Width : Positive) return Unsigned_64
     with Pre => Width <= 8 and then At_Byte <= Page_Bytes - Width;
   function Get (Data : Page; At_Byte : Natural; Width : Positive) return Unsigned_64
   is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Width - 1 loop
         Value := Shift_Left (Value, Byte_Bits) or Unsigned_64 (Data (At_Byte + 1 + I));
      end loop;
      return Value;
   end Get;

   function Encode_Disk (Item : Disk_Record) return Page is
      Result : Page := [others => 0];
   begin
      Put (Result, Blocks_At, Item.Blocks, 8);
      Put (Result, Block_Size_At, Unsigned_64 (Item.Block_Size), 4);
      Result (Identity_At + 1 .. Identity_At + DId.Record_Bytes) := Byte_Array (DId.Encode (Item.Identity));
      return Result;
   end Encode_Disk;

   procedure Decode_Disk (Data : Page; Item : out Disk_Record; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Item.Blocks := Get (Data, Blocks_At, 8);
      Item.Block_Size := Unsigned_32 (Get (Data, Block_Size_At, 4) mod 2 ** 32);
      DId.Decode (DId.Byte_Array (Data (Identity_At + 1 .. Identity_At + DId.Record_Bytes)),
                  Item.Identity, Valid);
   end Decode_Disk;

   function Encode_Install (Item : Install_Request) return Page is
      Result : Page := [others => 0];
   begin
      Put (Result, Disk_At, Unsigned_64 (Item.Disk), 4);
      Put (Result, Ratio_At, Unsigned_64 (Item.Ratio), 4);
      Put (Result, ESP_At, Unsigned_64 (Item.ESP_Size), 4);
      Put (Result, Source_Length_At, Unsigned_64 (Item.Length), 2);
      Put (Result, Confirm_Blocks_At, Item.Confirm_Blocks, 8);
      Put (Result, Confirm_Size_At, Unsigned_64 (Item.Confirm_Block_Size), 4);
      Result (Confirmation_At + 1 .. Confirmation_At + DId.Record_Bytes) :=
        Byte_Array (DId.Encode (Item.Confirmation));
      for I in 1 .. Item.Length loop
         Result (Source_At + I) := Character'Pos (Item.Source (I));
      end loop;
      return Result;
   end Encode_Install;

   procedure Decode_Install (Data : Page; Item : out Install_Request; Valid : out Boolean) is
      Disk : constant Unsigned_64 := Get (Data, Disk_At, 4);
      Ratio : constant Unsigned_64 := Get (Data, Ratio_At, 4);
      ESP : constant Unsigned_64 := Get (Data, ESP_At, 4);
      Length : constant Unsigned_64 := Get (Data, Source_Length_At, 2);
   begin
      Item := (others => <>);
      Valid := Disk <= Unsigned_64 (Disk_Ordinal'Last)
        and then Ratio in Unsigned_64 (Inode_Ratio'First) .. Unsigned_64 (Inode_Ratio'Last)
        and then ESP in Unsigned_64 (ESP_MiB'First) .. Unsigned_64 (ESP_MiB'Last)
        and then Length in 1 .. Maximum_Source;
      if not Valid then
         return;
      end if;
      Item.Disk := Disk_Ordinal (Disk);
      Item.Ratio := Inode_Ratio (Ratio);
      Item.ESP_Size := ESP_MiB (ESP);
      Item.Length := Source_Length (Length);
      Item.Confirm_Blocks := Get (Data, Confirm_Blocks_At, 8);
      Item.Confirm_Block_Size := Unsigned_32 (Get (Data, Confirm_Size_At, 4) mod 2 ** 32);
      for I in 1 .. Item.Length loop
         if Data (Source_At + I) not in Character'Pos (' ') .. Character'Pos ('~') then
            Valid := False;
            return;
         end if;
         Item.Source (I) := Character'Val (Data (Source_At + I));
      end loop;
      DId.Decode (DId.Byte_Array (Data (Confirmation_At + 1 .. Confirmation_At + DId.Record_Bytes)),
                  Item.Confirmation, Valid);
   end Decode_Install;

end CuBit.Installer_Protocol;
