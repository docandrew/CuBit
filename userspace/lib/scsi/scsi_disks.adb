------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body SCSI_Disks with SPARK_Mode is

   --  READ CAPACITY(10) (SBC-3 5.16.2): last LBA, block length.
   Last_LBA_10      : constant := 0;
   Block_Length_10  : constant := 4;
   --  READ CAPACITY(16) (SBC-3 5.15.2): last LBA, block length.
   Last_LBA_16      : constant := 0;
   Block_Length_16  : constant := 8;

   procedure From_Capacity_10
     (Data : Bytes; Result : out Geometry; Outcome : out Capacity_Outcome)
   is
   begin
      Result := (others => <>);
      if Data'Length /= Capacity_10_Bytes then
         Outcome := Malformed_Capacity;
         return;
      end if;
      declare
         Last : constant Unsigned_32 := Big_Endian_32 (Data, Data'First + Last_LBA_10);
         Length : constant Unsigned_32 :=
           Big_Endian_32 (Data, Data'First + Block_Length_10);
      begin
         if Last = Capacity_10_Overflow then
            Outcome := Capacity_16_Required;
         elsif not Served (Length) then
            Outcome := Unsupported_Block_Size;
         else
            Result := (Blocks => Unsigned_64 (Last) + 1, Size => Size_Of (Length));
            Outcome := Capacity_Valid;
         end if;
      end;
   end From_Capacity_10;

   procedure From_Capacity_16
     (Data : Bytes; Result : out Geometry; Outcome : out Capacity_Outcome)
   is
   begin
      Result := (others => <>);
      if Data'Length < Capacity_16_Minimum then
         Outcome := Malformed_Capacity;
         return;
      end if;
      declare
         Last : constant Unsigned_64 := Big_Endian_64 (Data, Data'First + Last_LBA_16);
         Length : constant Unsigned_32 :=
           Big_Endian_32 (Data, Data'First + Block_Length_16);
      begin
         if Last = Unsigned_64'Last then
            Outcome := Malformed_Capacity;
         elsif not Served (Length) then
            Outcome := Unsupported_Block_Size;
         else
            Result := (Blocks => Last + 1, Size => Size_Of (Length));
            Outcome := Capacity_Valid;
         end if;
      end;
   end From_Capacity_16;
end SCSI_Disks;
