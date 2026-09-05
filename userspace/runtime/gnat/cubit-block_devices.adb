------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body CuBit.Block_Devices is
   function Pack_Sizes
     (logicalSize, physicalSize : Logical_Block_Size) return Unsigned_64
   is
   begin
      return Unsigned_64 (logicalSize) or
        Shift_Left (Unsigned_64 (physicalSize), 32);
   end Pack_Sizes;

   function Pack_Properties
     (features : Device_Features;
      media    : Media_Kind) return Unsigned_64
   is
   begin
      return Unsigned_64 (features and 16#FFFF_FFFF#) or
        Shift_Left (Unsigned_64 (Media_Kind'Pos (media)), 32) or
        Shift_Left (Unsigned_64 (PROTOCOL_VERSION), 48);
   end Pack_Properties;

   function Valid_Block_Size (value : Unsigned_64) return Boolean is
   begin
      return value in
        512 | 1024 | 2048 | 4096 | 8192 | 16_384 | 32_768 | 65_536;
   end Valid_Block_Size;

   function Decode_Description
     (word0, word1, word2, word3 : Unsigned_64;
      description : out Device_Description) return Boolean
   is
      logicalSize  : constant Unsigned_64 := word1 and 16#FFFF_FFFF#;
      physicalSize : constant Unsigned_64 := Shift_Right (word1, 32);
      mediaValue   : constant Unsigned_64 :=
        Shift_Right (word3, 32) and 16#FF#;
      version      : constant Unsigned_64 := Shift_Right (word3, 48);
   begin
      description := (others => <>);
      if word0 = 0 or else word2 = 0 or else
         word2 > Unsigned_64 (Unsigned_32'Last) or else
         not Valid_Block_Size (logicalSize) or else
         not Valid_Block_Size (physicalSize) or else
         physicalSize < logicalSize or else
         mediaValue > Unsigned_64 (Media_Kind'Pos (Media_Kind'Last)) or else
         (word3 and 16#0000_FF00_0000_0000#) /= 0 or else
         version /= Unsigned_64 (PROTOCOL_VERSION)
      then
         return False;
      end if;

      description.blockCount := word0;
      description.logicalBlockSize := Logical_Block_Size (logicalSize);
      description.physicalBlockSize := Logical_Block_Size (physicalSize);
      description.maxTransferBlocks := Unsigned_32 (word2);
      description.features := Device_Features (word3 and 16#FFFF_FFFF#);
      description.media := Media_Kind'Enum_Val (Natural (mediaValue));
      return True;
   end Decode_Description;

   function Is_Read_Only (description : Device_Description) return Boolean is
   begin
      return (description.features and FEATURE_READ_ONLY) /= 0;
   end Is_Read_Only;
end CuBit.Block_Devices;
