------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body UFSHCI with SPARK_Mode is
   --  CAP fields (UFSHCI 5.2.1).
   Transfer_Slots_Mask : constant Register_Value := 16#1F#;       --  4:0
   Task_Slots_Shift    : constant := 16;                         --  18:16
   Task_Slots_Mask     : constant Register_Value := 16#7#;
   Auto_Hibernate_Bit  : constant Register_Value := 2 ** 23;
   Addressing_64_Bit   : constant Register_Value := 2 ** 24;
   Crypto_Bit          : constant Register_Value := 2 ** 28;

   function Decode_Capabilities (Value : Register_Value)
     return Controller_Capabilities is
     ((Transfer_Slots => Natural (Value and Transfer_Slots_Mask) + 1,
       Task_Slots     =>
         Natural ((Value / 2 ** Task_Slots_Shift) and Task_Slots_Mask) + 1,
       Auto_Hibernate => (Value and Auto_Hibernate_Bit) /= 0,
       Addressing_64  => (Value and Addressing_64_Bit) /= 0,
       Crypto         => (Value and Crypto_Bit) /= 0));

   --  VER (UFSHCI 5.2.3): major 15:8, minor 7:4, suffix 3:0.
   Major_Shift : constant := 8;
   Minor_Shift : constant := 4;
   Byte_Mask   : constant Register_Value := 16#FF#;
   Nibble_Mask : constant Register_Value := 16#F#;

   function Decode_Version (Value : Register_Value) return Interface_Version is
     ((Major  => Unsigned_8 ((Value / 2 ** Major_Shift) and Byte_Mask),
       Minor  => Unsigned_8 ((Value / 2 ** Minor_Shift) and Nibble_Mask),
       Suffix => Unsigned_8 (Value and Nibble_Mask)));
end UFSHCI;
