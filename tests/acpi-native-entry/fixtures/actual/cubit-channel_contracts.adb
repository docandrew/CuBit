------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Channel_Contracts with SPARK_Mode is

   Size_Bits    : constant := 16#FFFF_FFFF#;
   Bounded_Bit  : constant := 32;
   Version_Shift : constant := 48;
   Byte_Mask    : constant := 16#FF#;
   Policy_Shift : constant := 8;
   Pages_Shift  : constant := 16;
   Buffers_Shift : constant := 24;
   Buffers_Mask : constant := 16#FFFF#;
   Rule_Shift   : constant := 40;

   function Encode (Item : Contract) return Words is
     [0 => Unsigned_64 (Item.Element.Identity),
      1 => Unsigned_64 (Item.Element.Wire_Size)
           or (if Item.Element.Sizing = Bounded_Size
               then Shift_Left (1, Bounded_Bit) else 0)
           or Shift_Left (Unsigned_64 (Item.Element.Version), Version_Shift),
      2 => Unsigned_64 (Channel_Kind'Enum_Rep (Item.Kind))
           or Shift_Left (Unsigned_64 (Channel_Policy'Enum_Rep (Item.Policy)), Policy_Shift)
           or Shift_Left (Unsigned_64 (Item.Pages), Pages_Shift)
           or Shift_Left (Unsigned_64 (Item.Buffers), Buffers_Shift)
           or Shift_Left (Unsigned_64 (Read_Rule'Enum_Rep (Item.Rule)), Rule_Shift)];

   procedure Decode (Item : Words; Result : out Contract; Accepted : out Boolean) is
      Kind_Value : constant Unsigned_64 := Item (2) and Byte_Mask;
      Policy_Value : constant Unsigned_64 := Shift_Right (Item (2), Policy_Shift) and Byte_Mask;
      Pages_Value : constant Unsigned_64 := Shift_Right (Item (2), Pages_Shift) and Byte_Mask;
      Buffers_Value : constant Unsigned_64 :=
        Shift_Right (Item (2), Buffers_Shift) and Buffers_Mask;
      Rule_Value : constant Unsigned_64 := Shift_Right (Item (2), Rule_Shift) and Byte_Mask;
   begin
      Result := (others => <>);
      Accepted := False;
      if Kind_Value not in 1 .. 3 or else Policy_Value not in 1 .. 3
        or else Pages_Value not in 1 .. Unsigned_64 (Ring_Pages'Last)
        or else Buffers_Value not in 1 .. Unsigned_64 (Buffer_Count'Last)
        or else Rule_Value not in 1 .. 2
      then
         return;
      end if;
      Result :=
        (Element =>
           (Identity  => Schema_Id (Item (0)),
            Version   => Protocol_Version (Shift_Right (Item (1), Version_Shift)),
            Sizing    => (if (Shift_Right (Item (1), Bounded_Bit) and 1) = 1
                          then Bounded_Size else Fixed_Size),
            Wire_Size => Unsigned_32 (Item (1) and Size_Bits)),
         Kind    => (if Kind_Value = 1 then Queue elsif Kind_Value = 2 then Arena else Duplex),
         Policy  => (if Policy_Value = 1 then Lossless
                     elsif Policy_Value = 2 then Drop_Oldest else Shed_Newest),
         Pages   => Ring_Pages (Pages_Value),
         Buffers => Buffer_Count (Buffers_Value),
         Rule    => (if Rule_Value = 1 then Copy_Then_Validate else In_Place));
      --  Canonical only: any bit the encoding would not produce is refused.
      Accepted := Valid (Result) and then Encode (Result) = Item;
      if not Accepted then
         Result := (others => <>);
      end if;
   end Decode;

end CuBit.Channel_Contracts;
