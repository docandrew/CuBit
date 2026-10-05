------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Net_Addresses with SPARK_Mode is

   function Matches (A, Network : Address; Prefix : Prefix_Length) return Boolean is
      Bits : Natural;
      Mask : Unsigned_8;
   begin
      for I in Address_Index loop
         Bits := (if Prefix > 8 * I then Prefix - 8 * I else 0);
         Mask := (if Bits >= 8 then 16#FF# else Shift_Left (16#FF#, 8 - Bits));
         if (A (I) and Mask) /= (Network (I) and Mask) then
            return False;
         end if;
      end loop;
      return True;
   end Matches;

   Port_Bits    : constant := 16;
   Prefix_Shift : constant := 32;
   Action_Shift : constant := 40;
   Resolve_Bit  : constant := 48;
   Byte_Mask    : constant := 16#FF#;

   procedure Decode
     (Slot : Natural; Word_0, Word_1, Descriptor : Unsigned_64;
      Result : out Scope; Valid : out Boolean)
   is
      Prefix : constant Unsigned_64 := Shift_Right (Descriptor, Prefix_Shift) and Byte_Mask;
   begin
      Result := (Slot => Slot, others => <>);
      Valid := Prefix <= Address_Bits;
      if not Valid then
         return;
      end if;
      for I in 0 .. 7 loop
         Result.Network (I) := Unsigned_8 (Shift_Right (Word_0, 8 * I) and Byte_Mask);
         Result.Network (8 + I) := Unsigned_8 (Shift_Right (Word_1, 8 * I) and Byte_Mask);
      end loop;
      Result.First := Unsigned_16 (Descriptor and 16#FFFF#);
      Result.Last := Unsigned_16 (Shift_Right (Descriptor, Port_Bits) and 16#FFFF#);
      Result.Prefix := Natural (Prefix);
      Result.Action := Unsigned_8 (Shift_Right (Descriptor, Action_Shift) and Byte_Mask);
      Result.Resolve := (Shift_Right (Descriptor, Resolve_Bit) and 1) = 1;
   end Decode;

end CuBit.Libc_Net_Addresses;
