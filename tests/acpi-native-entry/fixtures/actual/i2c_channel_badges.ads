pragma Ada_2022;
with Interfaces;
package I2C_Channel_Badges with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   subtype Hertz is Interfaces.Unsigned_32;
   use type Word;
   use type Hertz;
   type Address_Mode is (Seven_Bit, Ten_Bit);
   type Address_Value is range 0 .. 1023;
   Seven_Bit_Last : constant Address_Value := 127;
   type Target is record
      Mode : Address_Mode;
      Address : Address_Value;
      Maximum_Hz : Hertz;
   end record;
   Domain : constant Word := 16#4932#;
   Domain_Base : constant Word := 2 ** 48;
   Version : constant Word := 1;
   Version_Base : constant Word := 2 ** 44;
   Version_Radix : constant Word := 16;
   Reserved_Base : constant Word := 2 ** 43;
   Mode_Base : constant Word := 2 ** 42;
   Address_Base : constant Word := 2 ** 32;
   Address_Radix : constant Word := 1024;
   Prefix : constant Word := Domain * Domain_Base + Version * Version_Base;
   function Valid (Value : Target) return Boolean is
     (Value.Maximum_Hz /= 0 and then
      (Value.Mode = Ten_Bit or else Value.Address <= Seven_Bit_Last));
   function Tail (Badge : Word) return Word is
     (if Badge >= Prefix + Mode_Base then Badge - (Prefix + Mode_Base) else Badge - Prefix)
     with Pre => Badge >= Prefix and then Badge < Prefix + Reserved_Base,
       Post => Tail'Result < Mode_Base;
   function Valid_Word (Badge : Word) return Boolean is
     (Badge >= Prefix and then Badge < Prefix + Reserved_Base
      and then Tail (Badge) mod Address_Base /= 0
      and then (Badge >= Prefix + Mode_Base or else
        Tail (Badge) / Address_Base <= Word (Seven_Bit_Last)));
   function Canonical (Value : Target) return Word is
     (Prefix + (if Value.Mode = Ten_Bit then Mode_Base else 0)
      + Word (Value.Address) * Address_Base + Word (Value.Maximum_Hz));
   type Decode_Status is (Decoded, Wrong_Domain, Wrong_Version,
      Reserved_Bits, Zero_Speed, Invalid_Address);
   type Decoding (Status : Decode_Status := Wrong_Domain) is record
      case Status is
         when Decoded => Value : Target;
         when others => null;
      end case;
   end record;
   function Decode (Badge : Word) return Decoding
     with Post =>
       (Decode'Result.Status = Decoded) = Valid_Word (Badge)
       and then (if Decode'Result.Status = Decoded then
          Valid (Decode'Result.Value)
          and then Canonical (Decode'Result.Value) = Badge);
   type Encode_Status is (Encoded, Invalid_Target);
   type Encoding (Status : Encode_Status := Invalid_Target) is record
      case Status is
         when Encoded => Badge : Word;
         when others => null;
      end case;
   end record;
   function Encode (Value : Target) return Encoding
     with Post =>
       (Encode'Result.Status = Encoded) = Valid (Value)
       and then (if Encode'Result.Status = Encoded then
          Encode'Result.Badge = Canonical (Value)
          and then Decode (Encode'Result.Badge).Status = Decoded
          and then Decode (Encode'Result.Badge).Value = Value);
   -- Data representation only. A badge becomes authority only when stamped by
   -- the kernel on the admitted i2c endpoint. Never decode message words as a
   -- substitute for that authenticated stamp. Hardware/address policy is separate.
end I2C_Channel_Badges;
