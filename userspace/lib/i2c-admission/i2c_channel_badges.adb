pragma Ada_2022;
package body I2C_Channel_Badges with SPARK_Mode is
   function Decode (Badge : Word) return Decoding is
      Mode : Address_Mode;
      Address : Address_Value;
      Speed : Hertz;
      Payload : Word;
   begin
      if Badge < Domain * Domain_Base or else Badge >= (Domain + 1) * Domain_Base
      then return (Status => Wrong_Domain); end if;
      if Badge < Prefix or else Badge >= Prefix + Version_Base
      then return (Status => Wrong_Version); end if;
      if Badge >= Prefix + Reserved_Base then return (Status => Reserved_Bits); end if;
      Payload := Tail (Badge);
      Speed := Hertz (Payload mod Address_Base);
      if Speed = 0 then return (Status => Zero_Speed); end if;
      Mode := (if Badge >= Prefix + Mode_Base then Ten_Bit else Seven_Bit);
      Address := Address_Value (Payload / Address_Base);
      if Mode = Seven_Bit and then Address > Seven_Bit_Last then return (Status => Invalid_Address); end if;
      pragma Assert (Word (Address) * Address_Base + Word (Speed) = Payload);
      return (Status => Decoded, Value => (Mode, Address, Speed));
   end Decode;
   function Encode (Value : Target) return Encoding is
   begin
      if not Valid (Value) then return (Status => Invalid_Target); end if;
      return (Status => Encoded, Badge => Canonical (Value));
   end Encode;
end I2C_Channel_Badges;
