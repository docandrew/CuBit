pragma Ada_2022;
package body GPIO_Channel_Badges with SPARK_Mode is
   procedure Lemma_Payload_Cancellation (Bits : Payload_Value)
     with Ghost, Global => null,
       Post => Payload (Prefix + Word (Bits)) = Bits;
   procedure Lemma_Payload_Cancellation (Bits : Payload_Value) is
      Badge : constant Word := Prefix + Word (Bits);
   begin
      pragma Assert (Badge >= Prefix and then Badge < Prefix + Payload_Radix);
      pragma Assert (Badge - Prefix = Word (Bits));
      pragma Assert (Payload (Badge) = Payload_Value (Badge - Prefix));
      pragma Assert (Payload_Value (Word (Bits)) = Bits);
   end Lemma_Payload_Cancellation;
   function Decode (Badge : Word) return Decoding is
      Bits, Tail, Active_Code : Payload_Value;
      Value : Interrupt_Input;
   begin
      if Badge < Domain * Domain_Base
        or else Badge >= (Domain + 1) * Domain_Base
      then return (Status => Wrong_Domain); end if;
      if Badge < Prefix or else Badge >= Prefix + Version_Base
      then return (Status => Wrong_Version); end if;
      if Badge >= Prefix + Payload_Radix then
         return (Status => Reserved_Bits);
      end if;
      Bits := Payload (Badge);
      Tail := (if Bits >= Trigger_Base then Bits - Trigger_Base else Bits);
      if Tail mod Pin_Radix = No_Pin then
         return (Status => Missing_Pin);
      end if;
      Active_Code := Tail / Pin_Radix;
      if Active_Code = Reserved_Polarity_Code then
         return (Status => Reserved_Polarity);
      end if;
      Value := (Pin => Pin_Number (Tail mod Pin_Radix),
         Trigger => (if Bits >= Trigger_Base then Edge_Triggered
                     else Level_Triggered),
         Active => (case Active_Code is when High_Code => Active_High,
            when Low_Code => Active_Low, when others => Active_Both));
      if not Valid (Value) then return (Status => Invalid_Trigger); end if;
      pragma Assert (Active_Code < Polarity_Radix);
      pragma Assert (Polarity_Code (Value.Active) = Active_Code);
      pragma Assert (Payload_Value (Value.Pin) + Active_Code * Pin_Radix = Tail);
      pragma Assert (Bits = Tail +
         (if Value.Trigger = Edge_Triggered then Trigger_Base else 0));
      pragma Assert (Canonical (Value) = Badge);
      return (Status => Decoded, Value => Value);
   end Decode;
   function Encode (Value : Interrupt_Input) return Encoding is
      Bits : constant Payload_Value := Canonical_Payload (Value);
      Badge : constant Word := Canonical (Value);
   begin
      if not Valid (Value) then return (Status => Invalid_Input); end if;
      pragma Assert (Bits mod Pin_Radix = Payload_Value (Value.Pin));
      pragma Assert ((Bits / Pin_Radix) mod Polarity_Radix =
         Polarity_Code (Value.Active));
      pragma Assert ((Bits >= Trigger_Base) = (Value.Trigger = Edge_Triggered));
      pragma Assert (Badge >= Prefix and then Badge < Prefix + Payload_Radix);
      pragma Assert (Badge - Prefix = Word (Bits));
      Lemma_Payload_Cancellation (Bits);
      pragma Assert (Payload (Badge) = Bits);
      pragma Assert (Valid_Word (Badge));
      return (Status => Encoded, Badge => Badge);
   end Encode;
end GPIO_Channel_Badges;
