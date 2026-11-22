pragma Ada_2022;
with Interfaces;
--  A data codec for one interrupt input on the endpoint's GPIO controller.
--  Only a kernel-stamped channel badge conveys authority. Message words,
--  process IDs and Decode do not create pin authority. The launcher validates
--  ACPI resources and the controller's supported pin plan before stamping.
package GPIO_Channel_Badges with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   type Pin_Number is range 0 .. 65_534;
   --  ACPI pin number, not the controller owner's internal slot. The ACPI
   --  16#FFFF# no-pin marker is never admitted.
   type Trigger_Mode is (Level_Triggered, Edge_Triggered);
   type Polarity is (Active_High, Active_Low, Active_Both);
   type Interrupt_Input is record
      Pin : Pin_Number;
      Trigger : Trigger_Mode;
      Active : Polarity;
   end record;
   function Valid (Value : Interrupt_Input) return Boolean is
     (Value.Trigger = Edge_Triggered or else Value.Active /= Active_Both);
   Domain : constant Word := 16#4750#;
   Domain_Base : constant Word := 2 ** 48;
   Version : constant Word := 1;
   Version_Base : constant Word := 2 ** 44;
   Pin_Radix : constant := 2 ** 16;
   No_Pin : constant := Pin_Radix - 1;
   Polarity_Radix : constant := 4;
   Trigger_Radix : constant := 2;
   Trigger_Base : constant := Pin_Radix * Polarity_Radix;
   Payload_Radix : constant := Trigger_Base * Trigger_Radix;
   Prefix : constant Word := Domain * Domain_Base + Version * Version_Base;
   type Payload_Value is range 0 .. Payload_Radix - 1;
   High_Code : constant Payload_Value := 0;
   Low_Code : constant Payload_Value := 1;
   Both_Code : constant Payload_Value := 2;
   Reserved_Polarity_Code : constant Payload_Value := 3;
   function Polarity_Code (Value : Polarity) return Payload_Value is
     (case Value is when Active_High => High_Code, when Active_Low => Low_Code,
        when Active_Both => Both_Code);
   function Canonical_Payload (Value : Interrupt_Input) return Payload_Value is
     (Payload_Value (Value.Pin) + Polarity_Code (Value.Active) * Pin_Radix
      + (if Value.Trigger = Edge_Triggered then Trigger_Base else 0));
   function Canonical (Value : Interrupt_Input) return Word is
     (Prefix + Word (Canonical_Payload (Value)));
   function Payload (Badge : Word) return Payload_Value is
     (Payload_Value (Badge - Prefix))
     with Pre => Badge >= Prefix and then Badge < Prefix + Payload_Radix,
       Post => Payload'Result = Payload_Value (Badge - Prefix)
         and then Word (Payload'Result) = Badge - Prefix;
   function Valid_Word (Badge : Word) return Boolean is
     (Badge >= Prefix and then Badge < Prefix + Payload_Radix
      and then Payload (Badge) mod Pin_Radix /= No_Pin
      and then (Payload (Badge) / Pin_Radix) mod Polarity_Radix <= Both_Code
      and then (Payload (Badge) >= Trigger_Base
         or else (Payload (Badge) / Pin_Radix) mod Polarity_Radix /= Both_Code));
   type Decode_Status is (Decoded, Wrong_Domain, Wrong_Version,
      Reserved_Bits, Missing_Pin, Reserved_Polarity, Invalid_Trigger);
   type Decoding (Status : Decode_Status := Wrong_Domain) is record
      case Status is
         when Decoded => Value : Interrupt_Input;
         when others => null;
      end case;
   end record;
   function Decode (Badge : Word) return Decoding
     with Global => null,
       Post => (Decode'Result.Status = Decoded) = Valid_Word (Badge)
       and then (if Decode'Result.Status = Decoded then
          Valid (Decode'Result.Value)
          and then Canonical (Decode'Result.Value) = Badge
          and then Payload_Value (Decode'Result.Value.Pin) =
            Payload (Badge) mod Pin_Radix
          and then Polarity_Code (Decode'Result.Value.Active) =
            (Payload (Badge) / Pin_Radix) mod Polarity_Radix
          and then (Decode'Result.Value.Trigger = Edge_Triggered) =
            (Payload (Badge) >= Trigger_Base));
   type Encode_Status is (Encoded, Invalid_Input);
   type Encoding (Status : Encode_Status := Invalid_Input) is record
      case Status is
         when Encoded => Badge : Word;
         when others => null;
      end case;
   end record;
   function Encode (Value : Interrupt_Input) return Encoding
     with Global => null,
       Post => (Encode'Result.Status = Encoded) = Valid (Value)
       and then (if Encode'Result.Status = Encoded then
          Encode'Result.Badge = Canonical (Value)
          and then Decode (Encode'Result.Badge).Status = Decoded
          and then Decode (Encode'Result.Badge).Value = Value);
   --  Domain63..48="GP", version47..44=1, reserved43..19=0, edge18,
   --  polarity17..16(high0/low1/both2), ACPI pin15..0.
   --  Controller identity is the endpoint lifetime, not an address in this word.
   --  Interrupt-input only: no GPIO output/register access, wake or share grant.
   --  Lease IDs and event tickets are separate, checked by GPIO_Pin_Leases.
end GPIO_Channel_Badges;
