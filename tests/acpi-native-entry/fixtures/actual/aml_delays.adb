with Interfaces;
package body AML_Delays with SPARK_Mode is
   function Normalize
     (Kind : Delay_Kind; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Normalization_Result
   is
      Low_Word_Mask : constant Interfaces.Unsigned_64 := 16#FFFF_FFFF#;
      Canonical : constant Interfaces.Unsigned_64 :=
        (if Width = AML_Decode.Bits_32 then Value and Low_Word_Mask else Value);
      Stall_Value : constant Interfaces.Unsigned_64 := Canonical and Low_Word_Mask;
   begin
      case Kind is
         when Sleep_Delay =>
            return (Status => Accepted,
                    Item => (Kind => Sleep_Delay,
                      Milliseconds => Sleep_Milliseconds
                        (Interfaces.Unsigned_64'Min
                          (Canonical, Interfaces.Unsigned_64 (Sleep_Milliseconds'Last)))));
         when Stall_Delay =>
            if Stall_Value > Interfaces.Unsigned_64 (Stall_Microseconds'Last) then
               return (Status => Invalid_Duration);
            end if;
            return (Status => Accepted,
                    Item => (Kind => Stall_Delay,
                             Microseconds => Stall_Microseconds (Stall_Value)));
      end case;
   end Normalize;
   procedure Unavailable_Provider (Item : Request; Result : out Outcome) is
      pragma Unreferenced (Item);
   begin
      Result := Unavailable;
   end Unavailable_Provider;
end AML_Delays;
