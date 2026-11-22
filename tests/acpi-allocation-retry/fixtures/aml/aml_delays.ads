with AML_Decode;
package AML_Delays with SPARK_Mode, Pure is
   type Delay_Kind is (Sleep_Delay, Stall_Delay);
   type Sleep_Milliseconds is range 0 .. 2_000;
   -- ACPICA compatibility ceiling; ACPI specifies at most 100 microseconds.
   type Stall_Microseconds is range 0 .. 255;
   type Request (Kind : Delay_Kind := Sleep_Delay) is record
      case Kind is
         when Sleep_Delay => Milliseconds : Sleep_Milliseconds := 0;
         when Stall_Delay => Microseconds : Stall_Microseconds := 0;
      end case;
   end record;
   type Outcome is (Completed, Unavailable, Failed);
   type Normalization_Status is (Accepted, Invalid_Duration);
   type Normalization_Result (Status : Normalization_Status := Invalid_Duration) is record
      case Status is
         when Accepted => Item : Request;
         when Invalid_Duration => null;
      end case;
   end record;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Width;
   Word_Modulus : constant AML_Decode.Integer_Value := 2 ** 32;
   function Normalize
     (Kind : Delay_Kind; Width : AML_Decode.Integer_Width;
      Value : AML_Decode.Integer_Value) return Normalization_Result
     with Global => null,
       Post =>
         (if Kind = Sleep_Delay then
            Normalize'Result.Status = Accepted
            and then Normalize'Result.Item.Kind = Sleep_Delay
            and then AML_Decode.Integer_Value (Normalize'Result.Item.Milliseconds) =
              AML_Decode.Integer_Value'Min
                ((if Width = AML_Decode.Bits_32 then Value mod Word_Modulus else Value),
                 AML_Decode.Integer_Value (Sleep_Milliseconds'Last))
          else
            (Normalize'Result.Status = Accepted) =
              (Value mod Word_Modulus <= AML_Decode.Integer_Value (Stall_Microseconds'Last))
            and then
              (if Normalize'Result.Status = Accepted then
                 Normalize'Result.Item.Kind = Stall_Delay
                 and then AML_Decode.Integer_Value (Normalize'Result.Item.Microseconds) =
                   Value mod Word_Modulus));
   procedure Unavailable_Provider (Item : Request; Result : out Outcome)
     with Global => null, Post => Result = Unavailable;
end AML_Delays;
