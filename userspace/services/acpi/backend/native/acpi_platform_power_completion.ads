with ACPI_Sleep_Entry;
with Platform_Power_ABI;
-- Post-call effect classification only. The native adapter must separately
-- enforce deadline, one-shot ownership, valid staging and kernel-object rights.
package ACPI_Platform_Power_Completion with SPARK_Mode is
   package ABI renames Platform_Power_ABI;
   package S renames ACPI_Sleep_Entry;
   use type S.Completion;
   use type ABI.Refusal;
   type Returned_Outcome (Decoded : Boolean := False) is record
      case Decoded is
         when True => Reason : ABI.Refusal := ABI.Malformed;
         when False => null;
      end case;
   end record;
   function Classify (Item : Returned_Outcome) return S.Completion
     with Global => null,
       Post => Classify'Result /= S.Completed and then
         (if not Item.Decoded then Classify'Result = S.Indeterminate
          elsif Item.Reason = ABI.Did_Not_Take_Effect then
            Classify'Result = S.Indeterminate
          else Classify'Result = S.Not_Attempted);
end ACPI_Platform_Power_Completion;
