package body ACPI_Platform_Power_Completion with SPARK_Mode is
   function Classify (Item : Returned_Outcome) return S.Completion is
   begin
      -- No reply cannot establish the no-write guarantee of a decoded refusal.
      if not Item.Decoded then return S.Indeterminate; end if;
      -- Exhaustive list deliberately requires review when the ABI adds a reason.
      case Item.Reason is
         when ABI.Did_Not_Take_Effect => return S.Indeterminate;
         when ABI.Malformed | ABI.Unknown_Operation | ABI.Not_Permitted
            | ABI.Sleep_Type_Out_Of_Range | ABI.Not_Yet | ABI.No_Sleep_Registers
            | ABI.No_Reset_Register | ABI.Unsupported_Register | ABI.In_Progress =>
            return S.Not_Attempted;
      end case;
   end Classify;
end ACPI_Platform_Power_Completion;
