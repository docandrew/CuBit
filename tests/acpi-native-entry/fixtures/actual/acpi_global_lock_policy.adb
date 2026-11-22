pragma Ada_2022;
package body ACPI_Global_Lock_Policy with SPARK_Mode is
   function Propose_Acquire (Observed : Lock_Word) return Acquire_Proposal is
      Busy : constant Boolean := (Observed and Owned_Bit) /= 0;
   begin
      return (Replacement => (Observed and Reserved_Bits) or Owned_Bit or
        (if Busy then Pending_Bit else 0), Would_Acquire => not Busy);
   end Propose_Acquire;
   function Propose_Release (Observed : Lock_Word) return Release_Proposal is
   begin
      return (Replacement => Observed and Reserved_Bits,
        Signal_Required => (Observed and Pending_Bit) /= 0);
   end Propose_Release;
end ACPI_Global_Lock_Policy;
