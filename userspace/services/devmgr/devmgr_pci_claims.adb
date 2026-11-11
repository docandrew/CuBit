pragma Ada_2022;
package body Devmgr_PCI_Claims with SPARK_Mode is
   function Complete (Item : Claim; Fact : Delivery) return Claim is
      Result : Claim := Item;
   begin
      Result.State := (case Fact is
        when Transferred => Granted_Lifetime_Unconfirmed,
        when Refused => Reply_Refused, when Uncertain => Reply_Uncertain);
      return Result;
   end Complete;
end Devmgr_PCI_Claims;
