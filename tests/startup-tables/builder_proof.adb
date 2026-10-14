pragma Ada_2022;
package body Builder_Proof with SPARK_Mode is
   procedure Mint
     (Into : in out Source; Item : Startup_Entry; Index : Entry_Index;
      Handle : out Unsigned_64; Outcome : out Mint_Outcome)
   is
      pragma Unreferenced (Item);
   begin
      if Into.Calls < Natural'Last then
         Into.Calls := Into.Calls + 1;
      end if;
      Handle := Unsigned_64 (Index);
      Outcome := (if Into.Calls mod 3 = 0 then Not_Available else Granted);
   end Mint;

   procedure Report
     (Into : in out Source; Item : Startup_Entry; Index : Entry_Index; Outcome : Mint_Outcome)
   is
      pragma Unreferenced (Into, Item, Index, Outcome);
   begin
      null;
   end Report;
end Builder_Proof;
