with Interfaces; use Interfaces;
with CuBit.Grant_References; use CuBit.Grant_References;
package body Retirement_Proof with SPARK_Mode is
   procedure Check_Query (Item : Reference; Observed : Unsigned_64) is
   begin
      pragma Assert (not Retirement_Confirmed (Item, Unsigned_64'Last));
      pragma Assert (not Retirement_Confirmed (Item, Item.generation));
      pragma Assert (Retirement_Confirmed (Item, 0));
      if Retirement_Confirmed (Item, Observed) then
         pragma Assert
           (Observed = 0 or else
            (Observed <= Maximum_Generation and Observed > Item.generation));
      end if;
   end Check_Query;
end Retirement_Proof;
