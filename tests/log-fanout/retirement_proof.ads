with Interfaces;
with CuBit.Grant_References;
package Retirement_Proof with SPARK_Mode is
   procedure Check_Query
     (Item : CuBit.Grant_References.Reference;
      Observed : Interfaces.Unsigned_64) with Ghost;
end Retirement_Proof;
