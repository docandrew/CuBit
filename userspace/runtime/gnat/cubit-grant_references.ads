with Interfaces; use Interfaces;

--  Portable identity only: a reference is not authority or an address.
package CuBit.Grant_References with Pure, SPARK_Mode is
   Maximum_Slot : constant Unsigned_64 := 4095;
   Maximum_Generation : constant Unsigned_64 :=
     Unsigned_64 (Unsigned_32'Last);
   subtype Global_Slot is Unsigned_64 range 0 .. Maximum_Slot;
   subtype Generation is Unsigned_64 range 1 .. Maximum_Generation;
   type Reference is record
      slot : Global_Slot := 0;
      generation : Grant_References.Generation := 1;
   end record;
   --  Kernel query: zero = owned inactive slot, live generation otherwise;
   --  errors/foreign slots return U64'Last. Generations never wrap.
   function Retirement_Confirmed
     (Item : Reference; Observed : Unsigned_64) return Boolean is
     (Observed = 0 or else
      (Observed in Generation and then Observed > Item.generation));
end CuBit.Grant_References;
