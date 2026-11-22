with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
package Intel_GPU_ADLN_Golden with SPARK_Mode is
   type Class_Values is array (Natural range 0 .. 15) of Unsigned_32;
   type Reservation is record
      Valid : Boolean := False;
      Bytes : Unsigned_64 := 0;
      Addresses, State_Bytes : Class_Values := [others => 0];
   end record;
   function Required_Bytes (Description : Intel_GPU_ADLN_Inventory.Inventory)
     return Unsigned_64;
   function Plan (Description : Intel_GPU_ADLN_Inventory.Inventory;
                  GPU_Base, Capacity : Unsigned_64) return Reservation
     with Post => (if Plan'Result.Valid then Plan'Result.Bytes <= Capacity);
   -- One full context per enabled CLASS, not per engine. Addresses name the
   -- full images; State_Bytes excludes HWSP+80 DWORDs (4416 bytes) on ADL-N.
   -- This reserves numeric extents only. No context has been captured, no GPU
   -- mapping established, and firmware recovery must remain disabled until
   -- the late golden-state initialization and recovery path are ready.
end Intel_GPU_ADLN_Golden;
