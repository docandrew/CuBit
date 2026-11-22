with Interfaces; use Interfaces;
package Intel_GPU_ADLN_MOCS with SPARK_Mode, Pure is
   subtype Entry_Index is Natural range 0 .. 63;
   subtype Register_Index is Natural range 0 .. 95;
   Uncached_Index : constant Entry_Index := 3;
   -- ADL-N uses the Gen12 (not TGL/RKL) global MOCS table. Unspecified
   -- entries inherit entry2; that does not make them client-usable indices.
   -- The last32 register values pack two16-bit L3 policies each.
   function Control (Index : Entry_Index) return Unsigned_32;
   function L3 (Index : Entry_Index) return Unsigned_32;
   function Offset (Index : Register_Index) return Unsigned_32
     with Post => Offset'Result mod 4 = 0 and then
       (Offset'Result in 16#4000# .. 16#40FC# or
        Offset'Result in 16#B020# .. 16#B09C#);
   function Value (Index : Register_Index) return Unsigned_32;
   -- Numeric plan only. Apply under exclusive GT ownership/forcewake before
   -- new GuC/engine memory transactions, not as a live cache-policy migration.
end Intel_GPU_ADLN_MOCS;
