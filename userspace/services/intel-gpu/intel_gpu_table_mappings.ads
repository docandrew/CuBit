with Interfaces; use Interfaces;
package Intel_GPU_Table_Mappings is
   -- Retained CPU/DMA address pairs, not capabilities or ownership receipts.
   -- A common value type lets separately gated writers inspect the same
   -- caller-owned mapping view without a quota-sized conversion copy.
   type Page_Mapping is record
      CPU, DMA : Unsigned_64 := 0;
   end record;
   type Mappings is array (Positive range <>) of Page_Mapping;
end Intel_GPU_Table_Mappings;
