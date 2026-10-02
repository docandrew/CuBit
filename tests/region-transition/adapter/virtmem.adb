package body Virtmem is
   function getNextTable (Table : PN; Index : PageTableIndex) return PhysAddress is
     (if Table (Index).present then Integer_Address (Table (Index).pgNum) * 4096 else 0);
end Virtmem;
