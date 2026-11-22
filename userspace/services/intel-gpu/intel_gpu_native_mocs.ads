with Interfaces;
generic
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_MOCS is
   function Address_For (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_64;
   function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
end Intel_GPU_Native_MOCS;
