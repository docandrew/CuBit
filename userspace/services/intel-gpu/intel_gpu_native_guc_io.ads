with Interfaces;
generic
   -- Requires all fixed control pages mapped UC/NX, retained forcewake and
   -- exclusive startup ownership. A false result never authorizes an access.
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_GuC_IO is
   function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   -- Exposed for selector tests; zero means denied. No caller-selected alias.
   function Address_For (Offset : Interfaces.Unsigned_32; Writing : Boolean)
     return Interfaces.Unsigned_64;
   -- Success means CPU store completed under the ownership checks, not device
   -- acknowledgement. False after a store is ambiguous: retain all resources.
end Intel_GPU_Native_GuC_IO;
