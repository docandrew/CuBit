with Interfaces;
generic
   with function Owner_Ready return Boolean;
   with function Status_GPU return Interfaces.Unsigned_64;
package Intel_GPU_Native_RCS_Start is
   function Write_Allowed (Offset, Value : Interfaces.Unsigned_32) return Boolean;
   function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
end Intel_GPU_Native_RCS_Start;
