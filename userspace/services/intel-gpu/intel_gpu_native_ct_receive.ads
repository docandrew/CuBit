with Interfaces;
generic
   CPU_Base : Interfaces.Unsigned_64;
   -- Trusted retained 32KiB CT allocation, coherent CPU WB / GPU mappings,
   -- fixed CT_Setup layout, successful registration and running firmware.
   -- Serialized single receiver. Mapping must remain valid even after failure.
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_CT_Receive is
   procedure Read_Descriptor
     (Head, Tail, Status : out Interfaces.Unsigned_32; Success : out Boolean);
   procedure Read_Word
     (Index : Interfaces.Unsigned_32; Value : out Interfaces.Unsigned_32;
      Success : out Boolean);
   procedure Finish_Reads (Success : out Boolean);
   procedure Write_Head (Value : Interfaces.Unsigned_32; Success : out Boolean);
   procedure Make_Visible (Success : out Boolean);
   -- x86 barriers order coherent memory. They do NOT create coherency for
   -- an incorrectly mapped allocation. No CLFLUSH or whole-descriptor writes.
end Intel_GPU_Native_CT_Receive;
