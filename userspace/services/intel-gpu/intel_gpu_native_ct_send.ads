with Interfaces;
generic
   CPU_Base : Interfaces.Unsigned_64;
   -- Retained, coherent CPU WB / GPU CT allocation with successful
   -- registration. Serialized single producer; mapping survives failure.
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_CT_Send is
   procedure Read_Descriptor
     (Head, Tail, Status : out Interfaces.Unsigned_32; Success : out Boolean);
   procedure Write_Word
     (Index, Value : Interfaces.Unsigned_32; Success : out Boolean);
   procedure Write_Tail (Value : Interfaces.Unsigned_32; Success : out Boolean);
   procedure Make_Visible (Success : out Boolean);
   -- Orders coherent memory only. Does not establish DMA coherency itself.
   -- Never modifies firmware head/status or flushes a shared cache line.
end Intel_GPU_Native_CT_Send;
