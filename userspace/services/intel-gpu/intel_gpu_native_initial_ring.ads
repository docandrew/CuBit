with Interfaces;
with Intel_GPU_ADLN_Context_Init;
generic
   -- Includes exact retained backing identity, initialization, GPU mapping
   -- and runtime authority. Exclusive additionally means never scheduled.
   with function Owner_Ready return Boolean;
   with function Exclusive_Ready return Boolean;
package Intel_GPU_Native_Initial_Ring is
   procedure Publish (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Success : out Boolean);
   procedure Read_Marker (Value : out Interfaces.Unsigned_64;
                          OK : out Boolean);
   -- Reads the dedicated private-VM probe destination, not the ring HWSP.
   -- Caller must also verify ordered ring completion and scheduling disable.
   procedure Read_Batch_Result (Value : out Interfaces.Unsigned_64;
                                OK : out Boolean);
end Intel_GPU_Native_Initial_Ring;
