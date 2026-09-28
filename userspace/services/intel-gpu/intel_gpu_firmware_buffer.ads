with Interfaces;
with System;
package Intel_GPU_Firmware_Buffer is
   type Prepared_Buffer (Ready : Boolean := False) is record
      case Ready is
         when True =>
            DMA_Address, CPU_Address, Allocation_Bytes, Content_Bytes : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Prepared return Prepared_Buffer;
   -- Only published after complete copy/padding/readback and x86 cache flush.
   -- Addresses refer to
   -- retained backing, not GGTT placement. This does not establish device cache
   -- visibility, authentication, GPU ownership or permission to free memory.
   -- One-shot retained CPU buffer preparation. No GPU mapping,
   -- authentication, upload or submission is performed.
   function Prepare (Source : System.Address; Bytes : Interfaces.Unsigned_64)
     return String;
end Intel_GPU_Firmware_Buffer;
