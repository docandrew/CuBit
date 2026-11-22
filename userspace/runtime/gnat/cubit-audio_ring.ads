------------------------------------------------------------------------------
--  CuBit audio shared ring geometry
------------------------------------------------------------------------------
package CuBit.Audio_Ring with Pure is
   Header_Bytes : constant := 64;
   --  A power of two divides the U32 counter modulus: byte offsets remain
   --  continuous when the producer and consumer counters wrap at 2**32.
   Data_Bytes : constant := 2**13;
   Page_Count : constant := (Header_Bytes + Data_Bytes + 4095) / 4096;
   Allocation_Bytes : constant := Page_Count * 4096;
end CuBit.Audio_Ring;
