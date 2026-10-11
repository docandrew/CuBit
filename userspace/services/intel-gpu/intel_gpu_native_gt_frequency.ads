with Interfaces;
-- Writes to the GT frequency control page (Intel_GPU_Reset_Pages page 15,
-- 0xA000): GEN6_PMINTRMSK's ARAT bit for GuC SLPC, and a GEN6_RPNSWREQ
-- software request for the fallback (Intel_GPU_SLPC). Nothing else on the
-- page is writable here.
generic
   with function Owner_Ready return Boolean;
package Intel_GPU_Native_GT_Frequency is
   -- Clear ARAT_EXPIRED_INTRMSK, keep every other bit, read it back.
   procedure Unmask_ARAT (Before, After : out Interfaces.Unsigned_32;
                          Success : out Boolean);
   -- Write a ratio request (bits 31:23 only), read it back.
   procedure Request (Value : Interfaces.Unsigned_32;
                      Readback : out Interfaces.Unsigned_32;
                      Success : out Boolean);
end Intel_GPU_Native_GT_Frequency;
