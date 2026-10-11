with Interfaces;

--  Private devmgr -> virtio-input startup protocol. devmgr maps the device's
--  modern (virtio 1.0) register BAR before startup and programs the MSI-X
--  table itself, for the vector in the driver's manifest-named slot
--  (docs/msi-interrupts.md); the message names offsets inside the BAR
--  mapping and cannot authorize any other MMIO access.
package CuBit.Virtio_Input_Control with SPARK_Mode => On is
   use Interfaces;
   type Operation is (Configure);
   for Operation use (Configure => 16#0418#);
   Modern_Virtual_Address : constant Unsigned_64 := 16#0000_6000_0001_0000#;
   Maximum_Modern_Bytes   : constant := 65_536;
   --  Event-queue ring and event buffers: one page (see virtio-input main).
   DMA_Pages : constant := 1;
   --  Configure has three words, each two 32-bit halves (low, high):
   --  0: common configuration offset, device configuration offset;
   --  1: notification offset, notify_off_multiplier;
   --  2: bytes of the BAR mapped (low half; the high half is zero).
   function Low (Word : Unsigned_64) return Unsigned_64 is
     (Word and 16#FFFF_FFFF#);
   function High (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, 32));
   function Pair (Low, High : Unsigned_64) return Unsigned_64 is
     ((Low and 16#FFFF_FFFF#) or Shift_Left (High and 16#FFFF_FFFF#, 32));
end CuBit.Virtio_Input_Control;
