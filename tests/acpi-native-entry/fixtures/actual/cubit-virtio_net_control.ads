with Interfaces;

--  Private devmgr -> virtio-net startup protocol. For a modern device its
--  register BAR is mapped by devmgr before startup; the message cannot
--  authorize arbitrary MMIO access. devmgr programs the MSI-X table itself
--  and installs the vector's interrupt capability in the driver's
--  manifest-named slot (docs/msi-interrupts.md): RX, TX and configuration
--  changes all use MSI-X entry MSIX_Entry.
package CuBit.Virtio_Net_Control with SPARK_Mode => On is
   use Interfaces;
   type Operation is (Configure_MSIX, Configure_Modern);
   for Operation use
     (Configure_MSIX => 16#0410#, Configure_Modern => 16#0411#);
   MSIX_Entry : constant Unsigned_16 := 0;
   --  Configure_MSIX (a legacy device) has one word: the I/O base.

   --  A modern (virtio 1.0) device: its register BAR, mapped from its
   --  start, at most Maximum_Modern_Bytes of it.
   Modern_Virtual_Address : constant Unsigned_64 := 16#0000_6000_0001_0000#;
   Maximum_Modern_Bytes   : constant := 65_536;
   --  Configure_Modern has three words, each two 32-bit halves (low, high):
   --  0: common configuration offset, device configuration offset;
   --  1: notification offset, notify_off_multiplier;
   --  2: bytes of the BAR mapped (low half; the high half is zero).
   function Low (Word : Unsigned_64) return Unsigned_64 is
     (Word and 16#FFFF_FFFF#);
   function High (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, 32));
   function Pair (Low, High : Unsigned_64) return Unsigned_64 is
     ((Low and 16#FFFF_FFFF#) or Shift_Left (High and 16#FFFF_FFFF#, 32));
end CuBit.Virtio_Net_Control;
