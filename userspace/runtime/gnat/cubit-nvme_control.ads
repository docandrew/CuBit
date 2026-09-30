with Interfaces;

--  Private devmgr -> nvme.drv startup protocol: the first message the
--  driver receives, accepted only from the registered devmgr. The MSI-X
--  table lies in BAR0, which the driver maps itself (Maximum_BAR_Bytes of
--  it); the message cannot authorize any other MMIO access.
package CuBit.NVMe_Control with SPARK_Mode => On is
   use Interfaces;
   type Operation is (Configure_MSIX, Configure_Polled);
   for Operation use
     (Configure_MSIX => 16#0420#, Configure_Polled => 16#0421#);
   --  Configure_MSIX has two words: the MSI-X table offset in BAR0 and
   --  the IDT vector of entry zero, which the I/O completion queue uses.
   --  Configure_Polled has none: completions are polled.
   Maximum_BAR_Bytes : constant := 16_384;
   subtype Table_Offset is Unsigned_64 range 0 .. Maximum_BAR_Bytes - 16;
   --  The kernel has two MSI vector stubs: 49 is virtio-net's; 48 is
   --  shared with xHCI (every subscriber checks its own device on each
   --  notification) until a dedicated vector exists.
   Device_Vector : constant Unsigned_64 := 48;
end CuBit.NVMe_Control;
