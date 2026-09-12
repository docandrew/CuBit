with Interfaces;

--  Private devmgr -> virtio-net startup protocol. The table page is mapped by
--  devmgr before startup; the message cannot authorize arbitrary MMIO access.
package CuBit.Virtio_Net_Control with SPARK_Mode => On is
   use Interfaces;
   type Operation is (Configure_MSIX);
   for Operation use (Configure_MSIX => 16#0410#);
   Table_Virtual_Address : constant Unsigned_64 := 16#0000_6000_0000_0000#;
   subtype Table_Offset is Unsigned_64 range 0 .. 4096 - 16;
   Device_Vector : constant Unsigned_64 := 49;
   --  Configure_MSIX has three words: I/O base, page-relative table offset,
   --  and IDT vector. Entry zero is shared by RX, TX, and config changes.
end CuBit.Virtio_Net_Control;
