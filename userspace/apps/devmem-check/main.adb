with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;

--  CAP-001 diagnostics check: with no device-memory authority, ask the
--  kernel to map a kernel-image page and a bus page. Both must be refused,
--  and each refusal must come back as a "kernel: device-memory denied"
--  record (logstore) and on the Desktop bootstrap screen; the headless
--  test devmem-denials looks for both.
procedure Main is
   use ASCII;
   --  The first page above the legacy low megabyte: the kernel image.
   Kernel_Image_Page : constant Unsigned_64 := 16#10_0000#;
   --  Undescribed bus space in the 32-bit PCI hole.
   Bus_Page          : constant Unsigned_64 := 16#C000_0000#;
   --  An unused user address for the attempted mappings.
   Map_Target        : constant Unsigned_64 := 16#0000_5100_0000_0000#;
   One_Page          : constant Unsigned_64 := 1;
   Read_Only         : constant Unsigned_64 := 1;
   Kernel_Refused, Bus_Refused : Boolean;
begin
   Kernel_Refused := syscall (SYSCALL_MAP_DEVICE, Kernel_Image_Page, Map_Target,
                              One_Page, Read_Only) = Unsigned_64'Last;
   Bus_Refused := syscall (SYSCALL_MAP_DEVICE, Bus_Page, Map_Target,
                           One_Page, Read_Only) = Unsigned_64'Last;
   if Kernel_Refused and then Bus_Refused then
      debugPrint ("devmem-check: kernel-image and bus mappings refused" & LF);
   else
      debugPrint ("devmem-check: FAIL a device mapping was granted" & LF);
   end if;
end Main;
