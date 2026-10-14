with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Devices;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with Startup;

--  CAP-001 diagnostics check: with no device-memory authority, ask the
--  kernel to map a kernel-image page and a bus page. Both must be refused,
--  and each refusal must come back as a "kernel: device-memory denied"
--  record (logstore) and on the Desktop bootstrap screen; the headless
--  test devmem-denials looks for both.
--
--  CAP-002: the authority is a device-memory handle. It holds none, so the
--  typed call passes the null handle; a word forged in the raw glue (its
--  own endpoint's word, and one with a generation never issued) is refused
--  the same way: a handle of another kind or none at all is no authority.
procedure Main is
   use ASCII;
   package ST renames CuBit.Startup_Tables;
   --  The first page above the legacy low megabyte: the kernel image.
   Kernel_Image_Page : constant Unsigned_64 := 16#10_0000#;
   --  Undescribed bus space in the 32-bit PCI hole.
   Bus_Page          : constant Unsigned_64 := 16#C000_0000#;
   --  An unused user address for the attempted mappings.
   Map_Target        : constant Unsigned_64 := 16#0000_5100_0000_0000#;
   One_Page          : constant Unsigned_64 := 1;
   --  MAP_DEVICE's raw ABI number and read-only flag (arguments: physical,
   --  virtual, pages, read-only, device-memory handle).
   Raw_Map_Device    : constant Unsigned_64 := 70;
   Raw_Read_Only     : constant Unsigned_64 := 1;
   Refused           : constant Unsigned_64 := Unsigned_64'Last;
   --  One generation step in a handle word (generation << 32 | slot).
   Generation_Step   : constant Unsigned_64 := 2 ** 32;
   Self_Word         : Unsigned_64;
   Kernel_Refused, Bus_Refused, Forged_Refused : Boolean;
begin
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("devmem-check: FAIL startup capabilities" & LF);
      return;
   end if;
   Kernel_Refused := not CuBit.Handles.Devices.Map_Device
     (CuBit.Handles.No_Device_Memory, Kernel_Image_Page, Map_Target, One_Page,
      Read_Only => True);
   Bus_Refused := not CuBit.Handles.Devices.Map_Device
     (CuBit.Handles.No_Device_Memory, Bus_Page, Map_Target, One_Page,
      Read_Only => True);
   --  Forged in the raw glue: its own endpoint (wrong kind), the same slot
   --  with a generation never issued, and an old-style slot number.
   Self_Word := CuBit.Startup_State.Granted (ST.Self_Text, ST.Endpoint);
   Forged_Refused :=
     syscall (Raw_Map_Device, Bus_Page, Map_Target, One_Page, Raw_Read_Only,
              Self_Word) = Refused
     and then syscall (Raw_Map_Device, Bus_Page, Map_Target, One_Page, Raw_Read_Only,
                       Self_Word + Generation_Step) = Refused
     and then syscall (Raw_Map_Device, Bus_Page, Map_Target, One_Page, Raw_Read_Only,
                       1) = Refused;
   if Kernel_Refused and then Bus_Refused and then Forged_Refused then
      debugPrint ("devmem-check: kernel-image and bus mappings refused" & LF);
   else
      debugPrint ("devmem-check: FAIL a device mapping was granted" & LF);
   end if;
end Main;
