------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The physical pages a PCI function's memory decode covers: each memory
--  BAR's extent from its sizing probe, and a type-1 bridge's memory
--  windows, as page ranges for devmgr's claim ledger (Devmgr_Memory_Claims,
--  held by Device).
--
--  @description
--  devmgr sizes every memory BAR once, at its initial PCI enumeration and
--  before any driver starts: with the function's memory decode off it
--  writes all-ones to the BAR (both dwords of a 64-bit BAR), reads back
--  which address bits the BAR implements, and restores the BAR and the
--  command register. This package is the arithmetic, proved; the
--  configuration-space accesses stay in devmgr's glue.
--
--  * The size is the lowest address bit the BAR implements (PCI Local
--    Bus 3.0, 6.2.5.1: the read-back is ones from the top down to the
--    size). A 32-bit BAR's upper half is taken as all ones, so its size is
--    at most 4 GiB; a BAR that implements no address bit is unimplemented.
--  * An extent covers every page any byte of [Base, Base + Size) touches,
--    clamped to the kernel's physical limit (no envelope or data root lies
--    above it). A BAR at address 0 is unassigned (it is not claimed). A
--    BAR whose end wraps past 2 ** 64 is a malformed assignment: its
--    extent runs from its base to the limit (fail closed).
--  * A bridge's memory window and prefetchable window (PCI-to-PCI Bridge
--    1.2, 3.2.5.8 - 3.2.5.10) are [Base, Limit] with 1 MiB granularity;
--    base above limit is a closed window, base 0 an unassigned one.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Devmgr_Memory_Claims;

package PCI_BAR_Extents with Pure, SPARK_Mode is

   --  The claim ledger's pages and the kernel's physical limit, in pages.
   Page_Bytes : constant := Devmgr_Memory_Claims.Page_Bytes;
   Page_Limit : constant := Devmgr_Memory_Claims.Page_Limit;

   ---------------------------------------------------------------------------
   --  BAR register encoding (PCI Local Bus 3.0, 6.2.5.1).
   ---------------------------------------------------------------------------
   BAR_Space_IO      : constant Unsigned_32 := 16#0000_0001#;
   BAR_Type_Mask     : constant Unsigned_32 := 16#0000_0006#;
   BAR_Type_32       : constant Unsigned_32 := 16#0000_0000#;
   BAR_Type_64       : constant Unsigned_32 := 16#0000_0004#;
   BAR_Prefetchable  : constant Unsigned_32 := 16#0000_0008#;
   BAR_Address_Mask  : constant Unsigned_32 := 16#FFFF_FFF0#;
   --  What is written to size a BAR (both dwords of a 64-bit one).
   BAR_All_Ones      : constant Unsigned_32 := 16#FFFF_FFFF#;
   --  The expansion ROM BAR (6.2.5.2): its enable bit and address bits.
   ROM_Enable        : constant Unsigned_32 := 16#0000_0001#;
   ROM_Address_Mask  : constant Unsigned_32 := 16#FFFF_F800#;
   Dword_Bits        : constant := 32;
   Upper_Ones        : constant Unsigned_64 := 16#FFFF_FFFF_0000_0000#;

   type BAR_Kind is
     (IO_Space,       --  an I/O BAR: never a memory claim
      Memory_32,      --  a 32-bit memory BAR
      Memory_64,      --  the low dword of a 64-bit memory BAR (the next is its high dword)
      Memory_Other);  --  a reserved type (01b below 1 MiB, 11b): one dword, sized as 32-bit

   function Kind_Of (Low : Unsigned_32) return BAR_Kind is
     (if (Low and BAR_Space_IO) /= 0 then IO_Space
      elsif (Low and BAR_Type_Mask) = BAR_Type_32 then Memory_32
      elsif (Low and BAR_Type_Mask) = BAR_Type_64 then Memory_64
      else Memory_Other);

   function Prefetchable (Low : Unsigned_32) return Boolean is
     (Kind_Of (Low) /= IO_Space and then (Low and BAR_Prefetchable) /= 0);

   --  The registers a BAR at Low occupies: two for a 64-bit memory BAR.
   function Registers_Of (Low : Unsigned_32) return Positive is
     (if Kind_Of (Low) = Memory_64 then 2 else 1);

   --  A memory BAR's base address: the low dword's address bits, and the
   --  high dword for a 64-bit BAR.
   function Base_Of (Low, High : Unsigned_32) return Unsigned_64 is
     ((if Kind_Of (Low) = Memory_64 then Shift_Left (Unsigned_64 (High), Dword_Bits) else 0)
      or Unsigned_64 (Low and BAR_Address_Mask));

   --  The address bits the BAR implements, from the values read back after
   --  writing all-ones; a 32-bit BAR's upper half reads as all ones. Zero
   --  when it implements no address bit (it decodes nothing).
   function Address_Mask (Low_Probe, High_Probe : Unsigned_32; Wide : Boolean) return Unsigned_64 is
     (if Wide then
        Shift_Left (Unsigned_64 (High_Probe), Dword_Bits) or Unsigned_64 (Low_Probe and BAR_Address_Mask)
      elsif (Low_Probe and BAR_Address_Mask) = 0 then 0
      else Upper_Ones or Unsigned_64 (Low_Probe and BAR_Address_Mask));

   --  The expansion ROM BAR's implemented address bits, the same way.
   function ROM_Mask (Probe : Unsigned_32) return Unsigned_64 is
     (if (Probe and ROM_Address_Mask) = 0 then 0
      else Upper_Ones or Unsigned_64 (Probe and ROM_Address_Mask));

   --  The size a mask names: its lowest set bit, a power of two; 0 for no
   --  implemented address bit.
   function Size_Of (Mask : Unsigned_64) return Unsigned_64
   with Post =>
     (if Mask = 0 then Size_Of'Result = 0
      else Size_Of'Result /= 0
           and then (Size_Of'Result and (Size_Of'Result - 1)) = 0
           and then (Mask and Size_Of'Result) = Size_Of'Result
           and then (Mask and (Size_Of'Result - 1)) = 0);

   ---------------------------------------------------------------------------
   --  Extents
   ---------------------------------------------------------------------------
   type Extent_Status is
     (Sized,          --  [First_Page, End_Page) covers it
      Overflows,      --  its end wraps: [First_Page, Page_Limit), fail closed
      Bounded,        --  unprobed, no documented size: up to the next claim, fail closed
      Unimplemented,  --  no size (or a closed bridge window): decodes nothing
      Unassigned,     --  at address 0: not placed, not claimed
      Beyond_Limit);  --  wholly above the kernel's physical limit

   --  Pages [First_Page, End_Page).
   type Extent is record
      Status     : Extent_Status := Unimplemented;
      First_Page : Unsigned_64 := 0;
      End_Page   : Unsigned_64 := 0;
   end record;

   function Claimable (E : Extent) return Boolean is (E.Status in Sized | Overflows | Bounded);

   --  A claimable extent is a nonempty page range inside the limit.
   function Well_Formed (E : Extent) return Boolean is
     (if Claimable (E) then E.First_Page < E.End_Page and then E.End_Page <= Page_Limit
      else E.First_Page = 0 and then E.End_Page = 0);

   --  The pages [Base, Base + Size) touches, below the limit.
   function Extent_Of (Base, Size : Unsigned_64) return Extent
   with Post =>
     Well_Formed (Extent_Of'Result)
     and then (if Size = 0 then Extent_Of'Result.Status = Unimplemented
               elsif Base = 0 then Extent_Of'Result.Status = Unassigned
               elsif Base / Page_Bytes >= Page_Limit then Extent_Of'Result.Status = Beyond_Limit
               elsif Size - 1 > Unsigned_64'Last - Base then
                 Extent_Of'Result.Status = Overflows
                 and then Extent_Of'Result.First_Page = Base / Page_Bytes
                 and then Extent_Of'Result.End_Page = Page_Limit
               else
                 Extent_Of'Result.Status = Sized
                 and then Extent_Of'Result.First_Page = Base / Page_Bytes
                 --  Every page of it below the limit, its last one too.
                 and then Extent_Of'Result.End_Page =
                            Unsigned_64'Min ((Base + (Size - 1)) / Page_Bytes + 1, Page_Limit));

   ---------------------------------------------------------------------------
   --  Type-1 bridge windows (PCI-to-PCI Bridge 1.2, 3.2.5.8 - 3.2.5.10)
   ---------------------------------------------------------------------------
   --  Memory base/limit: bits 15:4 are address bits 31:20.
   Window_Address_Mask : constant Unsigned_16 := 16#FFF0#;
   Window_Shift        : constant := 16;
   Window_Granule      : constant := 16#10_0000#;   --  1 MiB
   --  Prefetchable base/limit: the low nibble says 64-bit (upper dwords).
   Window_Type_Mask    : constant Unsigned_16 := 16#000F#;
   Window_Type_64      : constant Unsigned_16 := 16#0001#;

   --  [Base, Limit] from a window's registers; Wide uses the upper dwords
   --  (a prefetchable window whose base register says 64-bit).
   function Window_Base (Base_Register : Unsigned_16; Upper : Unsigned_32; Wide : Boolean)
     return Unsigned_64 is
     ((if Wide then Shift_Left (Unsigned_64 (Upper), Dword_Bits) else 0)
      or Shift_Left (Unsigned_64 (Base_Register and Window_Address_Mask), Window_Shift));

   function Window_Limit (Limit_Register : Unsigned_16; Upper : Unsigned_32; Wide : Boolean)
     return Unsigned_64 is
     ((if Wide then Shift_Left (Unsigned_64 (Upper), Dword_Bits) else 0)
      or Shift_Left (Unsigned_64 (Limit_Register and Window_Address_Mask), Window_Shift)
      or (Window_Granule - 1));

   --  The pages a window [Base, Limit] covers; base above limit is closed.
   function Window_Extent (Base, Limit : Unsigned_64) return Extent
   with Post =>
     Well_Formed (Window_Extent'Result)
     and then (if Base > Limit then Window_Extent'Result.Status = Unimplemented
               elsif Base = 0 then Window_Extent'Result.Status = Unassigned
               else Window_Extent'Result = Extent_Of (Base, Limit - Base + 1));

   ---------------------------------------------------------------------------
   --  The active boot display: never probed
   ---------------------------------------------------------------------------
   --  Turning a display's memory decode off while the firmware scanout is
   --  live is not done. Display-class functions are sized after the scan:
   --  the one whose memory BAR holds the firmware framebuffer (the greatest
   --  BAR base at or below it), and every Intel display (Report_Intel_Display
   --  never probes either), get extents from what devmgr knows without
   --  probing; every other display function is probed as usual.
   Display_Class_Base : constant Unsigned_8 := 16#03#;
   Class_Base_Shift   : constant := 8;
   Intel_Vendor       : constant Unsigned_16 := 16#8086#;

   function Display_Class (Class_Code : Unsigned_16) return Boolean is
     (Unsigned_8 (Shift_Right (Class_Code, Class_Base_Shift)) = Display_Class_Base);

   type Base_Array is array (Positive range <>) of Unsigned_64;

   --  Which base holds the framebuffer at FB_First (FB_Bytes long): the
   --  greatest nonzero base at or below it; 0 when there is no framebuffer
   --  or no base is at or below it.
   function Scanout_Holder (Bases : Base_Array; FB_First, FB_Bytes : Unsigned_64) return Natural
   with Post =>
     (if Scanout_Holder'Result /= 0 then
        FB_Bytes /= 0
        and then Scanout_Holder'Result in Bases'Range
        and then Bases (Scanout_Holder'Result) /= 0
        and then Bases (Scanout_Holder'Result) <= FB_First
        and then (for all I in Bases'Range =>
                    (if Bases (I) /= 0 and then Bases (I) <= FB_First
                     then Bases (I) <= Bases (Scanout_Holder'Result)))
      else FB_Bytes = 0
           or else (for all I in Bases'Range => Bases (I) = 0 or else Bases (I) > FB_First));

   --  A BAR's documented size, for a device whose BARs are not probed; 0
   --  when none is documented here. Alder Lake-N GTTMMADR (BAR 0): 16 MiB
   --  (Intel_GPU_Resources); QEMU's standard VGA MMIO (BAR 2): 4 KiB.
   ADLN_First_Device  : constant Unsigned_16 := 16#46D0#;
   ADLN_Last_Device   : constant Unsigned_16 := 16#46D4#;
   ADLN_GTTMMADR_BAR  : constant := 0;
   ADLN_GTTMMADR_Size : constant := 16#100_0000#;
   QEMU_VGA_Vendor    : constant Unsigned_16 := 16#1234#;
   QEMU_VGA_Device    : constant Unsigned_16 := 16#1111#;
   QEMU_VGA_MMIO_BAR  : constant := 2;
   QEMU_VGA_MMIO_Size : constant := 16#1000#;

   function Documented_Size (Vendor, Device : Unsigned_16; BAR : Natural) return Unsigned_64 is
     (if Vendor = Intel_Vendor and then Device in ADLN_First_Device .. ADLN_Last_Device
        and then BAR = ADLN_GTTMMADR_BAR
      then ADLN_GTTMMADR_Size
      elsif Vendor = QEMU_VGA_Vendor and then Device = QEMU_VGA_Device
        and then BAR = QEMU_VGA_MMIO_BAR
      then QEMU_VGA_MMIO_Size
      else 0);

   --  The largest 32-bit memory BAR (bit 31 alone).
   Ceiling_32 : constant := 2 ** 31;

   --  An unprobed BAR with no documented size: from its base page up to
   --  the next claimed range (Next_Claim, a page), at most the largest size
   --  a BAR of its width can have (a 64-bit one: the physical limit), and
   --  at least its base page. Fail closed: no data envelope reaches into
   --  what may be the BAR.
   function Bounded_Extent (Base : Unsigned_64; Wide : Boolean; Next_Claim : Unsigned_64)
     return Extent
   with Post =>
     Well_Formed (Bounded_Extent'Result)
     and then (if Base = 0 then Bounded_Extent'Result.Status = Unassigned
               elsif Base / Page_Bytes >= Page_Limit then
                 Bounded_Extent'Result.Status = Beyond_Limit
               else
                 Bounded_Extent'Result.Status = Bounded
                 and then Bounded_Extent'Result.First_Page = Base / Page_Bytes
                 and then Bounded_Extent'Result.End_Page >= Base / Page_Bytes + 1
                 and then (Next_Claim <= Base / Page_Bytes + 1
                           or else Bounded_Extent'Result.End_Page <= Next_Claim)
                 and then (Wide
                           or else Bounded_Extent'Result.End_Page
                                     <= Base / Page_Bytes + Ceiling_32 / Page_Bytes));

end PCI_BAR_Extents;
