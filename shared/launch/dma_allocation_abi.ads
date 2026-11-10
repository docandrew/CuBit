------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ALLOC_DMA's mode word and device binding (IOMMU-001): one definition for
--  the kernel and the device manager.
--
--  @description
--  ALLOC_DMA (74) arguments: arg0 the target process, arg1 the block order,
--  arg2 the target's virtual address, arg3 this mode word, arg4 the
--  physical ceiling (0: none), arg5 a process handle with RIGHT_GRANT for
--  the target, arg6 (device-bound requests only) the caller's CAP_IOPORT
--  covering the PCI configuration mechanism (ports 0CF8h .. 0CFFh, write).
--
--  Mode word: bits 7:0 the allocation mode; bit 8 Device: the block is DMA
--  memory for one PCI function, whose requester ID is bits 31:16 (bus 15:8,
--  device 7:3, function 2:0 of the field). Every other bit is zero.
--
--  A device-bound block is mapped into that function's IOMMU domain only
--  (identity: the device uses the returned physical address), and the
--  domain is bound to the target process; a domain bound to another live
--  process refuses it. Holding the configuration-mechanism ports is the
--  authority: it already lets the holder program any function's command
--  register, so naming a function's domain widens nothing. Without an
--  IOMMU (no DMAR, or the function's unit not translating) the block is
--  allocated unconfined, as before; the kernel reports that state.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package DMA_Allocation_ABI with SPARK_Mode, Pure is

   Mode_Mask          : constant Unsigned_64 := 16#FF#;
   --  Freed with the owner.
   Mode_Ordinary      : constant Unsigned_64 := 0;
   --  Kept until reboot (retained DMA).
   Mode_Retained      : constant Unsigned_64 := 1;
   --  Retained, mapped as one 2 MiB page (order 9 only).
   Mode_Retained_Large : constant Unsigned_64 := 3;

   Device_Flag     : constant Unsigned_64 := 16#100#;
   Requester_Shift : constant := 16;
   Requester_Mask  : constant Unsigned_64 := 16#FFFF#;

   --  Bits that must be zero.
   Reserved_Bits : constant Unsigned_64 := 16#FFFF_FFFF_0000_FE00#;
   pragma Compile_Time_Error
     (Reserved_Bits /= not (Mode_Mask or Device_Flag or Requester_Mask * 2 ** Requester_Shift),
      "reserved bits are the rest of the mode word");

   subtype Requester_ID is Unsigned_16;
   subtype Bus_Number is Unsigned_8;
   subtype Device_Number is Unsigned_8 range 0 .. 31;
   subtype Function_Number is Unsigned_8 range 0 .. 7;

   function Requester
     (Bus : Bus_Number; Device : Device_Number; Func : Function_Number)
      return Requester_ID is
     (Requester_ID (Bus) * 256 + Requester_ID (Device) * 8 + Requester_ID (Func));

   function Device_Mode (Mode : Unsigned_64; Who : Requester_ID) return Unsigned_64 is
     ((Mode and Mode_Mask) or Device_Flag
      or Shift_Left (Unsigned_64 (Who), Requester_Shift));

   function Mode_Of (Word : Unsigned_64) return Unsigned_64 is (Word and Mode_Mask);
   function Is_Device (Word : Unsigned_64) return Boolean is
     ((Word and Device_Flag) /= 0);
   function Requester_Of (Word : Unsigned_64) return Requester_ID is
     (Requester_ID (Shift_Right (Word, Requester_Shift) and Requester_Mask));
   function Well_Formed (Word : Unsigned_64) return Boolean is
     ((Word and Reserved_Bits) = 0);

   --  The PCI configuration mechanism the authority must cover.
   PCI_Config_First_Port : constant := 16#CF8#;
   PCI_Config_Ports      : constant := 8;
end DMA_Allocation_ABI;
