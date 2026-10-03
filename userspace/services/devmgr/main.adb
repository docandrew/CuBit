------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Device Manager
--
--  Responsible for PCI scanning, service spawning, capability granting,
--  and startup ordering. Replaces the kernel-side policy in modules.adb.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Capability_Grants;
with Native_GPU_Probe_Protocol;
with Boot_RTC;
with CCL.Configurations;
with CCL.Declarations;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Network_Authority;
with CuBit.Devices;
with CuBit.Virtio_Net_Control;
with CuBit.NVMe_Control;
with CuBit.Filesystems;
with CuBit.Memory_Grants;
with CuBit.File_Access;
with Cpio;
with XHCI_DMA_Layout;
with XHCI_Capabilities;
with Intel_GPU_Probe;
with Intel_GPU_Display_Pages;
with Intel_GPU_PHY_Pages;
with Intel_GPU_Resources;
with Intel_GPU_Reset_Pages;
with Intel_GPU_Display_Claim;
with Intel_GPU_Boot;
with Intel_GPU_PCI_Power;
with Intel_GPU_ADS_Backing;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Record_Growth;
with Intel_GPU_Extent_Growth;
with Intel_GPU_Allocation_Growth;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Directory;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_PCI_Interrupts;
with Intel_GPU_PCI_IRQ_Disable;
with Intel_GPU_GGTT_Access;
with Intel_GPU_Broker_Request;
with Intel_Render_Broker;

procedure main is
   use ASCII;

   --  IPC labels (must match kernel/src/ipc_labels.ads)
   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   REPLY_ERR : constant Unsigned_32 := 16#F001#;

   --  Sysinfo / driver ID constants
   DRIVER_KEYBOARD : constant Unsigned_64 := 1;
   DRIVER_ATA      : constant Unsigned_64 := 2;
   DRIVER_NETSTACK : constant Unsigned_64 := 3;
   DRIVER_PROCMGR  : constant Unsigned_64 := 4;
   DRIVER_NVME     : constant Unsigned_64 := 5;
   DRIVER_FS       : constant Unsigned_64 := 6;
   DRIVER_DEVMGR   : constant Unsigned_64 := 7;
   DRIVER_HDA      : constant Unsigned_64 := 8;
   DRIVER_MIXER    : constant Unsigned_64 := 9;
   DRIVER_MOUSE    : constant Unsigned_64 := 10;
   DRIVER_CONFIG   : constant Unsigned_64 := 11;
   DRIVER_NETMGR   : constant Unsigned_64 := 12;
   DRIVER_DESKTOP  : constant Unsigned_64 := 15;
   DRIVER_GPU      : constant Unsigned_64 := 17;

   --  (SERVICE_FILESYSTEM_PID removed: now uses dynamic PID assignment
   --  and kernel well-known service registry)

   --  Address where kernel mapped the initrd into our space
   INITRD_BASE : constant Unsigned_64 := 16#0000_5000_0000_0000#;

   --  DMA virtual base for drivers
   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;

   --  PCI config I/O ports
   PCI_CONFIG_ADDR : constant Unsigned_16 := 16#0CF8#;
   PCI_CONFIG_DATA : constant Unsigned_16 := 16#0CFC#;

   --  PCI class codes
   CLASS_STORAGE_IDE   : constant Unsigned_16 := 16#0101#;
   CLASS_STORAGE_SATA  : constant Unsigned_16 := 16#0106#;
   CLASS_STORAGE_NVME  : constant Unsigned_16 := 16#0108#;
   CLASS_NET_ETHERNET  : constant Unsigned_16 := 16#0200#;
   CLASS_MULTIMEDIA_HDA : constant Unsigned_16 := 16#0403#;
   CLASS_DISPLAY_VGA    : constant Unsigned_16 := 16#0300#;
   CLASS_SERIAL_USB     : constant Unsigned_16 := 16#0C03#;

   --  PCI vendor ID for virtio devices
   VENDOR_VIRTIO       : constant Unsigned_16 := 16#1AF4#;
   DEVICE_VIRTIO_GPU    : constant Unsigned_16 := 16#1050#;

   --  PCI config space offsets
   PCI_VENDOR_ID       : constant Unsigned_8 := 0;
   PCI_COMMAND         : constant Unsigned_8 := 4;
   PCI_STATUS          : constant Unsigned_8 := 6;
   PCI_PROG_IF         : constant Unsigned_8 := 9;
   PCI_CLASS_DEVICE    : constant Unsigned_8 := 10;
   PCI_HEADER_TYPE     : constant Unsigned_8 := 14;
   PCI_BASEADDR_0      : constant Unsigned_8 := 16;
   PCI_BASEADDR_1      : constant Unsigned_8 := 20;
   PCI_CAP_PTR         : constant Unsigned_8 := 52;
   PCI_INTERRUPT_LINE  : constant Unsigned_8 := 60;
   PCI_CAP_ID_MSI      : constant Unsigned_8 := 5;
   PCI_CAP_ID_MSIX     : constant Unsigned_8 := 16#11#;
   PCI_HEADER_MULTIFUNCTION : constant Unsigned_8 := 16#80#;
   PCI_PROG_IF_AHCI         : constant Unsigned_8 := 16#01#;
   PCI_PROG_IF_XHCI         : constant Unsigned_8 := 16#30#;

   --  VirtIO modern PCI capability types
   PCI_CAP_ID_VENDOR_SPECIFIC : constant Unsigned_8 := 9;
   VIRTIO_PCI_CAP_COMMON_CFG  : constant Unsigned_8 := 1;
   VIRTIO_PCI_CAP_NOTIFY_CFG  : constant Unsigned_8 := 2;
   VIRTIO_PCI_CAP_ISR_CFG     : constant Unsigned_8 := 3;
   VIRTIO_PCI_CAP_DEVICE_CFG  : constant Unsigned_8 := 4;

   --  Capability types (must match kernel/src/capabilities.ads)
   CAP_ENDPOINT     : constant Unsigned_64 := 1;
   CAP_NOTIFICATION : constant Unsigned_64 := 2;
   CAP_IOPORT       : constant Unsigned_64 := 4;
   CAP_IRQ          : constant Unsigned_64 := 5;
   CAP_PROCESS      : constant Unsigned_64 := 6;
   CAP_DEVICE_MEM   : constant Unsigned_64 := 7;
   CAP_CSPACE       : constant Unsigned_64 := 10;
   CAP_SCHEDULING   : constant Unsigned_64 := 11;

   --  Rights bitmask (must match kernel/src/capabilities.ads)
   RIGHT_READ    : constant Unsigned_64 := 1;
   RIGHT_WRITE   : constant Unsigned_64 := 2;
   RIGHT_EXECUTE : constant Unsigned_64 := 4;
   RIGHT_GRANT   : constant Unsigned_64 := 8;

   --  MAP_INTO flags
   MAP_FLAG_RW : constant Unsigned_64 := 0;
   MAP_FLAG_RO : constant Unsigned_64 := 1;
   MAP_FLAG_IO : constant Unsigned_64 := 2;

   --  PCI device info (found during scanning)
   type PCIDeviceInfo is record
      found    : Boolean := False;
      bus      : Unsigned_8 := 0;
      slot     : Unsigned_8 := 0;
      func     : Unsigned_8 := 0;
   end record;

   function Same_Location
      (left, right : PCIDeviceInfo) return Boolean
   is
     (left.found and then right.found and then
      left.bus = right.bus and then left.slot = right.slot and then
      left.func = right.func);

   type Inventory_Device is record
      location  : PCIDeviceInfo;
      vendorID  : Unsigned_16 := 0;
      deviceID  : Unsigned_16 := 0;
      classCode : Unsigned_16 := 0;
      progIf    : Unsigned_8 := 0;
      kind      : CuBit.Devices.Device_Kind := CuBit.Devices.Other_Device;
   end record;
   subtype Inventory_Index is
     Positive range 1 .. CuBit.Devices.MAX_PCI_DEVICES;
   type Inventory_Array is array (Inventory_Index) of Inventory_Device;
   inventory : Inventory_Array := [others => (others => <>)];
   inventoryCount : Natural range 0 .. CuBit.Devices.MAX_PCI_DEVICES := 0;
   inventoryOverflow : Boolean := False;

   type XHCI_Diagnostic_Snapshot is record
      valid             : Boolean := False;
      decodedReports    : Unsigned_64 := 0;
      motionReports     : Unsigned_64 := 0;
      buttonTransitions : Unsigned_32 := 0;
      completionErrors  : Unsigned_32 := 0;
      lastReport        : Unsigned_32 := 0;
      lastLength        : Unsigned_8 := 0;
      lastCompletion    : Unsigned_8 := 0;
      interruptMode     : CuBit.Devices.Interrupt_Mode :=
        CuBit.Devices.Interrupt_Polling;
   end record;
   xhciDiagnostics : XHCI_Diagnostic_Snapshot;

   nvmeDev   : PCIDeviceInfo;
   ataDev    : PCIDeviceInfo;
   netDev    : PCIDeviceInfo;
   hdaDev    : PCIDeviceInfo;
   xhciDev   : PCIDeviceInfo;
   gpuDev    : PCIDeviceInfo;
   intelDev : PCIDeviceInfo;
   Intel_Inspection_PID : Unsigned_64 := 0;
   package GP renames Native_GPU_Probe_Protocol;
   package CG renames CuBit.Capability_Grants;
   -- These slots remain reserved for the broker's retained lifetime. Slot27
   -- holds RTC port authority; slot31 is GPU control and slot57 the viewer.
   function GPU_Launch_Reply_Slot (ID : Positive) return CapabilitySlot is
     (if ID <= 11 then CapabilitySlot (15 + ID)
      elsif ID <= 14 then CapabilitySlot (16 + ID)
      elsif ID = 15 then 32 else 56);
   package GPU_Launch is new Intel_Render_Broker
     (31, 16#4750_1000#, 16#4750_1FFF#, GPU_Launch_Reply_Slot);
   GPU_Launches : GPU_Launch.Broker;
   Launch_Receipt : aliased CompletionEntry;
   Request_Found, Receipt_Consumed : Boolean;
   Launch_Now : Unsigned_64;
   Launch_Activity : Activity_Result;
   GPU_Viewer_PID : Unsigned_64 := 0;
   GPU_Viewer_Target : CG.Recipient;
   GPU_Viewer_Ready, GPU_Viewer_Desktop_Bound : Boolean := False;
   --  Static bring-up ownership: no general PCI changes after handoff. The
   --  one-shot IRQ-disable executor below has its own exact word allowlist.
   --  No runtime suspend/rebind is supported while this claim exists. Keep
   --  it frozen even if the child exits; recovery requires a device lifecycle.
   Intel_Config_Frozen : Boolean := False;
   Intel_Claim_Base : Unsigned_64 := 0;
   Intel_Claim_Identity : Unsigned_32 := 0;
   Intel_Forcewake_Granted : Boolean := False;
   Intel_Reset_Granted : array (Intel_GPU_Reset_Pages.Page_Index) of Boolean := [others => False];
   Intel_Reset_Authorized : Boolean := False;
   Intel_Display_Ownership : Intel_GPU_Display_Claim.Claim;
   Intel_Display_Granted : array (Intel_GPU_Display_Pages.Page_Index) of Boolean := [others => False];
   Intel_PHY_Granted : array (Intel_GPU_PHY_Pages.Page_Index) of Boolean := [others => False];
   Intel_GGTT_Granted : Boolean := False;
   Intel_GGTT_BAR, Intel_GGTT_Bytes : Unsigned_64 := 0;
   Intel_GGTT_Write_Attempted : Boolean := False;
   Intel_Buffer_Attempted : Boolean := False;
   Intel_Buffer_Physical : Unsigned_64 := 0;
   Intel_ADS_Attempted : Boolean := False;
   Intel_ADS_Physical : Unsigned_64 := 0;
   function Context_Allocation_Ready return Boolean is
     (Intel_Inspection_PID /= 0 and then Intel_Reset_Authorized and then
      Intel_Config_Frozen and then Intel_GGTT_Granted);
   function Allocate_Buffer_Block (CPU : Unsigned_64) return Unsigned_64;
   package Buffer_Allocations is new Intel_GPU_Extent_Allocator
     (Context_Allocation_Ready, Allocate_Buffer_Block);
   Intel_Buffer_Pool : Buffer_Allocations.Pool;
   function Allocate_Buffer_Block (CPU : Unsigned_64) return Unsigned_64 is
     (syscall (SYSCALL_ALLOC_DMA, Intel_Inspection_PID,
       Intel_GPU_Physical_Extents.Allocation_Order, CPU, 3,
       Buffer_Allocations.DMA_Ceiling (Intel_Buffer_Pool)));
   package Intel_Extent_Growth is new Intel_GPU_Extent_Growth
     (Buffer_Allocations, Intel_Buffer_Pool, Intel_GPU_Metadata_Platform.Storage,
      Context_Allocation_Ready);
   Intel_Buffer_Arena_Granted : Boolean := False;
   -- Metadata policy is independent of BO/RAM size. Reserve VA lazily and
   -- commit only demanded chunks; this is not a preallocated million objects.
   Intel_Metadata_Quota : constant Unsigned_64 := 64 * 1024 * 1024;
   Intel_Record_Quota : constant Positive := 1_048_576;
   Intel_Allocation_Reply_Slot : constant CapabilitySlot := 58;
   function Intel_Record_Capacity return Positive is
     (Buffer_Allocations.Record_Capacity (Intel_Buffer_Pool));
   procedure Publish_Intel_Records
     (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Buffer_Allocations.Extend_Records (Intel_Buffer_Pool, Base, Bytes, Accepted);
   end Publish_Intel_Records;
   package Intel_Record_Growth is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Intel_Record_Capacity, Publish_Intel_Records);
   function Save_Intel_Allocation_Reply return Boolean is
     (saveReplyCap (Unsigned_64 (Intel_Allocation_Reply_Slot)) = 1);
   procedure Acquire_Intel_Buffer
     (Index : Positive; Pages : Intel_GPU_Buffer_Backing.Page_Count;
      Generation : Unsigned_32; Buffer : out Intel_GPU_Buffer_Reply.Extent_View;
      Success, Pending : out Boolean) is
   begin
      Intel_Extent_Growth.Step
        (Intel_Inspection_PID, Index, Pages, Generation, Buffer, Success, Pending);
   end Acquire_Intel_Buffer;
   procedure Reply_Intel_Allocation
     (Index : Positive; Generation : Unsigned_32;
      Buffer : Intel_GPU_Buffer_Reply.Extent_View; Success : Boolean) is
      Response : Message :=
        (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0]);
      Delivery : Unsigned_64;
      pragma Unreferenced (Delivery);
   begin
      if Success then
         Intel_Buffer_Arena_Granted := True;
         Response :=
           (tag => (16#F004#, 4, 0, 0), authorityTag => 0,
            words => [Intel_GPU_Buffer_Reply.CPU_Address (Buffer),
              Intel_GPU_Buffer_Reply.Byte_Count (Buffer), Intel_Inspection_PID,
              Intel_GPU_Buffer_Backing.Allocation_Key (Index, Generation)]);
      end if;
      -- Delivery failure consumes the saved capability, not the allocation.
      -- Retained backing must not be recycled on ambiguous client delivery.
      Delivery := replyCap (Intel_Allocation_Reply_Slot, Response);
   end Reply_Intel_Allocation;
   package Intel_Allocation_Growth is new Intel_GPU_Allocation_Growth
     (Intel_Record_Growth, Context_Allocation_Ready, Save_Intel_Allocation_Reply,
      Acquire_Intel_Buffer, Reply_Intel_Allocation);
   Intel_Allocations : Intel_Allocation_Growth.Dispatcher;
   gpuIsPrimary : Boolean := False;

   --  Service PIDs
   filesystemPID : Unsigned_64 := 0;
   ataPID        : Unsigned_64 := 0;
   ramdiskPID    : Unsigned_64 := 0;
   nvmePID       : Unsigned_64 := 0;
   netstackPID   : Unsigned_64 := 0;
   virtioNetPID  : Unsigned_64 := 0;
   netMSIXCap : Unsigned_8 := 0;
   netTableOffset : Unsigned_64 := 0;
   netIOBase : Unsigned_64 := 0;
   --  A modern (virtio 1.0) device: its register layout within the mapped
   --  BAR (CuBit.Virtio_Net_Control.Configure_Modern).
   netModern : Boolean := False;
   netCommonOff, netDeviceOff, netNotifyOff, netNotifyMult : Unsigned_64 := 0;
   netModernBytes : Unsigned_64 := 0;
   netTransportReady : Boolean := False;
   DEVMGR_NET_SLOT : constant Unsigned_64 := 3;
   --  NVMe completion interrupts (filesystem agent): MSI-X entry zero, set
   --  up masked here, filled by nvme.drv, unmasked after its reply.
   nvmeMSIXCap : Unsigned_8 := 0;
   nvmeTableOffset : Unsigned_64 := 0;
   nvmeMSIX : Boolean := False;
   --  Minted just before the configuration call, as DEVMGR_NET_SLOT is.
   DEVMGR_NVME_SLOT : constant Unsigned_64 := 3;
   procmgrPID    : Unsigned_64 := 0;
   shellPID      : Unsigned_64 := 0;
   hdaPID        : Unsigned_64 := 0;
   mixerPID      : Unsigned_64 := 0;
   ps2PID        : Unsigned_64 := 0;
   xhciPID       : Unsigned_64 := 0;
   configPID     : Unsigned_64 := 0;
   netmgrPID     : Unsigned_64 := 0;
   virtioGpuPID  : Unsigned_64 := 0;

   --  CPIO archive
   cpioArchive : Cpio.Archive;
   cpioOk      : Boolean := False;

   --  Number of CPUs
   numCPUs : Unsigned_64 := 1;

   --  Round-robin CPU counter for app placement
   nextAppCPU : Natural := 1;

   reterr : constant Unsigned_64 := Unsigned_64'Last;

   ---------------------------------------------------------------------------
   -- PCI config space access via port I/O syscalls
   ---------------------------------------------------------------------------
   function pciReadConfig32 (bus    : Unsigned_8;
                             pSlot  : Unsigned_8;
                             func   : Unsigned_8;
                             offset : Unsigned_8) return Unsigned_32
   is
      addr : Unsigned_32;
      ignore : Unsigned_64;
   begin
      addr := 16#8000_0000# or
              Shift_Left (Unsigned_32 (bus), 16) or
              Shift_Left (Unsigned_32 (pSlot), 11) or
              Shift_Left (Unsigned_32 (func), 8) or
              Unsigned_32 (offset and 16#FC#);
      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      return Unsigned_32 (portInp32 (PCI_CONFIG_DATA) and 16#FFFF_FFFF#);
   end pciReadConfig32;

   function pciReadConfig16 (bus    : Unsigned_8;
                             pSlot  : Unsigned_8;
                             func   : Unsigned_8;
                             offset : Unsigned_8) return Unsigned_16
   is
      addr : Unsigned_32;
      ignore : Unsigned_64;
      data : Unsigned_64;
   begin
      addr := 16#8000_0000# or
              Shift_Left (Unsigned_32 (bus), 16) or
              Shift_Left (Unsigned_32 (pSlot), 11) or
              Shift_Left (Unsigned_32 (func), 8) or
              Unsigned_32 (offset and 16#FC#);
      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      data := portInp32 (PCI_CONFIG_DATA);
      --  Select correct 16-bit word within the 32-bit register
      if (offset and 2) /= 0 then
         return Unsigned_16 (Shift_Right (Unsigned_32 (data), 16)
                             and 16#FFFF#);
      else
         return Unsigned_16 (data and 16#FFFF#);
      end if;
   end pciReadConfig16;

   function pciReadConfig8 (bus    : Unsigned_8;
                            pSlot  : Unsigned_8;
                            func   : Unsigned_8;
                            offset : Unsigned_8) return Unsigned_8
   is
      data32 : Unsigned_32;
      byteOff : constant Natural := Natural (offset and 3);
   begin
      data32 := pciReadConfig32 (bus, pSlot, func, offset);
      return Unsigned_8 (Shift_Right (data32, byteOff * 8) and 16#FF#);
   end pciReadConfig8;

   procedure pciWriteConfig32 (bus    : Unsigned_8;
                               pSlot  : Unsigned_8;
                               func   : Unsigned_8;
                               offset : Unsigned_8;
                               value  : Unsigned_32)
   is
      addr   : Unsigned_32;
      ignore : Unsigned_64;
   begin
      if Intel_Config_Frozen and then intelDev.found and then
        bus = intelDev.bus and then pSlot = intelDev.slot and then func = intelDev.func
      then
         debugPrint ("devmgr: rejected PCI write to claimed Intel GPU" & LF);
         return;
      end if;
      addr := 16#8000_0000# or
              Shift_Left (Unsigned_32 (bus), 16) or
              Shift_Left (Unsigned_32 (pSlot), 11) or
              Shift_Left (Unsigned_32 (func), 8) or
              Unsigned_32 (offset and 16#FC#);
      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      ignore := portOutp32 (PCI_CONFIG_DATA, value);
   end pciWriteConfig32;

   procedure pciWriteConfig16 (bus    : Unsigned_8;
                               pSlot  : Unsigned_8;
                               func   : Unsigned_8;
                               offset : Unsigned_8;
                               value  : Unsigned_16)
   is
      addr : Unsigned_32;
      ignore : Unsigned_64;
   begin
      if Intel_Config_Frozen and then intelDev.found and then
        bus = intelDev.bus and then pSlot = intelDev.slot and then func = intelDev.func
      then
         debugPrint ("devmgr: rejected PCI write to claimed Intel GPU" & LF);
         return;
      end if;
      addr := 16#8000_0000# or
              Shift_Left (Unsigned_32 (bus), 16) or
              Shift_Left (Unsigned_32 (pSlot), 11) or
              Shift_Left (Unsigned_32 (func), 8) or
              Unsigned_32 (offset and 16#FC#);
      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      -- Never widen this into a DWORD read/modify/write: e.g. COMMAND's
      -- neighboring STATUS word contains write-one-to-clear bits. PCI
      -- mechanism 1 selects the word with the CFC/CFE data-port lane.
      ignore := portOutp16
        (PCI_CONFIG_DATA + Unsigned_16 (offset and 2), value);
   end pciWriteConfig16;

   Intel_IRQ_Active, Intel_IRQ_Plan_Ready : Boolean := False;
   Intel_IRQ_Plan : Intel_GPU_PCI_Interrupts.Disable_Plan;
   function Intel_Config_Address (Offset : Natural) return Unsigned_32 is
     (16#8000_0000# or Shift_Left (Unsigned_32 (intelDev.bus), 16) or
      Shift_Left (Unsigned_32 (intelDev.slot), 11) or
      Shift_Left (Unsigned_32 (intelDev.func), 8) or (Unsigned_32 (Offset) and 16#FC#));
   procedure Read_Intel_IRQ_Config
     (Data : out Intel_GPU_PCI_Power.Configuration; Success : out Boolean)
   is
      Raw : Unsigned_64;
      Plan : Intel_GPU_Resources.Mapping_Plan;
      function Word32 (Offset : Natural) return Unsigned_32 is
        (Unsigned_32 (Data (Offset)) or Shift_Left (Unsigned_32 (Data (Offset + 1)), 8) or
         Shift_Left (Unsigned_32 (Data (Offset + 2)), 16) or
         Shift_Left (Unsigned_32 (Data (Offset + 3)), 24));
      use type Intel_GPU_Resources.Admission_Status;
      use type Intel_GPU_PCI_Power.Power_Status;
   begin
      Success := False; Intel_IRQ_Plan_Ready := False;
      Data := [others => 255];
      if not Intel_IRQ_Active or else not Intel_Config_Frozen or else
        not intelDev.found or else Intel_Claim_Identity /= 16#46D2_8086#
      then return; end if;
      for Index in 0 .. 63 loop
         if portOutp32 (PCI_CONFIG_ADDR, Intel_Config_Address (Index * 4)) /= 0 then return; end if;
         Raw := portInp32 (PCI_CONFIG_DATA);
         if Raw = reterr then return; end if;
         for Byte in 0 .. 3 loop
            Data (Index * 4 + Byte) := Unsigned_8 (Shift_Right (Raw, Byte * 8) and 255);
         end loop;
      end loop;
      Plan := Intel_GPU_Resources.Plan_ADLN_Registers
        (Intel_GPU_Probe.Alder_Lake_N, Unsigned_16 (Word32 (4) and 16#FFFF#),
         Word32 (16#10#), Word32 (16#14#), 0, 4096);
      Success := Word32 (0) = Intel_Claim_Identity and then
        Intel_GPU_PCI_Power.Decode (Data) = Intel_GPU_PCI_Power.D0 and then
        Plan.Status = Intel_GPU_Resources.Admitted and then Plan.Physical_Base = Intel_Claim_Base;
      if Success then
         Intel_IRQ_Plan := Intel_GPU_PCI_Interrupts.Plan_Disable (Data);
         Intel_IRQ_Plan_Ready := Intel_GPU_PCI_Interrupts.Valid (Intel_IRQ_Plan);
      end if;
   end Read_Intel_IRQ_Config;
   procedure Write_Intel_IRQ_Word
     (Offset : Natural; Value : Unsigned_16; Success : out Boolean)
   is
      Allowed : Boolean := False;
   begin
      Success := False;
      if not Intel_IRQ_Active or else not Intel_IRQ_Plan_Ready then return; end if;
      for I in 1 .. Intel_GPU_PCI_Interrupts.Count (Intel_IRQ_Plan) loop
         Allowed := Allowed or
           (Offset = Intel_GPU_PCI_Interrupts.Offset (Intel_IRQ_Plan, I) and then
            Value = Intel_GPU_PCI_Interrupts.After (Intel_IRQ_Plan, I));
      end loop;
      Intel_IRQ_Plan_Ready := False; -- one write per fresh admitted snapshot
      if not Allowed then return; end if;
      if portOutp32 (PCI_CONFIG_ADDR, Intel_Config_Address (Offset)) /= 0 then return; end if;
      Success := portOutp16 (PCI_CONFIG_DATA + Unsigned_16 (Offset mod 4), Value) = 0;
   end Write_Intel_IRQ_Word;
   package Intel_IRQ_Disable is new Intel_GPU_PCI_IRQ_Disable
     (Read_Intel_IRQ_Config, Write_Intel_IRQ_Word);

   ---------------------------------------------------------------------------
   -- PCI bus scan: find devices by class code
   ---------------------------------------------------------------------------
   procedure scanPCI is
      function Hex (Value : Unsigned_64; Digits_Count : Positive) return String is
         Result : String (1 .. Digits_Count);
         Characters : constant String := "0123456789ABCDEF";
         Remaining : Unsigned_64 := Value;
      begin
         for I in reverse Result'Range loop
            Result (I) := Characters (Natural (Remaining and 15) + 1);
            Remaining := Shift_Right (Remaining, 4);
         end loop;
         return Result;
      end Hex;

      procedure Report_Intel_Display (Location : PCIDeviceInfo;
                                      Device : Unsigned_16;
                                      Class_Code : Unsigned_16) is
         use Intel_GPU_Probe;
         Header : constant Unsigned_8 := pciReadConfig8
           (Location.bus, Location.slot, Location.func, PCI_HEADER_TYPE);
         Family : constant Platform := Identify
           (16#8086#, Device, Unsigned_8 (Shift_Right (Class_Code, 8)));
         Index : Natural range 0 .. 6 := 0;
         Low, High : Unsigned_32;
         Resource : BAR_Result;
         Register_Plan : Intel_GPU_Resources.Mapping_Plan;
      begin
         -- Whole bounded lines survive diagnostic capture without fragment loss.
         -- Configuration reads only: never probe sizes, enable bus mastering,
         -- map registers, or claim ownership of the active firmware scanout.
         debugPrint ("intel-gpu: PCI " & Hex (Unsigned_64 (Location.bus), 2)
           & ":" & Hex (Unsigned_64 (Location.slot), 2) & "."
           & Hex (Unsigned_64 (Location.func), 1) & " device="
           & Hex (Unsigned_64 (Device), 4) & " revision="
           & Hex (Unsigned_64 (pciReadConfig8
             (Location.bus, Location.slot, Location.func, 8)), 2)
           & " family=" & Platform'Image (Family) & LF);
         debugPrint ("intel-gpu: command=" & Hex (Unsigned_64
           (pciReadConfig16 (Location.bus, Location.slot, Location.func,
                            PCI_COMMAND)), 4) & " header="
           & Hex (Unsigned_64 (Header), 2) & LF);
         if (Header and 16#7F#) /= 0 then
            debugPrint ("intel-gpu: unsupported PCI header; no BAR scan" & LF);
            return;
         end if;
         while Index < 6 loop
            if Family = Alder_Lake_N and then not intelDev.found then
               intelDev := Location;
               intelDev.found := True;
            end if;
            Low := pciReadConfig32 (Location.bus, Location.slot, Location.func,
                                   Unsigned_8 (16 + Index * 4));
            High := 0;
            if (Low and 7) = 4 and then Index < 5 then
               High := pciReadConfig32
                 (Location.bus, Location.slot, Location.func,
                  Unsigned_8 (20 + Index * 4));
            end if;
            Resource := Decode_BAR (Low, High, Index < 5);
            if Index = 0 then
               Register_Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                 (Family, pciReadConfig16 (Location.bus, Location.slot,
                  Location.func, PCI_COMMAND), Low, High, 0, 16#20_0000#);
               debugPrint ("intel-gpu: register-region plan=" &
                 Intel_GPU_Resources.Admission_Status'Image (Register_Plan.Status)
                 & " base=" & Hex (Register_Plan.Physical_Base, 16)
                 & " bytes=" & Hex (Register_Plan.Bytes, 8) & LF);
            end if;
            debugPrint ("intel-gpu: BAR" & Hex (Unsigned_64 (Index), 1)
              & " " & BAR_Status'Image (Resource.Status)
              & " base=" & Hex (Resource.Base, 16)
              & " raw=" & Hex (Unsigned_64 (High), 8) & ":"
              & Hex (Unsigned_64 (Low), 8) & " size=unknown" & LF);
            exit when Index + Resource.Words >= 6;
            Index := Index + Resource.Words;
         end loop;
         debugPrint ("intel-gpu: inventory only; firmware display retained" & LF);
      end Report_Intel_Display;

      procedure printHexNibble (value : Unsigned_8) is
         hexCharacters : constant String := "0123456789ABCDEF";
      begin
         debugPrint
           ("" & hexCharacters (Natural (value and 16#0F#) + 1));
      end printHexNibble;

      procedure printHex8 (value : Unsigned_8) is
      begin
         printHexNibble (Shift_Right (value, 4));
         printHexNibble (value);
      end printHex8;

      procedure printHex16 (value : Unsigned_16) is
      begin
         printHex8 (Unsigned_8 (Shift_Right (value, 8)));
         printHex8 (Unsigned_8 (value and 16#FF#));
      end printHex16;

      procedure observeFunction
        (bus   : Unsigned_8;
         pSlot : Unsigned_8;
         func  : Unsigned_8)
      is
         vendorID  : constant Unsigned_16 :=
           pciReadConfig16 (bus, pSlot, func, PCI_VENDOR_ID);
         deviceID  : Unsigned_16;
         classCode : Unsigned_16;
         progIf    : Unsigned_8;
         location  : constant PCIDeviceInfo :=
           (found => True, bus => bus, slot => pSlot, func => func);
         observedKind : CuBit.Devices.Device_Kind :=
           CuBit.Devices.Other_Device;
      begin
         if vendorID = 16#FFFF# then
            return;
         end if;

         deviceID := pciReadConfig16 (bus, pSlot, func, 2);
         classCode := Unsigned_16
           (Shift_Right
              (pciReadConfig32 (bus, pSlot, func, PCI_CLASS_DEVICE), 16)
            and 16#FFFF#);
         progIf := pciReadConfig8 (bus, pSlot, func, PCI_PROG_IF);

         debugPrint ("devmgr: PCI ");
         printHex8 (bus);
         debugPrint (":");
         printHex8 (pSlot);
         debugPrint (".");
         printHexNibble (func);
         debugPrint (" vendor=");
         printHex16 (vendorID);
         debugPrint (" device=");
         printHex16 (deviceID);
         debugPrint (" class=");
         printHex16 (classCode);
         debugPrint (" prog-if=");
         printHex8 (progIf);

         if classCode = CLASS_STORAGE_NVME then
            nvmeDev := location;
            observedKind := CuBit.Devices.Storage_Controller;
            debugPrint (" NVMe");
         elsif classCode = CLASS_STORAGE_IDE then
            ataDev := location;
            observedKind := CuBit.Devices.Storage_Controller;
            debugPrint (" IDE");
         elsif classCode = CLASS_STORAGE_SATA and then
               progIf = PCI_PROG_IF_AHCI
         then
            observedKind := CuBit.Devices.Storage_Controller;
            debugPrint (" AHCI (unclaimed)");
         elsif classCode = CLASS_NET_ETHERNET then
            observedKind := CuBit.Devices.Network_Controller;
            if vendorID = VENDOR_VIRTIO then
               netDev := location;
               debugPrint (" virtio-net");
            else
               debugPrint (" Ethernet (unclaimed)");
            end if;
         elsif classCode = CLASS_MULTIMEDIA_HDA then
            hdaDev := location;
            observedKind := CuBit.Devices.Audio_Controller;
            debugPrint (" HDA");
         elsif classCode = CLASS_SERIAL_USB and then
               progIf = PCI_PROG_IF_XHCI
         then
            xhciDev := location;
            observedKind := CuBit.Devices.USB_Controller;
            debugPrint (" xHCI");
         elsif vendorID = VENDOR_VIRTIO and then
               (deviceID = DEVICE_VIRTIO_GPU or else
                classCode = CLASS_DISPLAY_VGA)
         then
            gpuDev := location;
            gpuIsPrimary := classCode = CLASS_DISPLAY_VGA;
            observedKind := CuBit.Devices.Display_Controller;
            debugPrint (" virtio-gpu");
         elsif Shift_Right (classCode, 8) = 3 then
            observedKind := CuBit.Devices.Display_Controller;
            debugPrint (" display (firmware/unclaimed)");
         end if;

         if inventoryCount < inventory'Length then
            inventoryCount := inventoryCount + 1;
            inventory (inventoryCount) :=
              (location  => location,
               vendorID  => vendorID,
               deviceID  => deviceID,
               classCode => classCode,
               progIf    => progIf,
               kind      => observedKind);
         else
            inventoryOverflow := True;
         end if;

         debugPrint (LF & "");
         if vendorID = 16#8086# and then Shift_Right (classCode, 8) = 3 then
            Report_Intel_Display (location, deviceID, classCode);
         end if;
      end observeFunction;

      vendorID  : Unsigned_16;
      lastFunc  : Unsigned_8;
   begin
      --  PCI devices on real machines are not confined to bus zero or
      --  function zero.  Probe every bus, and probe functions 1..7 only when
      --  function zero advertises a multifunction device.  Configuration
      --  reads for absent buses and slots return 0xFFFF.
      for bus in Unsigned_8'Range loop
         for pSlot in Unsigned_8 range 0 .. 31 loop
            vendorID := pciReadConfig16 (bus, pSlot, 0, PCI_VENDOR_ID);
            if vendorID /= 16#FFFF# then
               if (pciReadConfig8 (bus, pSlot, 0, PCI_HEADER_TYPE) and
                   PCI_HEADER_MULTIFUNCTION) /= 0
               then
                  lastFunc := 7;
               else
                  lastFunc := 0;
               end if;

               for func in Unsigned_8 range 0 .. lastFunc loop
                  observeFunction (bus, pSlot, func);
               end loop;
            end if;
         end loop;
      end loop;
   end scanPCI;

   ---------------------------------------------------------------------------
   -- Spawn a service from the CPIO initrd.
   -- Returns new PID, or 0 on failure.
   ---------------------------------------------------------------------------
   Optical_Storage_Ready : Boolean := False;
   Boot_Buffer : System.Address := System.Null_Address;
   Boot_Grant : CuBit.Memory_Grants.Grant_Reference;
   Boot_Granted : Boolean := False;
   Boot_Read_Policy : Boolean := False;
   Boot_Buffer_Bytes : constant Unsigned_64 := 8 * 1024 * 1024;

   function Read_Boot_Image (Name : String) return Unsigned_64 is
      Request : Message;
      Tag : MessageTag;
      Raw, Size, Read_Bytes : Unsigned_64;
      Handle : CuBit.Filesystems.File_Handle;
   begin
      if not Optical_Storage_Ready or else Name'Length not in 1 .. 256 then
         return 0;
      end if;
      if Boot_Buffer = System.Null_Address then
         Raw := syscall (SYSCALL_SBRK, Boot_Buffer_Bytes + 4096);
         if Raw = reterr then return 0; end if;
         Boot_Buffer := To_Address (Integer_Address ((Raw + 4095) and not 4095));
      end if;
      if not Boot_Granted then
         CuBit.Memory_Grants.Create_Via_Capability
           (1, Boot_Buffer, Natural (Boot_Buffer_Bytes / 4096), True,
            Boot_Grant, Boot_Granted);
         if not Boot_Granted then return 0; end if;
      end if;
      if not Boot_Read_Policy then
         declare
            Policy : CuBit.File_Access.Wire_Bytes (1 .. 72)
              with Import, Address => Boot_Buffer;
         begin
            Policy := [others => 0];
            Policy (1) := 1; -- Read_Objects only, empty prefix = bootstrap scope
         end;
         Request :=
           (tag => (label => CuBit.Filesystems.OP_SET_ACL,
                    length => 4, flags => 0, reserved => 0),
            authorityTag => 0,
            words => [getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR), 1,
                      Unsigned_64 (Boot_Grant.slot),
                      Unsigned_64 (Boot_Grant.generation)]);
         Tag := capCall (1, Request);
         if Tag.label /= REPLY_OK then return 0; end if;
         Boot_Read_Policy := True;
      end if;
      declare
         Path : String (1 .. Name'Length) with Import, Address => Boot_Buffer;
      begin
         Path := Name;
      end;
      Request := CuBit.Filesystems.Open_Request
        (Boot_Grant, CuBit.Filesystems.Nonempty_Path_Byte_Count (Name'Length));
      Tag := capCall (1, Request);
      if Tag.label /= REPLY_OK or else Tag.length /= 2 then return 0; end if;
      Handle := CuBit.Filesystems.File_Handle (Request.words (0));
      Size := Request.words (1);
      Read_Bytes := 0;
      if Size in 1 .. Boot_Buffer_Bytes then
         Request := CuBit.Filesystems.Read_Request (Handle, Boot_Grant, Size);
         Tag := capCall (1, Request);
         if Tag.label = REPLY_OK and then Tag.length >= 1 and then
           Request.words (0) = Size
         then Read_Bytes := Size; end if;
      end if;
      Request := CuBit.Filesystems.Close_Request (Handle);
      Tag := capCall (1, Request);
      if Tag.label /= REPLY_OK then return 0; end if;
      return Read_Bytes;
   end Read_Boot_Image;

   function spawnFromBootStorage (name     : String;
                           priority : Unsigned_64;
                           reqPID   : Unsigned_64 := 0) return Unsigned_64
   is
      idx  : Natural;
      addr : Unsigned_64;
      size : Unsigned_64;
      --  NUL-terminated copy of name for kernel to read
      nameBuf : String (1 .. 17) := (others => Character'Val (0));
      nameLen : Natural;
   begin
      idx := Cpio.findFile (cpioArchive, name);
      if idx >= cpioArchive.count then
         size := Read_Boot_Image (name);
         if size = 0 then
            debugPrint ("devmgr: boot image unavailable: " & name & LF);
            return 0;
         end if;
         addr := Unsigned_64 (To_Integer (Boot_Buffer));
         debugPrint ("devmgr: loaded from filesystem: " & name & LF);
      else
         addr := INITRD_BASE +
                 Unsigned_64 (cpioArchive.files (idx).dataOff);
         size := Unsigned_64 (cpioArchive.files (idx).dataSize);
      end if;

      nameLen := name'Length;
      if nameLen > 16 then
         nameLen := 16;
      end if;
      for i in 0 .. nameLen - 1 loop
         nameBuf (i + 1) := name (name'First + i);
      end loop;

      return syscall (SYSCALL_SPAWN, addr, size, priority,
                      Unsigned_64 (To_Integer (nameBuf'Address)), reqPID);
   end spawnFromBootStorage;

   ---------------------------------------------------------------------------
   -- Mint a capability into a target process
   ---------------------------------------------------------------------------
   procedure mintCap (target   : Unsigned_64;
                      capType  : Unsigned_64;
                      objRef   : Unsigned_64;
                      objParam : Unsigned_64;
                      rights   : Unsigned_64;
                      capSlot  : Unsigned_64)
   is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
                      target, capType, objRef, objParam, rights, capSlot);
      if ret = reterr then
         debugPrint ("devmgr: mint_cap failed" & LF);
      end if;
   end mintCap;

   ---------------------------------------------------------------------------
   -- String comparison (freestanding: can't use "=" which generates memcmp)
   ---------------------------------------------------------------------------
   function strEq (a : String; b : String) return Boolean is
   begin
      if a'Length /= b'Length then
         return False;
      end if;
      for i in 0 .. a'Length - 1 loop
         if a (a'First + i) /= b (b'First + i) then
            return False;
         end if;
      end loop;
      return True;
   end strEq;

   ---------------------------------------------------------------------------
   -- Grant an IPC endpoint between two processes
   ---------------------------------------------------------------------------
   procedure grantEndpoint (target    : Unsigned_64;
                            destPID   : Unsigned_64;
                            capSlot   : Unsigned_64;
                            authorityTagPID : Unsigned_64)
   is
   begin
      mintCap (target   => target,
               capType  => CAP_ENDPOINT,
               objRef   => destPID,
               objParam => 0,
               rights   => RIGHT_READ or RIGHT_WRITE,
               capSlot  => capSlot);
   end grantEndpoint;

   ---------------------------------------------------------------------------
   -- Assign CPU affinity based on service type
   ---------------------------------------------------------------------------
   procedure assignCPU (pid  : Unsigned_64;
                        name : String)
   is
      ignore : Unsigned_64;
      cpu    : Unsigned_64 := 0;
   begin
      if numCPUs <= 1 then
         return;
      end if;

      --  Storage/FS/input services pinned to CPU 0
      if strEq (name, "filesystem.svc") or strEq (name, "ata.drv") or
         strEq (name, "nvme.drv") or strEq (name, "ramdisk.drv") or
         strEq (name, "procmgr.svc") or
         strEq (name, "ps2.drv") or strEq (name, "xhci.drv")
      then
         cpu := 0;
      --  Network services on CPU 1
      --  The NIC driver polls while traffic flows (CuBit.Busy_Poll), so
      --  it gets a CPU of its own when there are enough.
      elsif strEq (name, "virtio-net.drv") and then numCPUs > 3 then
         cpu := 3;
      elsif strEq (name, "netstack.svc") or
            strEq (name, "virtio-net.drv") or
            strEq (name, "netmgr.svc")
      then
         if numCPUs > 1 then
            cpu := 1;
         end if;
      --  Display/GPU work gets its own CPU when available.
      elsif strEq (name, "virtio-gpu.drv") then
         if numCPUs > 2 then
            cpu := 2;
         elsif numCPUs > 1 then
            cpu := 1;
         end if;
      --  Other apps round-robin across CPUs 1..N-1
      else
         cpu := Unsigned_64 (nextAppCPU);
         nextAppCPU := nextAppCPU + 1;
         if nextAppCPU >= Natural (numCPUs) then
            nextAppCPU := 1;
         end if;
      end if;

      ignore := setCpu (pid, cpu);
   end assignCPU;

   ---------------------------------------------------------------------------
   -- Resume a suspended process
   ---------------------------------------------------------------------------
   procedure resumeProc (pid : Unsigned_64) is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_RESUME, pid);
      if ret = reterr then
         debugPrint ("devmgr: resume failed" & LF);
      end if;
   end resumeProc;

   --  IPC ready protocol constants
   OP_READY      : constant Unsigned_32 := 16#FF00#;
   OP_NOT_PRESENT : constant Unsigned_32 := 16#FF01#;
   CAP_SLOT_READY : constant Unsigned_64 := 15;
   --  mixer.svc may reserve real-time CPU for one audio period: 1.5 ms of
   --  every 5 ms (docs/scheduler.md, "Soft real time").
   CAP_SLOT_SCHEDULING : constant Unsigned_64 := 9;
   MIXER_REALTIME_BUDGET_US : constant Unsigned_64 := 1_500;
   MIXER_REALTIME_PERIOD_US : constant Unsigned_64 := 5_000;

   --  Our PID (discovered via registerDriver)
   myPID : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   -- Wait for a driver to signal readiness via IPC.
   -- Returns True if driver sent OP_READY, False if OP_NOT_PRESENT.
   ---------------------------------------------------------------------------
   function waitReady (driverPID : Unsigned_64) return Boolean is
      sender : ProcessID;
      rdyMsg : Message;
      ignore : Unsigned_64;
   begin
      loop
         receive (sender, rdyMsg);
         if sender = driverPID then
            ignore := reply (sender, NULL_MESSAGE);
            return rdyMsg.tag.label = OP_READY;
         elsif ((rdyMsg.tag = (16#022D#, 1, 0, 0) and then
                 rdyMsg.words (0) <= Unsigned_64 (Intel_GPU_Reset_Pages.Page_Index'Last)) or else
                (rdyMsg.tag = (Intel_GPU_Display_Pages.Request_Label, 1, 0, 0) and then
                 rdyMsg.words (0) <= Unsigned_64 (Intel_GPU_Display_Pages.Page_Index'Last)) or else
                (rdyMsg.tag = (Intel_GPU_PHY_Pages.Request_Label, 1, 0, 0) and then
                 rdyMsg.words (0) <= Unsigned_64 (Intel_GPU_PHY_Pages.Page_Index'Last))) and then
           sender = Intel_Inspection_PID and then sender /= 0 and then
           rdyMsg.authorityTag = 16#4947# and then
           rdyMsg.words (1 .. 3) = [0, 0, 0]
         then
            ignore := reply (sender,
              (tag => (16#F002#, 0, 0, 0), authorityTag => 0, words => [others => 0]));
         elsif rdyMsg.tag = (Intel_GPU_Buffer_Backing.Budget_Request_Label, 4, 0, 0) and then
           sender = Intel_Inspection_PID and then sender /= 0 and then
           rdyMsg.authorityTag = 16#4947# and then rdyMsg.words = [Intel_GPU_Buffer_Backing.Budget_Version, 0, 0, 0]
         then
            ignore := reply (sender,
              (tag => (16#F002#, 0, 0, 0), authorityTag => 0, words => [others => 0]));
         elsif Intel_GPU_Buffer_Backing.Valid_Allocation_Request
           (rdyMsg.tag.label, rdyMsg.tag.length, rdyMsg.tag.flags, rdyMsg.tag.reserved,
            rdyMsg.words (0), rdyMsg.words (1), rdyMsg.words (2), rdyMsg.words (3)) and then
           sender = Intel_Inspection_PID and then sender /= 0 and then
           rdyMsg.authorityTag = 16#4947#
         then
            ignore := reply (sender,
              (tag => (16#F002#, 0, 0, 0), authorityTag => 0, words => [others => 0]));
         elsif (rdyMsg.tag = (16#022A#, 0, 0, 0) or else
                rdyMsg.tag = (16#022B#, 0, 0, 0) or else
                rdyMsg.tag = (Intel_GPU_GGTT_Access.Write_Request_Label, 0, 0, 0) or else
                rdyMsg.tag = (16#022C#, 0, 0, 0) or else
                rdyMsg.tag = (16#022E#, 0, 0, 0) or else
                rdyMsg.tag = (Intel_GPU_PCI_Interrupts.Disable_Request_Label, 0, 0, 0) or else
                rdyMsg.tag = (Intel_GPU_ADS_Backing.Request_Label, 0, 0, 0) or else
                rdyMsg.tag = (16#022F#, 0, 0, 0)) and then
           sender = Intel_Inspection_PID and then sender /= 0 and then
           rdyMsg.authorityTag = 16#4947# and then rdyMsg.words = [0, 0, 0, 0]
         then
            ignore := reply (sender,
              (tag => (16#F002#, 0, 0, 0), authorityTag => 0, words => [others => 0]));
         else
            ignore := reply (sender, NULL_MESSAGE);
         end if;
      end loop;
   end waitReady;

   ---------------------------------------------------------------------------
   -- Send a wildcard ACL to the filesystem server for a target process
   ---------------------------------------------------------------------------
   OP_SET_ACL : constant Unsigned_32 := 16#0080#;

   procedure Grant_Intel_Firmware (Child : Unsigned_64) is
      Path : constant String := "firmware/intel/tgl_guc_70.bin";
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if filesystemPID = 0 or else not Boot_Granted or else
        Boot_Buffer = System.Null_Address
      then return; end if;
      declare
         Policy : CuBit.File_Access.Wire_Bytes (1 .. 72)
           with Import, Address => Boot_Buffer;
      begin
         Policy := [others => 0];
         Policy (1) := 1; -- Read_Objects; no write/create/delete authority.
         Policy (2) := Path'Length;
         for Index in Path'Range loop
            Policy (8 + Index) := Character'Pos (Path (Index));
         end loop;
      end;
      Request.tag := (CuBit.Filesystems.OP_SET_ACL, 4, 0, 0);
      Request.words := [Child, 1, Unsigned_64 (Boot_Grant.slot),
                        Unsigned_64 (Boot_Grant.generation)];
      Tag := capCall (1, Request);
      if Tag = (CuBit.Filesystems.REPLY_OK, 1, 0, 0) and then
        Request.words = [0, 0, 0, 0]
      then
         grantEndpoint (Child, filesystemPID, 6, Child);
         debugPrint ("devmgr: Intel firmware read scope installed" & LF);
      else
         debugPrint ("devmgr: Intel firmware read scope rejected" & LF);
      end if;
   end Grant_Intel_Firmware;

   procedure sendWildcardACL (targetPID : Unsigned_64) is
      aclMsg : Message;
      ignore : MessageTag;
   begin
      if filesystemPID = 0 then
         return;
      end if;
      aclMsg.tag := (label  => OP_SET_ACL,
                      length => 4,
                      flags  => 0,
                      reserved  => 0);
      aclMsg.authorityTag := 0;
      aclMsg.words := [0 => targetPID, 1 => 0, 2 => 0, 3 => 0];
      ignore := capCall (1, aclMsg);
   end sendWildcardACL;

   ---------------------------------------------------------------------------
   -- Send a wildcard ACL to the config store for a target process
   ---------------------------------------------------------------------------
   procedure sendWildcardACLConfig (targetPID : Unsigned_64) is
      aclMsg : Message;
      ignore : MessageTag;
   begin
      if configPID = 0 then
         return;
      end if;
      aclMsg.tag := (label  => OP_SET_ACL,
                      length => 4,
                      flags  => 0,
                      reserved  => 0);
      aclMsg.authorityTag := 0;
      aclMsg.words := [0 => targetPID, 1 => 0, 2 => 0, 3 => 0];
      ignore := capCall (2, aclMsg);
   end sendWildcardACLConfig;

   ---------------------------------------------------------------------------
   -- Setup NVMe driver: PCI config, DMA, capabilities
   ---------------------------------------------------------------------------
   procedure setupNvme is
      bar0Lo    : Unsigned_32;
      bar0Hi    : Unsigned_32;
      bar0Phys  : Unsigned_64;
      irqLine   : Unsigned_8;
      irqVector : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;
      ret       : Unsigned_64;

      DMA_ORDER : constant Unsigned_64 := 8;  --  256 pages = 1 MiB
      DMA_PAGES : constant Unsigned_64 := 256;
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
   begin
      if not nvmeDev.found or nvmePID = 0 then
         return;
      end if;

      --  Read BAR0 (64-bit MMIO)
      bar0Lo := pciReadConfig32 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_BASEADDR_0);
      bar0Hi := pciReadConfig32 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_BASEADDR_1);
      bar0Phys := Shift_Left (Unsigned_64 (bar0Hi), 32) or
                  Unsigned_64 (bar0Lo and 16#FFFF_FFF0#);

      --  Read IRQ line, compute vector (legacy PCI: vector = 32 + line)
      irqLine := pciReadConfig8 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_INTERRUPT_LINE);
      irqVector := 32 + Unsigned_64 (irqLine);

      --  Enable bus master + memory space
      pciCmd := pciReadConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);

      --  Completion interrupts by MSI-X when the table lies in the part of
      --  BAR0 the driver maps; otherwise the driver polls. The function
      --  stays masked until the driver has filled entry zero.
      nvmeMSIX := False;
      if (pciReadConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                           PCI_STATUS) and 16#10#) /= 0
      then
         nvmeMSIXCap := pciReadConfig8
           (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, PCI_CAP_PTR) and 16#FC#;
         for hop in 1 .. 48 loop
            exit when nvmeMSIXCap < 16#40# or else nvmeMSIXCap > 16#F4#;
            exit when pciReadConfig8
              (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap) =
              PCI_CAP_ID_MSIX;
            nvmeMSIXCap := pciReadConfig8
              (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap + 1) and 16#FC#;
         end loop;
         if nvmeMSIXCap >= 16#40# and then nvmeMSIXCap <= 16#F4# and then
           pciReadConfig8 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap) =
             PCI_CAP_ID_MSIX
         then
            declare
               tableInfo : constant Unsigned_32 := pciReadConfig32
                 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap + 4);
               offset : constant Unsigned_64 :=
                 Unsigned_64 (tableInfo and 16#FFFF_FFF8#);
               control : Unsigned_16;
            begin
               if (tableInfo and 7) = 0 and then
                 offset <= CuBit.NVMe_Control.Table_Offset'Last and then
                 enableIrq (CuBit.NVMe_Control.Device_Vector, nvmePID, 0,
                            messageSignaled => True) /= reterr
               then
                  control := pciReadConfig16
                    (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap + 2);
                  pciWriteConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                    nvmeMSIXCap + 2, control or 16#C000#);
                  nvmeTableOffset := offset;
                  nvmeMSIX := True;
               end if;
            end;
         end if;
      end if;
      if not nvmeMSIX then
         --  Legacy route, as before (the polling driver does not use it).
         ret := enableIrq (irqVector, nvmePID, 0);
      end if;

      --  Allocate DMA
      dmaPhys := allocDma (nvmePID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: NVMe DMA alloc failed" & LF);
         return;
      end if;

      --  Slot 4: CAP_DEVICE_MEM for BAR0 MMIO (16KB = 4 pages)
      mintCap (nvmePID, CAP_DEVICE_MEM, bar0Phys, 16384,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 5: CAP_IRQ for device interrupt
      mintCap (nvmePID, CAP_IRQ, irqVector, 0, RIGHT_READ, 5);

      --  Slot 6: CAP_DEVICE_MEM for DMA region
      mintCap (nvmePID, CAP_DEVICE_MEM, 0, DMA_SIZE,
               RIGHT_READ or RIGHT_WRITE, 6);

      --  Publish BAR0 and DMA phys via sysinfo
      ret := setSysinfo (SYSINFO_NVME_BAR0, bar0Phys);
      ret := setSysinfo (SYSINFO_NVME_DMA_PHYS, dmaPhys);

      debugPrint ("devmgr: NVMe setup complete" & LF);
   end setupNvme;

   ---------------------------------------------------------------------------
   -- Setup ATA driver: grant IOPORT + IRQ capabilities
   ---------------------------------------------------------------------------
   procedure setupAta is
      IDE1_VECTOR : constant Unsigned_64 := 46;
      ret : Unsigned_64;
   begin
      if ataPID = 0 then
         return;
      end if;

      --  Slot 4: CAP_IOPORT for primary ATA ports 0x1F0-0x1F7 (8 ports)
      mintCap (ataPID, CAP_IOPORT, 16#1F0#, 8,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 5: CAP_IOPORT for control port 0x3F6 (1 port)
      mintCap (ataPID, CAP_IOPORT, 16#3F6#, 1,
               RIGHT_READ or RIGHT_WRITE, 5);

      --  Slot 6: CAP_IRQ for IDE1 (vector 46)
      mintCap (ataPID, CAP_IRQ, IDE1_VECTOR, 0, RIGHT_READ, 6);

      --  Enable IRQ routing
      ret := enableIrq (IDE1_VECTOR, ataPID, 0);

      debugPrint ("devmgr: ATA setup complete" & LF);
   end setupAta;

   ---------------------------------------------------------------------------
   --  findModernNet - the virtio-net device's modern (1.0) register layout,
   --  if it has one in a single memory BAR (virtio 1.0 4.1.4).
   ---------------------------------------------------------------------------
   function probeMemoryBARSize
     (dev : PCIDeviceInfo; bar : Unsigned_8) return Unsigned_64;

   netModernBar : Unsigned_64 := 0;

   procedure findModernNet is
      NONE : constant Unsigned_8 := 16#FF#;
      capPtr, capId, next, cfgType, barIndex : Unsigned_8;
      capOff : Unsigned_32;
      commonBar, notifyBar, deviceBar : Unsigned_8 := NONE;
      lo, hi : Unsigned_32;
      bar : Unsigned_8;
      size : Unsigned_64;
   begin
      netModern := False;
      capPtr := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, PCI_CAP_PTR) and 16#FC#;
      for hop in 1 .. 48 loop
         exit when capPtr < 16#40# or else capPtr > 16#F0#;
         capId := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, capPtr);
         next := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, capPtr + 1) and 16#FC#;
         if capId = PCI_CAP_ID_VENDOR_SPECIFIC then
            cfgType := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, capPtr + 3);
            barIndex := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, capPtr + 4);
            capOff := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, capPtr + 8);
            case cfgType is
               when VIRTIO_PCI_CAP_COMMON_CFG =>
                  commonBar := barIndex;
                  netCommonOff := Unsigned_64 (capOff);
               when VIRTIO_PCI_CAP_NOTIFY_CFG =>
                  notifyBar := barIndex;
                  netNotifyOff := Unsigned_64 (capOff);
                  netNotifyMult := Unsigned_64
                    (pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, capPtr + 16));
               when VIRTIO_PCI_CAP_DEVICE_CFG =>
                  deviceBar := barIndex;
                  netDeviceOff := Unsigned_64 (capOff);
               when others =>
                  null;
            end case;
         end if;
         capPtr := next;
      end loop;
      if commonBar > 5 or else notifyBar /= commonBar or else deviceBar /= commonBar then
         return;   --  absent, or split across BARs (not supported)
      end if;
      bar := PCI_BASEADDR_0 + commonBar * 4;
      lo := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, bar);
      if (lo and 1) /= 0 or else (commonBar = 5 and then (lo and 6) = 4) then
         return;   --  an I/O BAR, or a 64-bit BAR with no high half
      end if;
      netModernBar := Unsigned_64 (lo and 16#FFFF_FFF0#);
      if (lo and 6) = 4 then
         hi := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, bar + 4);
         netModernBar := netModernBar or Shift_Left (Unsigned_64 (hi), 32);
      end if;
      size := probeMemoryBARSize (netDev, bar);
      if netModernBar = 0 or else size < 4096 or else netModernBar mod 4096 /= 0 then
         return;
      end if;
      netModernBytes := Unsigned_64'Min
        (size, CuBit.Virtio_Net_Control.Maximum_Modern_Bytes);
      netModernBytes := netModernBytes - netModernBytes mod 4096;
      netModern := True;
   end findModernNet;

   ---------------------------------------------------------------------------
   -- Setup virtio-net driver: PCI config, DMA, capabilities
   ---------------------------------------------------------------------------
   procedure setupVirtioNet is
      bar0Raw   : Unsigned_32;
      bar0Base  : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;
      ret       : Unsigned_64;

      --  Rings, a full queue of receive buffers and the transmit buffers
      --  (virtio-net's main.adb lays them out).
      DMA_ORDER : constant Unsigned_64 := 8;  --  256 pages = 1 MiB
      DMA_PAGES : constant Unsigned_64 := 2 ** Natural (DMA_ORDER);
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
   begin
      if not netDev.found or virtioNetPID = 0 then
         return;
      end if;

      --  A modern device (virtio 1.0, as a cloud host may offer only)
      --  is preferred: common, notification and device configuration in
      --  one memory BAR. Otherwise the legacy I/O transport (BAR0).
      findModernNet;
      if not netModern then
         bar0Raw := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func,
                                     PCI_BASEADDR_0);
         bar0Base := Unsigned_64 (bar0Raw and 16#FFFC#);

         if (bar0Raw and 1) = 0 or else bar0Base = 0 or else
            bar0Base > 16#FFE0#
         then
            debugPrint ("devmgr: virtio-net has neither modern nor legacy transport" & LF);
            return;
         end if;
      end if;

      --  Do not trust the firmware's legacy interrupt-line byte as an APIC
      --  route. Require MSI-X and keep the function masked until the driver
      --  installs its table entry and queue vectors. No periodic fallback.
      if (pciReadConfig16 (netDev.bus, netDev.slot, netDev.func,
                           PCI_STATUS) and 16#10#) = 0
      then
         return;
      end if;
      netMSIXCap := pciReadConfig8
        (netDev.bus, netDev.slot, netDev.func, PCI_CAP_PTR) and 16#FC#;
      for hop in 1 .. 48 loop
         exit when netMSIXCap < 16#40# or else netMSIXCap > 16#F4#;
         exit when pciReadConfig8
           (netDev.bus, netDev.slot, netDev.func, netMSIXCap) = PCI_CAP_ID_MSIX;
         netMSIXCap := pciReadConfig8
           (netDev.bus, netDev.slot, netDev.func, netMSIXCap + 1) and 16#FC#;
      end loop;
      if netMSIXCap < 16#40# or else netMSIXCap > 16#F4# or else
         pciReadConfig8 (netDev.bus, netDev.slot, netDev.func, netMSIXCap) /=
           PCI_CAP_ID_MSIX
      then
         debugPrint ("devmgr: virtio-net requires MSI-X" & LF);
         return;
      end if;
      declare
         tableInfo : constant Unsigned_32 := pciReadConfig32
           (netDev.bus, netDev.slot, netDev.func, netMSIXCap + 4);
         bir : constant Unsigned_8 := Unsigned_8 (tableInfo and 7);
         bar : Unsigned_8;
         lo, hi : Unsigned_32;
         base, size, offset, tableAddress : Unsigned_64;
         control : Unsigned_16;
      begin
         if bir > 5 then return; end if;
         bar := PCI_BASEADDR_0 + bir * 4;
         lo := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, bar);
         if (lo and 1) /= 0 or else (lo and 6) not in 0 | 4 or else
           (bir = 5 and then (lo and 6) = 4)
         then
            return;
         end if;
         base := Unsigned_64 (lo and 16#FFFF_FFF0#);
         if (lo and 6) = 4 then
            hi := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func, bar + 4);
            base := base or Shift_Left (Unsigned_64 (hi), 32);
         end if;
         size := probeMemoryBARSize (netDev, bar);
         offset := Unsigned_64 (tableInfo and 16#FFFF_FFF8#);
         if base = 0 or else size < 16 or else offset > size - 16 or else
            base > Unsigned_64'Last - size
         then
            return;
         end if;
         tableAddress := base + offset;
         netTableOffset := tableAddress mod 4096;
         if netTableOffset > CuBit.Virtio_Net_Control.Table_Offset'Last then
            return;
         end if;
         ret := mapInto (virtioNetPID, tableAddress - netTableOffset,
           CuBit.Virtio_Net_Control.Table_Virtual_Address, 1, MAP_FLAG_IO);
         if ret = reterr then return; end if;
         ret := enableIrq (CuBit.Virtio_Net_Control.Device_Vector,
           virtioNetPID, 0, messageSignaled => True);
         if ret = reterr then return; end if;
         control := pciReadConfig16
           (netDev.bus, netDev.slot, netDev.func, netMSIXCap + 2);
         pciWriteConfig16 (netDev.bus, netDev.slot, netDev.func,
           netMSIXCap + 2, control or 16#C000#);
      end;

      --  Enable I/O + MMIO + bus master; disable legacy INTx output.
      pciCmd := pciReadConfig16 (netDev.bus, netDev.slot, netDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (netDev.bus, netDev.slot, netDev.func,
                        PCI_COMMAND, pciCmd or 16#0407#);

      --  Allocate DMA
      dmaPhys := allocDma (virtioNetPID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-net DMA alloc failed" & LF);
         return;
      end if;

      if netModern then
         --  The register BAR, mapped where the driver expects it.
         ret := mapInto (virtioNetPID, netModernBar,
           CuBit.Virtio_Net_Control.Modern_Virtual_Address,
           netModernBytes / 4096, MAP_FLAG_IO);
         if ret = reterr then
            debugPrint ("devmgr: virtio-net register BAR map failed" & LF);
            return;
         end if;
         debugPrint ("devmgr: virtio-net modern (virtio 1.0) transport" & LF);
      else
         --  Slot 4: CAP_IOPORT for BAR0 (32 ports)
         mintCap (virtioNetPID, CAP_IOPORT, bar0Base, 32,
                  RIGHT_READ or RIGHT_WRITE, 4);
      end if;

      --  Slot 5: CAP_IRQ for device interrupt
      mintCap (virtioNetPID, CAP_IRQ, CuBit.Virtio_Net_Control.Device_Vector,
               0, RIGHT_READ, 5);

      --  Slot 6: CAP_DEVICE_MEM for DMA region
      mintCap (virtioNetPID, CAP_DEVICE_MEM, 0, DMA_SIZE,
               RIGHT_READ or RIGHT_WRITE, 6);

      --  Publish BAR0 via sysinfo
      ret := setSysinfo (SYSINFO_NET_IOBASE, bar0Base);
      netIOBase := bar0Base;
      netTransportReady := True;

      debugPrint ("devmgr: virtio-net setup complete" & LF);
   end setupVirtioNet;

   ---------------------------------------------------------------------------
   -- Setup HDA driver: PCI config, DMA, capabilities
   -- Follows the same pattern as setupNvme.
   ---------------------------------------------------------------------------
   procedure setupHDA is
      bar0Lo    : Unsigned_32;
      bar0Hi    : Unsigned_32;
      bar0Phys  : Unsigned_64;
      irqLine   : Unsigned_8;
      irqVector : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;
      ret       : Unsigned_64;
      msiCap    : Unsigned_8 := 0;
      msiReady  : Boolean := False;

      DMA_ORDER : constant Unsigned_64 := 5;   --  32 pages = 128KB
      DMA_PAGES : constant Unsigned_64 := 32;
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
      HDA_MSI_VECTOR : constant Unsigned_64 := 45;
   begin
      if not hdaDev.found or hdaPID = 0 then
         return;
      end if;

      --  Read BAR0 (64-bit MMIO)
      bar0Lo := pciReadConfig32 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                                 PCI_BASEADDR_0);
      bar0Hi := pciReadConfig32 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                                 PCI_BASEADDR_1);
      bar0Phys := Shift_Left (Unsigned_64 (bar0Hi), 32) or
                  Unsigned_64 (bar0Lo and 16#FFFF_FFF0#);

      --  Read IRQ line, compute vector (legacy PCI: vector = 32 + line)
      irqLine := pciReadConfig8 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                                 PCI_INTERRUPT_LINE);
      irqVector := 32 + Unsigned_64 (irqLine);

      --  Enable bus master + memory space
      pciCmd := pciReadConfig16 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);

      --  Prefer a dedicated MSI vector. Besides avoiding legacy PCI IRQ
      --  sharing, MSI is edge-triggered and therefore fits a userspace driver:
      --  the kernel may EOI immediately without a level line retriggering
      --  before HDA has cleared its stream status.
      if (pciReadConfig16 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                           PCI_STATUS) and 16#0010#) /= 0
      then
         msiCap := pciReadConfig8
           (hdaDev.bus, hdaDev.slot, hdaDev.func, PCI_CAP_PTR) and 16#FC#;
         for hop in 1 .. 48 loop
            exit when msiCap < 16#40# or else msiCap > 16#F0#;
            if pciReadConfig8
                 (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap) =
               PCI_CAP_ID_MSI
            then
               exit;
            end if;
            msiCap := pciReadConfig8
              (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap + 1) and 16#FC#;
         end loop;

         if msiCap >= 16#40# and then msiCap <= 16#F0# and then
            pciReadConfig8
              (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap) = PCI_CAP_ID_MSI
         then
            setupMsi : declare
               control : Unsigned_16 := pciReadConfig16
                 (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap + 2);
               dataOff : Unsigned_8;
            begin
               --  Destination APIC ID 0, fixed delivery, physical mode.
               pciWriteConfig32
                 (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap + 4,
                  16#FEE0_0000#);
               if (control and 16#0080#) /= 0 then
                  pciWriteConfig32
                    (hdaDev.bus, hdaDev.slot, hdaDev.func, msiCap + 8, 0);
                  dataOff := msiCap + 12;
               else
                  dataOff := msiCap + 8;
               end if;
               pciWriteConfig16
                 (hdaDev.bus, hdaDev.slot, hdaDev.func, dataOff,
                  Unsigned_16 (HDA_MSI_VECTOR));

               ret := enableIrq
                 (HDA_MSI_VECTOR, hdaPID, 0, messageSignaled => True);
               if ret /= reterr then
                  --  One message only, then enable MSI. Disable the device's
                  --  legacy INTx output so it cannot assert the shared PIRQ.
                  control := (control and not 16#0070#) or 1;
                  pciWriteConfig16
                    (hdaDev.bus, hdaDev.slot, hdaDev.func,
                     msiCap + 2, control);
                  pciCmd := pciReadConfig16
                    (hdaDev.bus, hdaDev.slot, hdaDev.func, PCI_COMMAND);
                  pciWriteConfig16
                    (hdaDev.bus, hdaDev.slot, hdaDev.func,
                     PCI_COMMAND, pciCmd or 16#0400#);
                  irqVector := HDA_MSI_VECTOR;
                  msiReady := True;
                  debugPrint ("devmgr: HDA using MSI" & LF);
               end if;
            end setupMsi;
         end if;
      end if;

      if not msiReady then
         --  Compatibility fallback. The long-term legacy-INTx path needs a
         --  mask/ack/unmask protocol before it can offer the same latency.
         ret := enableIrq (irqVector, hdaPID, 0);
         debugPrint ("devmgr: HDA using legacy INTx fallback" & LF);
      end if;

      --  Allocate DMA
      dmaPhys := allocDma (hdaPID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: HDA DMA alloc failed" & LF);
         return;
      end if;

      --  Slot 4: CAP_DEVICE_MEM for BAR0 MMIO (16KB = 4 pages)
      mintCap (hdaPID, CAP_DEVICE_MEM, bar0Phys, 16384,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 5: CAP_IRQ for device interrupt
      mintCap (hdaPID, CAP_IRQ, irqVector, 0, RIGHT_READ, 5);

      --  Slot 6: CAP_DEVICE_MEM for DMA region
      mintCap (hdaPID, CAP_DEVICE_MEM, 0, DMA_SIZE,
               RIGHT_READ or RIGHT_WRITE, 6);

      --  Publish HDA BAR0 and DMA phys via sysinfo
      ret := setSysinfo (SYSINFO_HDA_BAR0, bar0Phys);
      ret := setSysinfo (SYSINFO_HDA_DMA_PHYS, dmaPhys);

      debugPrint ("devmgr: HDA setup complete" & LF);
   end setupHDA;

   ---------------------------------------------------------------------------
   -- Setup VirtIO-GPU driver: parse modern PCI caps, DMA, capabilities
   ---------------------------------------------------------------------------
   procedure setupVirtioGpu is
      capPtr : Unsigned_8;
      capId  : Unsigned_8;
      next   : Unsigned_8;
      cfgType : Unsigned_8;
      barIndex : Unsigned_8;
      capOff : Unsigned_32;
      notifyMult : Unsigned_32 := 0;
      commonOff : Unsigned_32 := 0;
      notifyOff : Unsigned_32 := 0;
      isrOff : Unsigned_32 := 0;
      deviceOff : Unsigned_32 := 0;
      commonBar : Unsigned_8 := 16#FF#;
      notifyBar : Unsigned_8 := 16#FF#;
      isrBar : Unsigned_8 := 16#FF#;
      deviceBar : Unsigned_8 := 16#FF#;
      barRaw : Unsigned_32;
      barPhys : Unsigned_64;
      irqLine : Unsigned_8;
      irqVector : Unsigned_64;
      pciCmd : Unsigned_16;
      dmaPhys : Unsigned_64;
      ret : Unsigned_64;

      -- Two independently owned 8 MiB banks, within the allocator's supported
      -- block size. A missing second bank must not prevent the boot output.
      secondDmaPhys : Unsigned_64;
      DMA_ORDER : constant Unsigned_64 := 11;
      DMA_PAGES : constant Unsigned_64 := 2 ** Natural (DMA_ORDER);
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
      BAR_MAP_SIZE : constant Unsigned_64 := 65536;

      function readCap32 (offset : Unsigned_8) return Unsigned_32 is
      begin
         return pciReadConfig32 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 capPtr + offset);
      end readCap32;
   begin
      if not gpuDev.found or virtioGpuPID = 0 then
         return;
      end if;

      capPtr := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                PCI_CAP_PTR) and 16#FC#;
      while capPtr /= 0 loop
         capId := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                  capPtr);
         next := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 capPtr + 1) and 16#FC#;

         if capId = PCI_CAP_ID_VENDOR_SPECIFIC then
            cfgType := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                       capPtr + 3);
            barIndex := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                        capPtr + 4);
            capOff := readCap32 (8);

            case cfgType is
               when VIRTIO_PCI_CAP_COMMON_CFG =>
                  commonBar := barIndex;
                  commonOff := capOff;
               when VIRTIO_PCI_CAP_NOTIFY_CFG =>
                  notifyBar := barIndex;
                  notifyOff := capOff;
                  notifyMult := readCap32 (16);
               when VIRTIO_PCI_CAP_ISR_CFG =>
                  isrBar := barIndex;
                  isrOff := capOff;
               when VIRTIO_PCI_CAP_DEVICE_CFG =>
                  deviceBar := barIndex;
                  deviceOff := capOff;
               when others =>
                  null;
            end case;
         end if;

         capPtr := next;
      end loop;

      if commonBar = 16#FF# or else notifyBar = 16#FF# or else
         isrBar = 16#FF# or else notifyMult = 0
      then
         debugPrint ("devmgr: virtio-gpu missing modern caps" & LF);
         return;
      end if;

      if commonBar /= notifyBar or else commonBar /= isrBar or else
         (deviceBar /= 16#FF# and then commonBar /= deviceBar)
      then
         debugPrint ("devmgr: virtio-gpu split BARs unsupported" & LF);
         return;
      end if;

      barRaw := pciReadConfig32
        (gpuDev.bus, gpuDev.slot, gpuDev.func,
         PCI_BASEADDR_0 + commonBar * 4);
      if (barRaw and 1) /= 0 then
         debugPrint ("devmgr: virtio-gpu common BAR is I/O, unsupported" & LF);
         return;
      end if;
      barPhys := Unsigned_64 (barRaw and 16#FFFF_FFF0#);

      irqLine := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 PCI_INTERRUPT_LINE);
      irqVector := 32 + Unsigned_64 (irqLine);

      pciCmd := pciReadConfig16 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);
      ret := enableIrq (irqVector, virtioGpuPID, 0);

      dmaPhys := allocDma (virtioGpuPID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-gpu DMA alloc failed" & LF);
         return;
      end if;

      secondDmaPhys := allocDma
        (virtioGpuPID, DMA_ORDER, DMA_VIRT_BASE + DMA_SIZE);
      if secondDmaPhys = reterr then
         secondDmaPhys := 0;
         debugPrint ("devmgr: second GPU DMA bank unavailable" & LF);
      end if;
      ret := setSysinfo (SYSINFO_GPU_SECOND_DMA_PHYS, secondDmaPhys);

      --  Slot 4: virtio modern MMIO BAR.
      mintCap (virtioGpuPID, CAP_DEVICE_MEM, barPhys, BAR_MAP_SIZE,
               RIGHT_READ or RIGHT_WRITE, 4);
      --  Slot 5: IRQ.
      mintCap (virtioGpuPID, CAP_IRQ, irqVector, 0, RIGHT_READ, 5);
      --  Slot 6: mapped DMA banks; actual frame ownership is tracked per bank.
      mintCap (virtioGpuPID, CAP_DEVICE_MEM, 0,
               (if secondDmaPhys = 0 then DMA_SIZE else DMA_SIZE * 2),
               RIGHT_READ or RIGHT_WRITE, 6);

      ret := setSysinfo (SYSINFO_GPU_BAR0, barPhys);
      ret := setSysinfo (SYSINFO_GPU_DMA_PHYS, dmaPhys);
      ret := setSysinfo (SYSINFO_GPU_COMMON_OFF, Unsigned_64 (commonOff));
      ret := setSysinfo (SYSINFO_GPU_NOTIFY_OFF, Unsigned_64 (notifyOff));
      ret := setSysinfo (SYSINFO_GPU_ISR_OFF, Unsigned_64 (isrOff));
      ret := setSysinfo (SYSINFO_GPU_DEVICE_OFF, Unsigned_64 (deviceOff));
      ret := setSysinfo (SYSINFO_GPU_NOTIFY_MULT, Unsigned_64 (notifyMult));
      if gpuIsPrimary then
         ret := setSysinfo (SYSINFO_GPU_IS_PRIMARY, 1);
      else
         ret := setSysinfo (SYSINFO_GPU_IS_PRIMARY, 0);
      end if;

      debugPrint ("devmgr: virtio-gpu setup complete" & LF);
   end setupVirtioGpu;

   ---------------------------------------------------------------------------
   -- Setup PS/2 driver: grant IOPORT + IRQ capabilities
   ---------------------------------------------------------------------------
   procedure setupPS2 is
      ret : Unsigned_64;
   begin
      if ps2PID = 0 then
         return;
      end if;

      --  Slot 4: CAP_IOPORT for data port 0x60 (1 port)
      mintCap (ps2PID, CAP_IOPORT, 16#60#, 1,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 5: CAP_IOPORT for status/command port 0x64 (1 port)
      mintCap (ps2PID, CAP_IOPORT, 16#64#, 1,
               RIGHT_READ or RIGHT_WRITE, 5);

      --  Slot 6: CAP_IRQ for keyboard (vector 33)
      mintCap (ps2PID, CAP_IRQ, 33, 0, RIGHT_READ, 6);

      --  Slot 7: CAP_IRQ for mouse (vector 44)
      mintCap (ps2PID, CAP_IRQ, 44, 0, RIGHT_READ, 7);

      --  IRQ ownership is not publication authority. These two read-only
      --  notification grants independently authorize the PS/2 driver to
      --  publish only to the registered keyboard and mouse consumers.
      mintCap (ps2PID, CAP_NOTIFICATION, DRIVER_KEYBOARD, 0,
               RIGHT_READ, 8);
      mintCap (ps2PID, CAP_NOTIFICATION, DRIVER_MOUSE, 0,
               RIGHT_READ, 9);

      --  Register IRQ owners and enable IOAPIC routing
      ret := enableIrq (33, ps2PID, 0);
      ret := enableIrq (44, ps2PID, 0);

      debugPrint ("devmgr: PS/2 setup complete" & LF);
   end setupPS2;

   ---------------------------------------------------------------------------
   -- Probe a memory BAR's implemented size while its decode is disabled.
   -- Returns zero for an I/O BAR or a malformed/unsupported result.
   ---------------------------------------------------------------------------
   function probeMemoryBARSize
     (dev : PCIDeviceInfo; bar : Unsigned_8) return Unsigned_64
   is
      command : constant Unsigned_16 :=
        pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND);
      originalLo : constant Unsigned_32 :=
        pciReadConfig32 (dev.bus, dev.slot, dev.func, bar);
      originalHi : constant Unsigned_32 :=
        pciReadConfig32 (dev.bus, dev.slot, dev.func, bar + 4);
      probeLo : Unsigned_32;
      probeHi : Unsigned_32 := 0;
      barMask : Unsigned_64;
      size    : Unsigned_64;
      is64Bit : constant Boolean := (originalLo and 16#6#) = 16#4#;
   begin
      if (originalLo and 1) /= 0 then
         return 0;
      end if;

      pciWriteConfig16
        (dev.bus, dev.slot, dev.func, PCI_COMMAND,
         command and not Unsigned_16'(3));
      pciWriteConfig32
        (dev.bus, dev.slot, dev.func, bar, 16#FFFF_FFFF#);
      if is64Bit then
         pciWriteConfig32
           (dev.bus, dev.slot, dev.func, bar + 4, 16#FFFF_FFFF#);
      end if;

      probeLo := pciReadConfig32
        (dev.bus, dev.slot, dev.func, bar);
      if is64Bit then
         probeHi := pciReadConfig32
           (dev.bus, dev.slot, dev.func, bar + 4);
      end if;

      --  Restore the BAR before re-enabling memory decoding.
      if is64Bit then
         pciWriteConfig32
           (dev.bus, dev.slot, dev.func, bar + 4, originalHi);
      end if;
      pciWriteConfig32
        (dev.bus, dev.slot, dev.func, bar, originalLo);
      pciWriteConfig16
        (dev.bus, dev.slot, dev.func, PCI_COMMAND, command);

      if is64Bit then
         barMask := Shift_Left (Unsigned_64 (probeHi), 32) or
                    Unsigned_64 (probeLo and 16#FFFF_FFF0#);
         size := (not barMask) + 1;
      else
         barMask := Unsigned_64 (probeLo and 16#FFFF_FFF0#);
         size := Unsigned_64 ((not Unsigned_32 (barMask)) + 1);
      end if;

      if size < 4096 or else size > 1024 * 1024 or else
         (size and (size - 1)) /= 0
      then
         return 0;
      end if;
      return size;
   end probeMemoryBARSize;

   ---------------------------------------------------------------------------
   -- Setup xHCI: grant the driver only its precisely sized BAR and a bounded
   -- DMA arena, then send the physical configuration over a private endpoint.
   ---------------------------------------------------------------------------
   procedure setupXHCI is
      type XHCI_Interrupt_Mode is
        (XHCI_INTERRUPT_POLLING,
         XHCI_INTERRUPT_MSI,
         XHCI_INTERRUPT_MSIX);
      for XHCI_Interrupt_Mode use
        (XHCI_INTERRUPT_POLLING => 0,
         XHCI_INTERRUPT_MSI     => 1,
         XHCI_INTERRUPT_MSIX    => 2);

      bar0Lo    : Unsigned_32;
      bar0Hi    : Unsigned_32;
      bar0Phys  : Unsigned_64;
      barSize   : Unsigned_64;
      barPages  : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;
      cfgMsg    : Message;
      replyTag  : MessageTag;
      msiCap    : Unsigned_8 := 0;
      msixCap   : Unsigned_8 := 0;
      interruptMode : XHCI_Interrupt_Mode := XHCI_INTERRUPT_POLLING;
      msixTableOffset : Unsigned_64 := 0;
      irqRet    : Unsigned_64;

      scratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      dmaLayout : XHCI_DMA_Layout.Allocation;
      --  Bootstrap-only, uncached capability-register mapping. devmgr already
      --  owns physical mapping authority; it never touches rings or payloads.
      CAPABILITY_PROBE_BASE : constant Unsigned_64 := 16#0000_6000_1000_0000#;
      DEVMGR_XHCI_SLOT : constant Unsigned_64 := 3;
      OP_XHCI_CONFIGURE : constant Unsigned_32 := 16#0220#;
      XHCI_MSI_VECTOR : constant Unsigned_64 := 48;
   begin
      if not xhciDev.found or else xhciPID = 0 then
         return;
      end if;

      bar0Lo := pciReadConfig32
        (xhciDev.bus, xhciDev.slot, xhciDev.func, PCI_BASEADDR_0);
      bar0Hi := pciReadConfig32
        (xhciDev.bus, xhciDev.slot, xhciDev.func, PCI_BASEADDR_1);

      if (bar0Lo and 1) /= 0 then
         debugPrint ("devmgr: xHCI BAR0 is not MMIO" & LF);
         return;
      elsif (bar0Lo and 16#6#) = 16#4# then
         bar0Phys := Shift_Left (Unsigned_64 (bar0Hi), 32) or
                     Unsigned_64 (bar0Lo and 16#FFFF_FFF0#);
      else
         bar0Phys := Unsigned_64 (bar0Lo and 16#FFFF_FFF0#);
      end if;

      barSize := probeMemoryBARSize (xhciDev, PCI_BASEADDR_0);
      if bar0Phys = 0 or else bar0Phys mod 4096 /= 0 or else
         barSize < 4096 or else barSize > 256 * 4096 then
         debugPrint ("devmgr: xHCI BAR probe failed" & LF);
         return;
      end if;
      barPages := (barSize + 4095) / 4096;

      pciCmd := pciReadConfig16
        (xhciDev.bus, xhciDev.slot, xhciDev.func, PCI_COMMAND);
      pciWriteConfig16
        (xhciDev.bus, xhciDev.slot, xhciDev.func,
         PCI_COMMAND, pciCmd or 16#0006#);

      --  xHCI is a latency-sensitive producer. Prefer an MSI outside the
      --  legacy PIC range so its driver can block on event-ring changes
      --  instead of polling on the millisecond system tick.
      if (pciReadConfig16 (xhciDev.bus, xhciDev.slot, xhciDev.func,
                           PCI_STATUS) and 16#0010#) /= 0
      then
         msiCap := pciReadConfig8
           (xhciDev.bus, xhciDev.slot, xhciDev.func,
            PCI_CAP_PTR) and 16#FC#;
         for hop in 1 .. 48 loop
            exit when msiCap < 16#40# or else msiCap > 16#F0#;
            if pciReadConfig8
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msiCap) =
               PCI_CAP_ID_MSI
            then
               exit;
            end if;
            msiCap := pciReadConfig8
              (xhciDev.bus, xhciDev.slot, xhciDev.func,
               msiCap + 1) and 16#FC#;
         end loop;

         if msiCap >= 16#40# and then msiCap <= 16#F0# and then
            pciReadConfig8
              (xhciDev.bus, xhciDev.slot, xhciDev.func, msiCap) =
               PCI_CAP_ID_MSI
         then
            setupMsi : declare
               control : Unsigned_16 := pciReadConfig16
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msiCap + 2);
               dataOff : Unsigned_8;
            begin
               --  Destination APIC ID 0, fixed delivery, physical mode.
               pciWriteConfig32
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msiCap + 4,
                  16#FEE0_0000#);
               if (control and 16#0080#) /= 0 then
                  pciWriteConfig32
                    (xhciDev.bus, xhciDev.slot, xhciDev.func,
                     msiCap + 8, 0);
                  dataOff := msiCap + 12;
               else
                  dataOff := msiCap + 8;
               end if;
               pciWriteConfig16
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, dataOff,
                  Unsigned_16 (XHCI_MSI_VECTOR));

               irqRet := enableIrq
                 (XHCI_MSI_VECTOR, xhciPID, 0,
                  messageSignaled => True);
               if irqRet /= reterr then
                  --  Request one message and disable legacy INTx before MSI
                  --  is enabled. xhci.drv enables the controller interrupter
                  --  only after initialization has completed.
                  control := (control and not 16#0070#) or 1;
                  pciWriteConfig16
                    (xhciDev.bus, xhciDev.slot, xhciDev.func,
                     msiCap + 2, control);
                  pciCmd := pciReadConfig16
                    (xhciDev.bus, xhciDev.slot, xhciDev.func, PCI_COMMAND);
                  pciWriteConfig16
                    (xhciDev.bus, xhciDev.slot, xhciDev.func,
                     PCI_COMMAND, pciCmd or 16#0400#);
                  interruptMode := XHCI_INTERRUPT_MSI;
                  debugPrint ("devmgr: xHCI using MSI" & LF);
               end if;
            end setupMsi;
         end if;
      end if;

      --  Many xHCI implementations, including QEMU's controller, expose
      --  MSI-X rather than MSI. Keep the PCI function masked until xhci.drv
      --  has validated and populated the BAR0-resident table entry.
      if interruptMode = XHCI_INTERRUPT_POLLING and then
         (pciReadConfig16 (xhciDev.bus, xhciDev.slot, xhciDev.func,
                           PCI_STATUS) and 16#0010#) /= 0
      then
         msixCap := pciReadConfig8
           (xhciDev.bus, xhciDev.slot, xhciDev.func,
            PCI_CAP_PTR) and 16#FC#;
         for hop in 1 .. 48 loop
            exit when msixCap < 16#40# or else msixCap > 16#F0#;
            if pciReadConfig8
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msixCap) =
               PCI_CAP_ID_MSIX
            then
               exit;
            end if;
            msixCap := pciReadConfig8
              (xhciDev.bus, xhciDev.slot, xhciDev.func,
               msixCap + 1) and 16#FC#;
         end loop;

         if msixCap >= 16#40# and then msixCap <= 16#F0# and then
            pciReadConfig8
              (xhciDev.bus, xhciDev.slot, xhciDev.func, msixCap) =
               PCI_CAP_ID_MSIX
         then
            setupMsix : declare
               control : Unsigned_16 := pciReadConfig16
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msixCap + 2);
               tableInfo : constant Unsigned_32 := pciReadConfig32
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msixCap + 4);
               tableBIR : constant Unsigned_32 := tableInfo and 7;
               tableOffset : constant Unsigned_64 :=
                 Unsigned_64 (tableInfo and 16#FFFF_FFF8#);
            begin
               if tableBIR = 0 and then barSize >= 16 and then
                  tableOffset mod 8 = 0 and then
                  tableOffset <= barSize - 16
               then
                  irqRet := enableIrq
                    (XHCI_MSI_VECTOR, xhciPID, 0,
                     messageSignaled => True);
                  if irqRet /= reterr then
                     --  MSI-X Enable plus Function Mask. The entry itself is
                     --  still inaccessible to devmgr by design; xhci.drv
                     --  owns BAR0 and fills it before setup returns.
                     control := control or 16#C000#;
                     pciWriteConfig16
                       (xhciDev.bus, xhciDev.slot, xhciDev.func,
                        msixCap + 2, control);
                     pciCmd := pciReadConfig16
                       (xhciDev.bus, xhciDev.slot, xhciDev.func,
                        PCI_COMMAND);
                     pciWriteConfig16
                       (xhciDev.bus, xhciDev.slot, xhciDev.func,
                        PCI_COMMAND, pciCmd or 16#0400#);
                     interruptMode := XHCI_INTERRUPT_MSIX;
                     msixTableOffset := tableOffset;
                     debugPrint ("devmgr: xHCI using MSI-X" & LF);
                  end if;
               end if;
            end setupMsix;
         end if;
      end if;

      if interruptMode = XHCI_INTERRUPT_POLLING then
         debugPrint ("devmgr: xHCI using queued polling fallback" & LF);
      end if;

      irqRet := mapInto (myPID, bar0Phys, CAPABILITY_PROBE_BASE, 1, MAP_FLAG_IO);
      if irqRet = reterr then
         debugPrint ("devmgr: xHCI capability mapping failed" & LF);
         return;
      end if;
      declare
         type Capability_Register is new Unsigned_32 with Volatile_Full_Access;
         parameters : Capability_Register with Import,
           Address => To_Address (Integer_Address (CAPABILITY_PROBE_BASE + 8));
      begin
         scratchpads := XHCI_Capabilities.Scratchpad_Count (Unsigned_32 (parameters));
      end;
      dmaLayout := XHCI_DMA_Layout.Plan (scratchpads);
      dmaPhys := allocDma (xhciPID, Unsigned_64 (dmaLayout.Order), DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: xHCI DMA allocation failed" & LF);
         return;
      end if;

      mintCap
        (xhciPID, CAP_DEVICE_MEM, bar0Phys, barPages * 4096,
         RIGHT_READ or RIGHT_WRITE, 4);
      mintCap
        (xhciPID, CAP_DEVICE_MEM, 0, XHCI_DMA_Layout.Bytes (dmaLayout),
         RIGHT_READ or RIGHT_WRITE, 6);
      if interruptMode /= XHCI_INTERRUPT_POLLING then
         --  Slot 5: receive only the explicitly registered xHCI MSI.
         mintCap
           (xhciPID, CAP_IRQ, XHCI_MSI_VECTOR, 0, RIGHT_READ, 5);
      end if;
      --  Slot 7: may publish mouse-class events only to the currently
      --  registered mouse consumer.  RIGHT_READ cannot register or replace
      --  that consumer; desktop.svc holds the complementary RIGHT_WRITE.
      mintCap
        (xhciPID, CAP_NOTIFICATION, DRIVER_MOUSE, 0, RIGHT_READ, 7);
      -- Keyboard publication is distinct from mouse publication and does
      -- not permit replacing the registered consumer.
      mintCap
        (xhciPID, CAP_NOTIFICATION, DRIVER_KEYBOARD, 0, RIGHT_READ, 8);

      --  Slot 3 belongs to devmgr and reaches only this xHCI process.
      grantEndpoint (myPID, xhciPID, DEVMGR_XHCI_SLOT, myPID);
      -- Authenticated driver diagnostics/bootstrap log-grant request only.
      grantEndpoint (xhciPID, myPID, CAP_SLOT_READY, xhciPID);
      assignCPU (xhciPID, "xhci.drv");
      resumeProc (xhciPID);

      cfgMsg :=
        (tag => (label => OP_XHCI_CONFIGURE,
                 length => 4, flags => 0, reserved => 0),
         authorityTag => 0,
         words =>
           [0 => bar0Phys,
            --  Low 32 bits: BAR pages. High 32 bits: allocation's scratchpad
            --  count. Driver checks it against a fresh HCSPARAMS2 read before
            --  accessing DMA; this does not grant arbitrary allocation size.
            1 => barPages or Shift_Left (Unsigned_64 (scratchpads), 32),
            2 => dmaPhys,
            3 =>
              Unsigned_64 (XHCI_Interrupt_Mode'Enum_Rep (interruptMode)) or
              Shift_Left (XHCI_MSI_VECTOR, 8) or
              Shift_Left (msixTableOffset, 16)]);
      replyTag := capCall (DEVMGR_XHCI_SLOT, cfgMsg);
      if replyTag.label = REPLY_OK then
         if interruptMode = XHCI_INTERRUPT_MSIX then
            --  The driver has installed and unmasked table entry zero. Drop
            --  only the function mask; leave MSI-X itself enabled.
            declare
               control : Unsigned_16 := pciReadConfig16
                 (xhciDev.bus, xhciDev.slot, xhciDev.func, msixCap + 2);
            begin
               control := control and not Unsigned_16 (16#4000#);
               pciWriteConfig16
                 (xhciDev.bus, xhciDev.slot, xhciDev.func,
                  msixCap + 2, control);
            end;
         end if;
         debugPrint ("devmgr: xHCI controller started" & LF);
         --  Read-only optical block endpoint; no raw controller authority.
         if filesystemPID /= 0 then
            grantEndpoint (filesystemPID, xhciPID, 12, filesystemPID);
            Optical_Storage_Ready := True;
         end if;
      else
         debugPrint ("devmgr: xHCI controller rejected setup" & LF);
         xhciPID := 0;
      end if;
   end setupXHCI;

   ---------------------------------------------------------------------------
   -- Main entry point
   ---------------------------------------------------------------------------

   initrdAddr   : Unsigned_64;
   initrdSize   : Unsigned_64;
   initrdPhys   : Unsigned_64;
   initrdPages  : Unsigned_64;
   ret          : Unsigned_64;
   msg          : Message;
   from         : ProcessID;

begin
   debugPrint ("devmgr: starting" & LF);
   declare
      Accepted : Boolean;
   begin
      Intel_Allocation_Growth.Configure
        (Intel_Allocations, Intel_Metadata_Quota, Intel_Record_Quota, Accepted);
      if not Accepted then
         debugPrint ("devmgr: Intel allocation metadata policy rejected" & LF);
      end if;
   end;

   --  Get initrd info from sysinfo
   initrdAddr := getInfo (SYSINFO_RAMDISK_ADDRESS);
   initrdSize := getInfo (SYSINFO_RAMDISK_SIZE);
   numCPUs    := getInfo (SYSINFO_NUM_CPUS);

   if initrdAddr = 0 or initrdSize = 0 then
      debugPrint ("devmgr: no initrd found, halting" & LF);
      ret := syscall (SYSCALL_EXIT);
      return;
   end if;

   --  Parse CPIO archive from initrd
   Cpio.init (cpioArchive,
              To_Address (Integer_Address (INITRD_BASE)),
              initrdSize,
              cpioOk);

   if not cpioOk then
      debugPrint ("devmgr: failed to parse CPIO initrd" & LF);
      ret := syscall (SYSCALL_EXIT);
      return;
   end if;

   debugPrint ("devmgr: CPIO initrd parsed" & LF);

   --  Bootstrap endpoint authority is established before any child runs.
   ret := registerDriver (DRIVER_DEVMGR);
   myPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR);

   --  A writable live image is served by a separate Block.Device.V1 driver.
   --  It starts before FS, so FS never calls a not-yet-running provider.
   if Cpio.findFile (cpioArchive, "live-rw.ext2") < cpioArchive.count then
      ramdiskPID := spawnFromBootStorage ("ramdisk.drv", 5);
      if ramdiskPID /= 0 and then ramdiskPID /= reterr then
         initrdPhys := virtToPhys (To_Address (Integer_Address (INITRD_BASE)));
         initrdPages := (initrdSize + 4095) / 4096;
         declare
            Mapped : Unsigned_64 := 0;
            Count : Unsigned_64;
         begin
            while Mapped < initrdPages loop
               Count := Unsigned_64'Min
                 (MAX_MAP_INTO_PAGES_PER_CALL, initrdPages - Mapped);
               ret := mapInto
                 (ramdiskPID, initrdPhys + Mapped * 4096,
                  INITRD_BASE + Mapped * 4096, Count, MAP_FLAG_RO);
               exit when ret = reterr;
               Mapped := Mapped + Count;
            end loop;
            if Mapped /= initrdPages then
               debugPrint ("devmgr: RAM block-driver seed mapping failed" & LF);
               ret := syscall (SYSCALL_EXIT);
               return;
            end if;
         end;
         assignCPU (ramdiskPID, "ramdisk.drv");
         mintCap (ramdiskPID, CAP_ENDPOINT, myPID, 0,
                  RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
         resumeProc (ramdiskPID);
         if not waitReady (ramdiskPID) then
            ret := syscall (SYSCALL_EXIT);
            return;
         end if;
      else
         debugPrint ("devmgr: RAM block driver unavailable" & LF);
         ret := syscall (SYSCALL_EXIT);
         return;
      end if;
   end if;

   --  Scan PCI bus for devices
   scanPCI;

   -----------------------------------------------------------------------
   -- Phase 1: Spawn filesystem server (auto-assign PID)
   -----------------------------------------------------------------------
   filesystemPID := spawnFromBootStorage ("filesystem.svc", 5, 0);
   if filesystemPID = reterr then
      filesystemPID := 0;
      debugPrint ("devmgr: filesystem.svc spawn failed" & LF);
   end if;

   --  Register FS PID in kernel well-known service registry
   if filesystemPID /= 0 then
      ret := setWellKnown (ROLE_FILESYSTEM, filesystemPID);
   end if;

   if filesystemPID /= 0 then
      --  Map initrd into FS server for serving ramdisk files
      --  Get the physical address: kernel mapped initrd at INITRD_BASE in
      --  our space, but the physical address was stored for us.
      initrdPhys := virtToPhys (To_Address (Integer_Address (INITRD_BASE)));
      initrdPages := (initrdSize + 4095) / 4096;

      --  MAP_INTO deliberately bounds each kernel entry.  A live image may
      --  be larger than one request (the Workbench alone pushes it beyond
      --  4 MiB), so map the complete archive in consecutive chunks.
      declare
         mappedPages : Unsigned_64 := 0;
         pageCount   : Unsigned_64;
      begin
         ret := 0;
         while mappedPages < initrdPages loop
            pageCount := Unsigned_64'Min
              (MAX_MAP_INTO_PAGES_PER_CALL, initrdPages - mappedPages);
            ret := mapInto
              (filesystemPID,
               initrdPhys + mappedPages * 4096,
               INITRD_BASE + mappedPages * 4096,
               pageCount,
               MAP_FLAG_RO);
            exit when ret = reterr;
            mappedPages := mappedPages + pageCount;
         end loop;
      end;
      if ret = reterr then
         debugPrint ("devmgr: initrd mapping into FS failed" & LF);
      end if;

      --  Set sysinfo for ramdisk (so FS server can query it)
      ret := setSysinfo (SYSINFO_RAMDISK_ADDRESS, INITRD_BASE);
      ret := setSysinfo (SYSINFO_RAMDISK_SIZE, initrdSize);

      --  CAP_NOTIFICATION for DRIVER_FS registration (slot 7)
      mintCap (filesystemPID, CAP_NOTIFICATION, DRIVER_FS, 0,
               RIGHT_WRITE, 7);


      if ramdiskPID /= 0 then
         grantEndpoint
           (filesystemPID, ramdiskPID, CAP_SLOT_RAMDISK, filesystemPID);
      end if;
      assignCPU (filesystemPID, "filesystem.svc");
      mintCap (filesystemPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (filesystemPID);
      debugPrint ("devmgr: filesystem server started" & LF);

      --  Wait for FS to signal ready, then grant ourselves a FS endpoint
      --  at slot 1 so we can send ACL management messages.
      if not waitReady (filesystemPID) then
         filesystemPID := 0;
      end if;

      if myPID /= 0 and myPID /= Unsigned_64'Last and
         filesystemPID /= 0
      then
         grantEndpoint (myPID, filesystemPID, 1, myPID);
         debugPrint ("devmgr: minted FS endpoint at slot 1" & LF);
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2: Spawn and setup disk drivers
   -----------------------------------------------------------------------

   --  ATA driver
   ataPID := spawnFromBootStorage ("ata.drv", 5);
   if ataPID = reterr then
      ataPID := 0;
   end if;
   if ataPID /= 0 then
      setupAta;
      --  CAP_NOTIFICATION for DRIVER_ATA registration (slot 7)
      mintCap (ataPID, CAP_NOTIFICATION, DRIVER_ATA, 0,
               RIGHT_WRITE, 7);
      assignCPU (ataPID, "ata.drv");
      mintCap (ataPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (ataPID);
      debugPrint ("devmgr: ATA driver started" & LF);

      if not waitReady (ataPID) then
         ataPID := 0;
      end if;

      --  Grant ATA endpoint to FS server (slot 10)
      if filesystemPID /= 0 and ataPID /= 0 then
         grantEndpoint (filesystemPID, ataPID, 10, filesystemPID);
      end if;
   end if;

   --  NVMe driver
   nvmePID := spawnFromBootStorage ("nvme.drv", 5);
   if nvmePID = reterr then
      nvmePID := 0;
   end if;
   if nvmePID /= 0 then
      setupNvme;
      --  CAP_NOTIFICATION for DRIVER_NVME registration (slot 7)
      mintCap (nvmePID, CAP_NOTIFICATION, DRIVER_NVME, 0,
               RIGHT_WRITE, 7);
      assignCPU (nvmePID, "nvme.drv");
      mintCap (nvmePID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      grantEndpoint (myPID, nvmePID, DEVMGR_NVME_SLOT, myPID);
      resumeProc (nvmePID);
      declare
         use CuBit.NVMe_Control;
         cfg : Message :=
           (if nvmeMSIX then
              (tag => (label => Operation'Enum_Rep (Configure_MSIX),
                       length => 2, flags => 0, reserved => 0),
               authorityTag => 0,
               words => [0 => nvmeTableOffset, 1 => Device_Vector,
                         others => 0])
            else
              (tag => (label => Operation'Enum_Rep (Configure_Polled),
                       length => 0, flags => 0, reserved => 0),
               authorityTag => 0, words => [others => 0]));
         response : MessageTag;
         control : Unsigned_16;
      begin
         response := capCall (DEVMGR_NVME_SLOT, cfg);
         if nvmeMSIX then
            control := pciReadConfig16
              (nvmeDev.bus, nvmeDev.slot, nvmeDev.func, nvmeMSIXCap + 2);
            if response.label = REPLY_OK then
               --  Entry zero is filled: clear the function mask.
               pciWriteConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                 nvmeMSIXCap + 2, control and not Unsigned_16'(16#4000#));
               debugPrint ("devmgr: NVMe using MSI-X" & LF);
            else
               --  The driver polls: disable MSI-X again.
               pciWriteConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                 nvmeMSIXCap + 2, control and not Unsigned_16'(16#8000#));
            end if;
         end if;
      end;
      debugPrint ("devmgr: NVMe driver started" & LF);

      if not waitReady (nvmePID) then
         nvmePID := 0;
      end if;

      --  Grant NVMe endpoint to FS server (slot 11)
      if filesystemPID /= 0 and nvmePID /= 0 then
         grantEndpoint (filesystemPID, nvmePID, 11, filesystemPID);
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2b: Spawn PS/2 keyboard + mouse driver
   -----------------------------------------------------------------------
   ps2PID := spawnFromBootStorage ("ps2.drv", 5);
   if ps2PID = reterr then
      ps2PID := 0;
   end if;
   if ps2PID /= 0 then
      setupPS2;
      assignCPU (ps2PID, "ps2.drv");
      mintCap (ps2PID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (ps2PID);
      debugPrint ("devmgr: PS/2 driver started" & LF);

      if not waitReady (ps2PID) then
         ps2PID := 0;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2c: Spawn xHCI only when PCI discovery found a controller.
   -----------------------------------------------------------------------
   if xhciDev.found then
      xhciPID := spawnFromBootStorage ("xhci.drv", 5);
      if xhciPID = reterr then
         xhciPID := 0;
      end if;
      if xhciPID /= 0 then
         setupXHCI;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2d: Spawn config store service
   -----------------------------------------------------------------------
   configPID := spawnFromBootStorage ("config.svc", 5);
   if configPID = reterr then
      configPID := 0;
   end if;
   if configPID /= 0 then
      mintCap (configPID, CAP_NOTIFICATION, DRIVER_CONFIG, 0,
               RIGHT_WRITE, 7);
      assignCPU (configPID, "config.svc");
      mintCap (configPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (configPID);
      debugPrint ("devmgr: config server started" & LF);

      if not waitReady (configPID) then
         configPID := 0;
      end if;

      --  Grant ourselves config endpoint at slot 2
      if configPID /= 0 then
         grantEndpoint (myPID, configPID, 2, myPID);
      end if;

      --  Grant config ACL for devmgr itself (wildcard, so we can SET)
      if configPID /= 0 then
         sendWildcardACLConfig (myPID);
      end if;

      --  Seed config from system.ccl in CPIO initrd
      if configPID /= 0 then
         declare
            OP_CONFIG_SET  : constant Unsigned_32 := 16#0601#;
            CONFIG_REPLY_OK : constant Unsigned_32 := 16#F000#;
            Plan : CCL.Configurations.Compilation_Result;
            use type CCL.Configurations.Profile_Kind;
            confIdx    : Natural;
            confAddr   : Unsigned_64;
            confSize   : Unsigned_64;
            --  Grant buffer for sending key=value to config.svc
            cfgRawAddr : Unsigned_64;
            cfgAligned : Unsigned_64;
            cfgBufAddr : System.Address;
            Config_Grant : CuBit.Memory_Grants.Grant_Reference;
            cfgOk      : Boolean;
            cfgMsg     : Message;
         begin
            confIdx := Cpio.findFile (cpioArchive, "system.ccl");
            if confIdx < cpioArchive.count then
               confAddr := INITRD_BASE +
                 Unsigned_64 (cpioArchive.files (confIdx).dataOff);
               confSize := Unsigned_64 (cpioArchive.files (confIdx).dataSize);

               if confSize not in 1 .. CCL.Declarations.MAX_SOURCE then
                  debugPrint ("devmgr: invalid system.ccl size; boot denied" & LF);
                  ret := syscall (SYSCALL_EXIT);
                  return;
               end if;
               declare
                  Source : String (1 .. Natural (confSize)) with Import,
                    Address => To_Address (Integer_Address (confAddr));
               begin
                  CCL.Configurations.Compile (Source, Plan);
               end;
               if not Plan.Success or else
                 Plan.Plan.Kind /= CCL.Configurations.System_Profile
               then
                  debugPrint ("devmgr: system.ccl rejected at" &
                              Plan.Position'Image & ": " &
                              CCL.Configurations.Diagnostic_Name (Plan.Diagnostic) &
                              " (expected system profile); boot denied" & LF);
                  ret := syscall (SYSCALL_EXIT);
                  return;
               end if;

               --  Allocate 3 pages for a 2-page aligned payload buffer.
               cfgRawAddr := syscall (SYSCALL_SBRK, 3 * 4096);
               if cfgRawAddr /= Unsigned_64'Last then
                  cfgAligned := (cfgRawAddr + 4095) and
                    not Unsigned_64 (4095);
                  cfgBufAddr := To_Address (Integer_Address (cfgAligned));

                  CuBit.Memory_Grants.Create_Via_Capability
                    (2, cfgBufAddr, 2, True, Config_Grant, cfgOk);

                  if cfgOk then
                     --  Only checked, owned entries reach the IPC adapter.
                     for Item of Plan.Plan.Settings (1 .. Plan.Plan.Setting_Count) loop
                        declare
                           Key_Length : constant Natural := Item.Key.Length;
                           Value_Length : constant Natural := Item.Value.Length;
                           Buffer : String (1 .. Key_Length + Value_Length)
                             with Import, Address => cfgBufAddr;
                        begin
                           Buffer (1 .. Key_Length) := Item.Key.Data (1 .. Key_Length);
                           Buffer (Key_Length + 1 .. Buffer'Last) :=
                             Item.Value.Data (1 .. Value_Length);
                           cfgMsg :=
                             (tag => (label => OP_CONFIG_SET, length => 4,
                                      flags => 0, reserved => 0),
                              authorityTag => 0,
                              words => [0 => Config_Grant.slot,
                                        1 => Config_Grant.generation,
                                        2 => Unsigned_64 (Key_Length),
                                        3 => Unsigned_64 (Value_Length)]);
                           cfgMsg.tag := capCall (2, cfgMsg);
                           if cfgMsg.tag.label /= CONFIG_REPLY_OK then
                              debugPrint ("devmgr: config seed failed; boot denied" & LF);
                              ret := syscall (SYSCALL_EXIT);
                              return;
                           end if;
                        end;
                     end loop;

                     debugPrint (
                       "devmgr: system.ccl seeded into config" & LF);

                     -- The CCL seed is authoritative; no implicit disk overlay.
                     -- Seed this boot's hardware time sample separately.
                     --  This namespace is writable only by Config admins.
                     declare
                        UTC, Mono : Unsigned_64;
                        Valid : Boolean;
                        Key : constant String := "clock.boot-sample";
                     begin
                        mintCap (myPID, CAP_IOPORT, 16#70#, 2,
                                 RIGHT_READ or RIGHT_WRITE, 27);
                        Boot_RTC.Read (UTC, Mono, Valid);
                        declare
                           Value : constant String :=
                             (if Valid then UTC'Image & Mono'Image else "unavailable");
                           Buffer : String (1 .. Key'Length + Value'Length)
                             with Import, Address => cfgBufAddr;
                        begin
                           Buffer := Key & Value;
                           cfgMsg :=
                             (tag => (OP_CONFIG_SET, 4, 0, 0), authorityTag => 0,
                              words => [Config_Grant.slot, Config_Grant.generation, Key'Length, Value'Length]);
                           cfgMsg.tag := capCall (2, cfgMsg);
                           if cfgMsg.tag.label /= CONFIG_REPLY_OK then
                              debugPrint ("devmgr: clock seed rejected; boot denied" & LF);
                              ret := syscall (SYSCALL_EXIT);
                              return;
                           end if;
                           debugPrint ("devmgr: RTC seed " & Value & LF);
                        end;
                     end;
                     CuBit.Memory_Grants.Revoke (Config_Grant, cfgOk);
                  else
                     debugPrint ("devmgr: config seed grant failed; boot denied" & LF);
                     ret := syscall (SYSCALL_EXIT);
                     return;
                  end if;
               else
                  debugPrint ("devmgr: config seed allocation failed; boot denied" & LF);
                  ret := syscall (SYSCALL_EXIT);
                  return;
               end if;
            else
               debugPrint (
                 "devmgr: system.ccl not found in CPIO; boot denied" & LF);
               ret := syscall (SYSCALL_EXIT);
               return;
            end if;
         end;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2d: Spawn VirtIO-GPU driver when QEMU/hardware exposes it
   -----------------------------------------------------------------------
   virtioGpuPID := spawnFromBootStorage ("virtio-gpu.drv", 5);
   if virtioGpuPID = reterr then
      virtioGpuPID := 0;
   end if;
   if virtioGpuPID /= 0 then
      setupVirtioGpu;
      --  CAP_NOTIFICATION for DRIVER_GPU registration (slot 8)
      mintCap (virtioGpuPID, CAP_NOTIFICATION, DRIVER_GPU, 0,
               RIGHT_WRITE, 8);
      assignCPU (virtioGpuPID, "virtio-gpu.drv");
      mintCap (virtioGpuPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (virtioGpuPID);
      debugPrint ("devmgr: virtio-gpu driver started" & LF);

      if not waitReady (virtioGpuPID) then
         virtioGpuPID := 0;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 3: Spawn networking services
   -----------------------------------------------------------------------

   --  Netstack service
   netstackPID := spawnFromBootStorage ("netstack.svc", 5);
   if netstackPID = reterr then
      netstackPID := 0;
   end if;

   --  Virtio-net driver
   virtioNetPID := spawnFromBootStorage ("virtio-net.drv", 5);
   if virtioNetPID = reterr then
      virtioNetPID := 0;
   end if;

   --  Grant cross-endpoints between netstack and virtio-net
   if netstackPID /= 0 and virtioNetPID /= 0 then
      setupVirtioNet;

      --  Netstack slot 10 -> virtio-net driver
      grantEndpoint (netstackPID, virtioNetPID, 10, netstackPID);

      --  Virtio-net slot 7 -> netstack service
      mintCap (virtioNetPID, CAP_ENDPOINT, netstackPID,
               CuBit.Network_Authority.Driver_Authority_Tag,
               RIGHT_READ or RIGHT_WRITE, 7);

      --  CAP_NOTIFICATION for DRIVER_NETSTACK registration (slot 8)
      mintCap (netstackPID, CAP_NOTIFICATION, DRIVER_NETSTACK, 0,
               RIGHT_WRITE, 8);

      assignCPU (netstackPID, "netstack.svc");
      mintCap (netstackPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (netstackPID);
      debugPrint ("devmgr: netstack started" & LF);

      if not waitReady (netstackPID) then
         netstackPID := 0;
      end if;

      if netTransportReady then
         assignCPU (virtioNetPID, "virtio-net.drv");
         mintCap (virtioNetPID, CAP_ENDPOINT, myPID, 0,
                  RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
         grantEndpoint (myPID, virtioNetPID, DEVMGR_NET_SLOT, myPID);
         resumeProc (virtioNetPID);
         declare
            use CuBit.Virtio_Net_Control;
            cfg : Message :=
              (if netModern then
                 (tag => (label => Operation'Enum_Rep (Configure_Modern),
                          length => 4, flags => 0, reserved => 0),
                  authorityTag => 0,
                  words => [0 => Pair (netCommonOff, netDeviceOff),
                            1 => Pair (netNotifyOff, netNotifyMult),
                            2 => Pair (netTableOffset, netModernBytes),
                            3 => Device_Vector])
               else
                 (tag => (label => Operation'Enum_Rep (Configure_MSIX),
                          length => 3, flags => 0, reserved => 0),
                  authorityTag => 0,
                  words => [0 => netIOBase, 1 => netTableOffset,
                            2 => Device_Vector, others => 0]));
            response : MessageTag;
            control : Unsigned_16;
         begin
            response := capCall (DEVMGR_NET_SLOT, cfg);
            netTransportReady := response.label = REPLY_OK;
            if netTransportReady then
               control := pciReadConfig16
                 (netDev.bus, netDev.slot, netDev.func, netMSIXCap + 2);
               pciWriteConfig16 (netDev.bus, netDev.slot, netDev.func,
                 netMSIXCap + 2, control and not Unsigned_16'(16#4000#));
               debugPrint ("devmgr: virtio-net using MSI-X" & LF);
            end if;
         end;
         debugPrint ("devmgr: virtio-net driver started" & LF);

         if not netTransportReady or else not waitReady (virtioNetPID) then
            virtioNetPID := 0;
         end if;
      else
         debugPrint ("devmgr: virtio-net interrupt setup unavailable" & LF);
         virtioNetPID := 0;
      end if;
   elsif netstackPID /= 0 then
      --  Netstack without virtio-net (no network device found)
      --  CAP_NOTIFICATION for DRIVER_NETSTACK registration (slot 8)
      mintCap (netstackPID, CAP_NOTIFICATION, DRIVER_NETSTACK, 0,
               RIGHT_WRITE, 8);
      assignCPU (netstackPID, "netstack.svc");
      mintCap (netstackPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (netstackPID);

      if not waitReady (netstackPID) then
         netstackPID := 0;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 3b: Spawn network manager service
   -----------------------------------------------------------------------
   netmgrPID := spawnFromBootStorage ("netmgr.svc", 5);
   if netmgrPID = reterr then
      netmgrPID := 0;
   end if;
   if netmgrPID /= 0 and netstackPID /= 0 then
      --  Slot 4: endpoint to netstack (config + raw UDP IPC)
      mintCap (netmgrPID, CAP_ENDPOINT, netstackPID,
               CuBit.Network_Authority.Manager_Authority_Tag,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 20: endpoint to config.svc (CAP_SLOT_CONFIG)
      if configPID /= 0 then
         grantEndpoint (netmgrPID, configPID, 20, netmgrPID);
      end if;

      --  Slot 8: CAP_NOTIFICATION for DRIVER_NETMGR registration
      mintCap (netmgrPID, CAP_NOTIFICATION, DRIVER_NETMGR, 0,
               RIGHT_WRITE, 8);

      --  Config ACL for reading net.* keys
      sendWildcardACLConfig (netmgrPID);

      assignCPU (netmgrPID, "netmgr.svc");
      mintCap (netmgrPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (netmgrPID);
      debugPrint ("devmgr: netmgr started" & LF);

      if not waitReady (netmgrPID) then
         netmgrPID := 0;
      end if;
   elsif netmgrPID /= 0 then
      --  netmgr without netstack - still spawn so it can read config
      if configPID /= 0 then
         grantEndpoint (netmgrPID, configPID, 20, netmgrPID);
      end if;
      mintCap (netmgrPID, CAP_NOTIFICATION, DRIVER_NETMGR, 0,
               RIGHT_WRITE, 8);
      sendWildcardACLConfig (netmgrPID);
      assignCPU (netmgrPID, "netmgr.svc");
      mintCap (netmgrPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (netmgrPID);
      if not waitReady (netmgrPID) then
         netmgrPID := 0;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 4: Spawn audio services (HDA driver + mixer)
   -----------------------------------------------------------------------

   if intelDev.found then
      declare
         Low : constant Unsigned_32 := pciReadConfig32
           (intelDev.bus, intelDev.slot, intelDev.func, 16#10#);
         High : constant Unsigned_32 := pciReadConfig32
           (intelDev.bus, intelDev.slot, intelDev.func, 16#14#);
         Command : constant Unsigned_16 := pciReadConfig16
           (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND);
         Plan : constant Intel_GPU_Resources.Mapping_Plan :=
           Intel_GPU_Resources.Plan_ADLN_Registers
             (Intel_GPU_Probe.Alder_Lake_N, Command, Low, High, 0, 16#20_0000#);
         Child : Unsigned_64;
         Request : Message := NULL_MESSAGE;
         Submitted : Boolean;
         use type Intel_GPU_Resources.Admission_Status;
         use type Intel_GPU_PCI_Power.Power_Status;
         Config : Intel_GPU_PCI_Power.Configuration;
         Power : Intel_GPU_PCI_Power.Power_Status;
         IRQ_State : Intel_GPU_PCI_Interrupts.Snapshot;
      begin
         for Byte in Config'Range loop
            Config (Byte) := pciReadConfig8
              (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
         end loop;
         Power := Intel_GPU_PCI_Power.Decode (Config);
         IRQ_State := Intel_GPU_PCI_Interrupts.Decode (Config);
         debugPrint ("devmgr: Intel PCI IRQ snapshot valid=" &
           Boolean'Image (IRQ_State.Valid) & " INTx-disabled=" &
           Boolean'Image (IRQ_State.INTx_Disabled) & LF);
         debugPrint ("devmgr: Intel MSI present/enabled=" &
           Boolean'Image (IRQ_State.MSI_Present) & "/" &
           Boolean'Image (IRQ_State.MSI_Enabled) & " MSI-X present/enabled/masked=" &
           Boolean'Image (IRQ_State.MSIX_Present) & "/" &
           Boolean'Image (IRQ_State.MSIX_Enabled) & "/" &
           Boolean'Image (IRQ_State.MSIX_Masked) & LF);
         debugPrint ("devmgr: Intel PCI power=" &
           Intel_GPU_PCI_Power.Power_Status'Image (Power) & LF);
         if Plan.Status = Intel_GPU_Resources.Admitted and then
           Power = Intel_GPU_PCI_Power.D0
         then
            Child := spawnFromBootStorage ("intel-gpu.drv", 4);
            if Child /= 0 and then Child /= reterr then
               Intel_Inspection_PID := Child;
               Grant_Intel_Firmware (Child);
               mintCap (Child, CAP_ENDPOINT, myPID, 16#4947#, RIGHT_READ or RIGHT_WRITE, 15);
               mintCap (Child, CAP_DEVICE_MEM, Plan.Physical_Base, Plan.Bytes, RIGHT_READ, 4);
               mintCap (myPID, CAP_ENDPOINT, Child, Intel_GPU_Boot.Broker_Tag,
                 RIGHT_READ or RIGHT_WRITE or RIGHT_GRANT, 31);
               -- Optional boot diagnostic, created suspended. Capture its
               -- incarnation before installing any authority; never look up
               -- the PID again to grant Desktop access after startup.
               GPU_Viewer_PID := spawnFromBootStorage ("gpu-viewer.app", 5);
               if GPU_Viewer_PID /= 0 and then GPU_Viewer_PID /= reterr then
                  declare
                     Issued : Unsigned_64;
                  begin
                     Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
                       myPID, CAP_ENDPOINT, GPU_Viewer_PID, 0, RIGHT_READ,
                       GP.Supervisor_Recipient_Slot);
                     if Issued /= reterr then
                        GPU_Viewer_Target := CG.Capture (GP.Supervisor_Recipient_Slot);
                        if CG.Valid (GPU_Viewer_Target) and then
                          CG.Process_ID (GPU_Viewer_Target) = GPU_Viewer_PID
                        then
                           Issued := CG.Install (GPU_Viewer_Target, CAP_ENDPOINT,
                             Child, GP.Viewer_Tag, RIGHT_READ or RIGHT_WRITE,
                             GP.Viewer_Driver_Slot);
                           if Issued = 0 then
                              Issued := CG.Install (GPU_Viewer_Target, CAP_ENDPOINT,
                                myPID, GP.Viewer_Tag, RIGHT_READ or RIGHT_WRITE,
                                GP.Viewer_Supervisor_Slot);
                           end if;
                           if Issued = 0 then
                              Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
                                Child, CAP_ENDPOINT, GPU_Viewer_PID, 0, RIGHT_READ,
                                GP.Driver_Recipient_Slot);
                              GPU_Viewer_Ready := Issued /= reterr;
                           end if;
                        end if;
                     end if;
                  end;
               end if;
               Request.tag := (Intel_GPU_Boot.Configure_Label, 4, 0, 0);
               Request.words (0) := Unsigned_64 (Low) or Shift_Left (Unsigned_64 (High), 32);
               Request.words (1) := Unsigned_64 (pciReadConfig32
                 (intelDev.bus, intelDev.slot, intelDev.func, 0)) or
                 Shift_Left (Unsigned_64 (pciReadConfig8
                   (intelDev.bus, intelDev.slot, intelDev.func, 8)), 32) or
                 Shift_Left (Unsigned_64'(3), 40);
               Request.words (2) := Unsigned_64 (Command) or 16#10000#;
               Request.words (3) := Intel_GPU_Boot.Protocol_Version or
                 Shift_Left (Unsigned_64 (Intel_GPU_PCI_Interrupts.Pack (IRQ_State)), 32) or
                 Shift_Left (Unsigned_64 (pciReadConfig16
                   (intelDev.bus, intelDev.slot, intelDev.func, 16#50#)), 16);
               Intel_Claim_Base := Plan.Physical_Base;
               Intel_Claim_Identity := Unsigned_32 (Request.words (1) and 16#FFFF_FFFF#);
               Intel_Config_Frozen := True;
               resumeProc (Child);
               Submitted := capSubmit (31, Request, NO_COMPLETION_TOKEN);
               GPU_Viewer_Ready := GPU_Viewer_Ready and then Submitted;
               if not Submitted then debugPrint ("devmgr: Intel bootstrap submission failed" & LF); end if;
               debugPrint ("devmgr: Intel inspection launched; firmware display retained" & LF);
            end if;
         end if;
      end;
   end if;

   --  HDA driver
   hdaPID := spawnFromBootStorage ("hda.drv", 5);
   if hdaPID = reterr then
      hdaPID := 0;
   end if;

   --  Mixer service
   mixerPID := spawnFromBootStorage ("mixer.svc", 5);
   if mixerPID = reterr then
      mixerPID := 0;
   end if;

   if hdaPID /= 0 and mixerPID /= 0 then
      setupHDA;

      --  HDA slot 7: endpoint to mixer
      grantEndpoint (hdaPID, mixerPID, 7, hdaPID);

      --  Mixer slot 4: endpoint to HDA driver
      grantEndpoint (mixerPID, hdaPID, 4, mixerPID);

      --  CAP_NOTIFICATION for DRIVER_HDA registration (slot 8)
      mintCap (hdaPID, CAP_NOTIFICATION, DRIVER_HDA, 0,
               RIGHT_WRITE, 8);

      --  CAP_NOTIFICATION for DRIVER_MIXER registration (slot 8)
      mintCap (mixerPID, CAP_NOTIFICATION, DRIVER_MIXER, 0,
               RIGHT_WRITE, 8);

      assignCPU (hdaPID, "hda.drv");
      mintCap (hdaPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (hdaPID);
      debugPrint ("devmgr: HDA driver started" & LF);

      if not waitReady (hdaPID) then
         hdaPID := 0;
      end if;

      assignCPU (mixerPID, "mixer.svc");
      mintCap (mixerPID, CAP_SCHEDULING, MIXER_REALTIME_BUDGET_US,
               MIXER_REALTIME_PERIOD_US, RIGHT_READ, CAP_SLOT_SCHEDULING);
      mintCap (mixerPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (mixerPID);
      debugPrint ("devmgr: mixer started" & LF);

      if not waitReady (mixerPID) then
         mixerPID := 0;
      end if;
   elsif mixerPID /= 0 then
      --  Mixer without HDA (no audio hardware found)
      --  CAP_NOTIFICATION for DRIVER_MIXER registration (slot 8)
      mintCap (mixerPID, CAP_NOTIFICATION, DRIVER_MIXER, 0,
               RIGHT_WRITE, 8);
      assignCPU (mixerPID, "mixer.svc");
      mintCap (mixerPID, CAP_SCHEDULING, MIXER_REALTIME_BUDGET_US,
               MIXER_REALTIME_PERIOD_US, RIGHT_READ, CAP_SLOT_SCHEDULING);
      mintCap (mixerPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (mixerPID);

      if not waitReady (mixerPID) then
         mixerPID := 0;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 5: Spawn procmgr
   -- Disk drivers already waited in Phase 2, no duplicate wait needed.
   -----------------------------------------------------------------------
   procmgrPID := spawnFromBootStorage ("procmgr.svc", 5);
   if procmgrPID = reterr then
      procmgrPID := 0;
   end if;
   if procmgrPID /= 0 then
      --  Process management and capability-space administration are separate
      --  authorities. The latter is the explicit policy root used to install
      --  manifest-admitted capabilities into newly spawned processes.
      mintCap (procmgrPID, CAP_PROCESS, 0, 0,
               RIGHT_READ or RIGHT_EXECUTE or RIGHT_GRANT, 4);
      mintCap (procmgrPID, CAP_CSPACE, 0, 0, RIGHT_GRANT, 10);

      --  Grant FS endpoint at slot 1
      if filesystemPID /= 0 then
         grantEndpoint (procmgrPID, filesystemPID, 1, procmgrPID);
      end if;

      --  Grant config endpoint at slot 2
      if configPID /= 0 then
         grantEndpoint (procmgrPID, configPID, 2, procmgrPID);
      end if;

      --  Only this explicitly held endpoint may install network scopes.
      if netstackPID /= 0 then
         mintCap (procmgrPID, CAP_ENDPOINT, netstackPID,
                  CuBit.Network_Authority.Policy_Authority_Tag,
                  RIGHT_READ or RIGHT_WRITE,
                  CuBit.Network_Authority.Policy_Capability_Slot);
      end if;

      --  CAP_NOTIFICATION for DRIVER_PROCMGR registration (slot 7)
      mintCap (procmgrPID, CAP_NOTIFICATION, DRIVER_PROCMGR, 0,
               RIGHT_WRITE, 7);

      --  CAP_NOTIFICATION for DRIVER_KEYBOARD (slot 8, for child apps)
      --  procmgr mints derived caps for spawned children
      mintCap (procmgrPID, CAP_NOTIFICATION, DRIVER_KEYBOARD, 0,
               RIGHT_WRITE or RIGHT_GRANT, 8);

      --  CAP_NOTIFICATION for DRIVER_MOUSE (slot 9, for child apps)
      mintCap (procmgrPID, CAP_NOTIFICATION, DRIVER_MOUSE, 0,
               RIGHT_WRITE or RIGHT_GRANT, 9);

      assignCPU (procmgrPID, "procmgr.svc");
      -- Trusted launcher policy endpoint, not inherited by applications.
      mintCap (procmgrPID, CAP_ENDPOINT, myPID,
               Intel_GPU_Broker_Request.Authority_Tag,
               RIGHT_READ or RIGHT_WRITE,
               Intel_GPU_Broker_Request.Launcher_Endpoint_Slot);
      sendWildcardACL (procmgrPID);
      sendWildcardACLConfig (procmgrPID);
      mintCap (procmgrPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (procmgrPID);
      debugPrint ("devmgr: procmgr started" & LF);

      if not waitReady (procmgrPID) then
         procmgrPID := 0;
      end if;
   end if;
   -- Do not expose the viewer's request to waitReady's startup-only handler.
   -- Its first call may now queue until the normal receive loop below.
   if GPU_Viewer_Ready then
      resumeProc (GPU_Viewer_PID);
   end if;
   debugPrint ("devmgr: startup complete, entering service loop" & LF);

   loop
      -- One completion, one admission step and one request per iteration:
      -- incoming device traffic cannot starve pending GPU launch completion.
      Launch_Now := syscall (SYSCALL_GETTIME);
      if Poll_Completion (Launch_Receipt'Address) = 1 then
         GPU_Launch.Complete
           (GPU_Launches, Launch_Receipt, Launch_Now, Receipt_Consumed);
         if not Receipt_Consumed then
            -- This is currently devmgr's only completion-token owner;
            -- hardware bootstrap submits with NO_COMPLETION_TOKEN.
            debugPrint ("devmgr: unexpected async completion" & LF);
         end if;
      end if;
      GPU_Launch.Step (GPU_Launches, Launch_Now);
      Intel_Allocation_Growth.Step (Intel_Allocations);
      Poll_Service_Request (from, msg, Request_Found);
      if not Request_Found then
         if not GPU_Launch.Runnable (GPU_Launches) and then
           not Intel_Allocation_Growth.Pending (Intel_Allocations) then
            Launch_Activity := Wait_For_Activity_Until
              (GPU_Launch.Next_Deadline (GPU_Launches));
            if Launch_Activity = Unavailable then
               -- Exceptional kernel failure, not periodic broker polling.
               ret := syscall (SYSCALL_SLEEP, 1);
            end if;
         end if;
      elsif msg.tag.label = Intel_GPU_Broker_Request.Label then
         declare
            ID : GPU_Launch.Ticket;
         begin
            Launch_Now := syscall (SYSCALL_GETTIME);
            GPU_Launch.Begin_Request
              (GPU_Launches, procmgrPID, from, msg.authorityTag, msg,
               Launch_Now,
               (if Launch_Now > Unsigned_64'Last - 5_000 then Launch_Now
                else Launch_Now + 5_000), ID);
            if ID = 0 then
               ret := reply (from,
                 (tag => (Intel_GPU_Broker_Request.Label, 4, 0, 0),
                  authorityTag => 0,
                  words => [Intel_GPU_Broker_Request.Version, 1,
                            msg.words (3), 0]));
            end if;
         end;
      elsif msg.tag.label = GP.Desktop_Binding_Label then
         declare
            Code : Unsigned_64 := 1;
            Desktop_PID : constant Unsigned_64 :=
              getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
         begin
            if GPU_Viewer_Ready and then from = GPU_Viewer_PID and then
              msg.authorityTag = GP.Viewer_Tag and then
              msg.tag = (GP.Desktop_Binding_Label, 4, 0, 0) and then
              msg.words = [1, 0, 0, 0]
            then
               if GPU_Viewer_Desktop_Bound then Code := 0;
               elsif Desktop_PID = 0 or else Desktop_PID = reterr then Code := 2;
               elsif CG.Install (GPU_Viewer_Target, CAP_ENDPOINT, Desktop_PID, 0,
                 RIGHT_READ or RIGHT_WRITE, CAP_SLOT_DESKTOP) = 0
               then
                  GPU_Viewer_Desktop_Bound := True;
                  Code := 0;
               end if;
            end if;
            ret := reply (from, (tag => (GP.Desktop_Binding_Label, 4, 0, 0),
              authorityTag => 0, words => [Code, 1, 0, 0]));
         end;
      elsif msg.tag = (16#0228#, 0, 0, 0) and then
        from = xhciPID and then xhciPID /= 0 and then
        msg.authorityTag = xhciPID and then msg.words = [0, 0, 0, 0]
      then
         declare
            Collector : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE);
            Issued : Unsigned_64 := reterr;
         begin
            if Collector /= 0 then
               Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, xhciPID, CAP_ENDPOINT,
                 Collector, CuBit.Log_Protocol.Publisher_Tag (15, 1),
                 RIGHT_READ or RIGHT_WRITE, CuBit.Log_Protocol.Publisher_Slot);
            end if;
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Issued /= reterr then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0, words => [others => 0])));
         end;
      elsif msg.tag = (16#0229#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Collector : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE);
            Issued : Unsigned_64 := reterr;
         begin
            if Collector /= 0 then
               Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, from, CAP_ENDPOINT,
                 Collector, CuBit.Log_Protocol.Publisher_Tag (15, 2),
                 RIGHT_READ or RIGHT_WRITE, CuBit.Log_Protocol.Publisher_Slot);
            end if;
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Issued /= reterr then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0, words => [others => 0])));
         end;
      elsif msg.tag = (16#022A#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         --  One-page write authority for the GT forcewake request register.
         --  No PCI ports, GGTT, aperture, DMA or display ownership is granted.
         declare
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Issued : Unsigned_64 := reterr;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            if Intel_Config_Frozen and then intelDev.found and then
              Intel_Claim_Base /= 0 and then not Intel_Forcewake_Granted
            then
               for Byte in Config'Range loop
                  Config (Byte) := pciReadConfig8
                    (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
               end loop;
               Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                 (Intel_GPU_Probe.Alder_Lake_N,
                  pciReadConfig16 (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#10#),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#14#),
                  16#A000#, 4096);
               if Intel_GPU_PCI_Power.Decode (Config) = Intel_GPU_PCI_Power.D0 and then
                 pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                   Intel_Claim_Identity and then
                 Plan.Status = Intel_GPU_Resources.Admitted and then
                 Plan.Physical_Base >= Intel_Claim_Base and then
                 Plan.Physical_Base - Intel_Claim_Base = 16#A000#
               then
                  Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, from,
                    CAP_DEVICE_MEM, Plan.Physical_Base, 4096,
                    RIGHT_READ or RIGHT_WRITE, 5);
                  if Issued /= reterr then
                     Intel_Forcewake_Granted := True;
                     debugPrint ("devmgr: Intel forcewake page granted; PCI config frozen" & LF);
                  end if;
               end if;
            end if;
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Issued /= reterr then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (16#022F#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Allowed, Valid : Boolean := False;
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            if Intel_Config_Frozen and then intelDev.found and then
              Intel_Claim_Identity = 16#46D2_8086# and then
              Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = 0
            then
               for Byte in Config'Range loop
                  Config (Byte) := pciReadConfig8
                    (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
               end loop;
               Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                 (Intel_GPU_Probe.Alder_Lake_N,
                  pciReadConfig16 (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#10#),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#14#),
                  0, 4096);
               Valid := Intel_GPU_PCI_Power.Decode (Config) = Intel_GPU_PCI_Power.D0 and then
                 pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                   Intel_Claim_Identity and then Plan.Status = Intel_GPU_Resources.Admitted and then
                 Plan.Physical_Base = Intel_Claim_Base;
            end if;
            Intel_GPU_Display_Claim.Take
              (Intel_Display_Ownership, Intel_Inspection_PID, from,
               Badge_Valid => True, Device_Valid => Valid, Allowed => Allowed);
            -- Consume before replying, including lost replies. This designates
            -- the sole power-request manager; it grants NO writable MMIO and
            -- establishes NO hardware reference by itself. Any future power
            -- page grant must require this same retained owner.
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Allowed then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (Intel_GPU_PCI_Interrupts.Disable_Request_Label, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Status : Intel_IRQ_Disable.Result;
            use type Intel_IRQ_Disable.Result;
         begin
            Intel_IRQ_Active := Intel_Config_Frozen and then intelDev.found and then
              Intel_Reset_Authorized and then Intel_Forcewake_Granted and then
              Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = from;
            Intel_IRQ_Disable.Execute (Intel_IRQ_Active, Status);
            Intel_IRQ_Active := False;
            Intel_IRQ_Plan_Ready := False;
            -- Success is PCI control readback, NOT pending-vector drain.
            -- Repeated requests cannot replay a consumed executor after loss.
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Status = Intel_IRQ_Disable.Complete then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (16#022E#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Allowed : Boolean := False;
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            if not Intel_Reset_Authorized and then Intel_Config_Frozen and then
              intelDev.found and then Intel_Forcewake_Granted and then
              Intel_Claim_Identity = 16#46D2_8086# and then
              (for all Granted of Intel_Reset_Granted => Granted)
            then
               for Byte in Config'Range loop
                  Config (Byte) := pciReadConfig8
                    (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
               end loop;
               Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                 (Intel_GPU_Probe.Alder_Lake_N,
                  pciReadConfig16 (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#10#),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#14#),
                  0, 4096);
               Allowed := Intel_GPU_PCI_Power.Decode (Config) = Intel_GPU_PCI_Power.D0 and then
                 pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                   Intel_Claim_Identity and then Plan.Status = Intel_GPU_Resources.Admitted and then
                 Plan.Physical_Base = Intel_Claim_Base;
               if Allowed then Intel_Reset_Authorized := True; end if;
            end if;
            -- Consume permission before replying; no repeat after reply loss.
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Allowed then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif ((msg.tag = (16#022D#, 1, 0, 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_Reset_Pages.Page_Index'Last)) or else
             (msg.tag = (Intel_GPU_Display_Pages.Request_Label, 1, 0, 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_Display_Pages.Page_Index'Last)) or else
             (msg.tag = (Intel_GPU_PHY_Pages.Request_Label, 1, 0, 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_PHY_Pages.Page_Index'Last))) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then
        msg.words (1 .. 3) = [0, 0, 0]
      then
         declare
            Display_Page : constant Boolean := msg.tag.label = Intel_GPU_Display_Pages.Request_Label;
            PHY_Page : constant Boolean := msg.tag.label = Intel_GPU_PHY_Pages.Request_Label;
            Index : constant Intel_GPU_Reset_Pages.Page_Index := Natural (msg.words (0));
            Offset : constant Unsigned_64 :=
              (if Display_Page then Intel_GPU_Display_Pages.Offset (Index)
               elsif PHY_Page then Intel_GPU_PHY_Pages.Offset (Index)
               else Intel_GPU_Reset_Pages.Offset (Index));
            Target_Slot : constant Unsigned_64 :=
              (if Display_Page then Intel_GPU_Display_Pages.Slot (Index)
               elsif PHY_Page then Intel_GPU_PHY_Pages.Slot (Index)
               else Intel_GPU_Reset_Pages.Slot (Index));
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Issued : Unsigned_64 := reterr;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            -- Never caller-selected addresses, lengths, slots or PCI identity.
            -- Partial grants persist under the static claim; no retry/rebind.
            if Intel_Config_Frozen and then intelDev.found and then
              Intel_Forcewake_Granted and then Intel_Claim_Base /= 0 and then
              (if Display_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = from and then
                 not Intel_Display_Granted (Index)
               elsif PHY_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = from and then
                 not Intel_PHY_Granted (Index)
               else not Intel_Reset_Granted (Index))
            then
               for Byte in Config'Range loop
                  Config (Byte) := pciReadConfig8
                    (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
               end loop;
               Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                 (Intel_GPU_Probe.Alder_Lake_N,
                  pciReadConfig16 (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#10#),
                  pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#14#),
                  Offset, 4096);
               if Intel_GPU_PCI_Power.Decode (Config) = Intel_GPU_PCI_Power.D0 and then
                 Intel_Claim_Identity = 16#46D2_8086# and then
                 pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                   Intel_Claim_Identity and then Plan.Status = Intel_GPU_Resources.Admitted and then
                 Plan.Physical_Base >= Intel_Claim_Base and then
                 Plan.Physical_Base - Intel_Claim_Base = Offset
               then
                  Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, from,
                    CAP_DEVICE_MEM, Plan.Physical_Base, 4096,
                    RIGHT_READ or RIGHT_WRITE, Target_Slot);
                  if Issued /= reterr then
                     if Display_Page then Intel_Display_Granted (Index) := True;
                     elsif PHY_Page then Intel_PHY_Granted (Index) := True;
                     else Intel_Reset_Granted (Index) := True;
                     end if;
                  end if;
               end if;
            end if;
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Issued /= reterr then 16#F000# else 16#F001#),
                length => 0, flags => 0, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (Intel_GPU_GGTT_Access.Write_Request_Label, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Config : Intel_GPU_PCI_Power.Configuration := [others => 255];
            Plan : Intel_GPU_GGTT_Access.Grant_Plan;
            Raw, Issued : Unsigned_64 := reterr;
            Snapshot_OK : Boolean := True;
            use type Intel_IRQ_Disable.Phase;
         begin
            if not Intel_GGTT_Write_Attempted then
               -- Consume before inspection/mint; lost replies cannot replay.
               Intel_GGTT_Write_Attempted := True;
               if Intel_Config_Frozen and then intelDev.found and then
                 Intel_Claim_Identity = 16#46D2_8086# and then
                 Intel_Reset_Authorized and then Intel_GGTT_Granted and then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = from and then
                 Intel_IRQ_Disable.State = Intel_IRQ_Disable.PCI_Disabled
               then
                  -- A single fresh snapshot supplies identity, BAR, table size,
                  -- power and interrupt state. No client-selected range/slot.
                  for Index in 0 .. 63 loop
                     if portOutp32 (PCI_CONFIG_ADDR, Intel_Config_Address (Index * 4)) /= 0 then
                        Snapshot_OK := False; exit;
                     end if;
                     Raw := portInp32 (PCI_CONFIG_DATA);
                     if Raw = reterr then Snapshot_OK := False; exit; end if;
                     for Byte in 0 .. 3 loop
                        Config (Index * 4 + Byte) := Unsigned_8
                          (Shift_Right (Raw, Byte * 8) and 255);
                     end loop;
                  end loop;
                  if Snapshot_OK then
                     Plan := Intel_GPU_GGTT_Access.Plan_Write
                       (Config, Intel_GGTT_BAR, Intel_GGTT_Bytes, True, True);
                     if Plan.Valid then
                        Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, from,
                          CAP_DEVICE_MEM, Plan.Physical, Plan.Bytes,
                          RIGHT_READ or RIGHT_WRITE, Intel_GPU_GGTT_Access.Write_Slot);
                     end if;
                  end if;
               end if;
            end if;
            if Issued /= reterr then
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F000#, 2, 0, 0), authorityTag => 0,
                  words => [Plan.Physical, Plan.Bytes, 0, 0])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0,
                  words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (16#022B#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         declare
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Physical : Unsigned_64 := 0;
            Table_Bytes : Unsigned_64 := 0;
            Issued : Unsigned_64 := reterr;
            GGC : Unsigned_16;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            if not Intel_GGTT_Granted then
               if Intel_Config_Frozen and then intelDev.found then
                  for Byte in Config'Range loop
                     Config (Byte) := pciReadConfig8
                       (intelDev.bus, intelDev.slot, intelDev.func, Unsigned_8 (Byte));
                  end loop;
                  GGC := pciReadConfig16
                    (intelDev.bus, intelDev.slot, intelDev.func, 16#50#);
                  Plan := Intel_GPU_Resources.Plan_ADLN_Registers
                    (Intel_GPU_Probe.Alder_Lake_N,
                     pciReadConfig16 (intelDev.bus, intelDev.slot, intelDev.func, PCI_COMMAND),
                     pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#10#),
                     pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 16#14#),
                     0, 4096);
                  if Intel_GPU_PCI_Power.Decode (Config) = Intel_GPU_PCI_Power.D0 and then
                    Plan.Status = Intel_GPU_Resources.Admitted and then
                    Plan.Physical_Base = Intel_Claim_Base and then
                    pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                      Intel_Claim_Identity and then GGC /= Unsigned_16'Last and then
                    (Shift_Right (GGC, 6) and 3) /= 0
                  then
                     Physical := Plan.Physical_Base + 16#80_0000#;
                     Table_Bytes := 2 ** Natural (Shift_Right (GGC, 6) and 3) * 1024 * 1024;
                  end if;
               end if;
               if Physical /= 0 then
                  Issued := syscall (SYSCALL_POLICY_MINT_CAPABILITY, from,
                    CAP_DEVICE_MEM, Physical, Table_Bytes, RIGHT_READ, 7);
                  Intel_GGTT_Granted := Issued /= reterr;
                  if Intel_GGTT_Granted then
                     Intel_GGTT_BAR := Plan.Physical_Base;
                     Intel_GGTT_Bytes := Table_Bytes;
                  end if;
               end if;
            end if;
            if Issued /= reterr then
               debugPrint ("devmgr: Intel GGTT inspection page granted read-only" & LF);
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F000#, 2, 0, 0), authorityTag => 0,
                  words => [Physical, Table_Bytes, 0, 0])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0,
                  words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (16#022C#, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         -- Fixed supervisor-owned allocation policy, not caller-controlled
         -- addresses or sizes. Retain even if the driver subsequently dies.
         if not Intel_Buffer_Attempted then
            Intel_Buffer_Attempted := True;
            if Intel_GGTT_Granted then
               Intel_Buffer_Physical := syscall
                 (SYSCALL_ALLOC_DMA, from, 8, 16#6100_0000#, 1, 2 ** 32);
               debugPrint ("devmgr: Intel retained buffer physical" &
                 Unsigned_64'Image (Intel_Buffer_Physical) & LF);
               if Intel_Buffer_Physical = reterr or else
                 Intel_Buffer_Physical = 0 or else
                 Intel_Buffer_Physical mod 4096 /= 0 or else
                 Intel_Buffer_Physical > 2 ** 32 - 1024 * 1024
               then Intel_Buffer_Physical := 0; end if;
            end if;
         end if;
         if Intel_Buffer_Physical /= 0 then
            ret := Unsigned_64 (reply (from,
              (tag => (16#F000#, 3, 0, 0), authorityTag => 0,
               words => [Intel_Buffer_Physical, 16#6100_0000#, 1024 * 1024, 0])));
         else
            ret := Unsigned_64 (reply (from,
              (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
         end if;
      elsif Intel_GPU_Buffer_Backing.Valid_Allocation_Request
        (msg.tag.label, msg.tag.length, msg.tag.flags, msg.tag.reserved,
         msg.words (0), msg.words (1), msg.words (2), msg.words (3)) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947#
      then
         declare
            Accepted : Boolean;
         begin
            Intel_Allocation_Growth.Begin_Request
              (Intel_Allocations, Intel_GPU_Buffer_Backing.Slot (msg.words (0)),
               Intel_GPU_Buffer_Backing.Page_Count (msg.words (1)),
               Unsigned_32 (msg.words (2)), Accepted);
            if not Accepted then
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag.label = Intel_GPU_Buffer_Backing.Retire_Request_Label then
         declare
            Granted : Boolean := False;
         begin
            if Intel_GPU_Buffer_Backing.Retirement_Authorized
              (msg.tag.label, msg.tag.length, msg.tag.flags, msg.tag.reserved,
               Intel_GPU_Buffer_Backing.Budget_Words (msg.words), Unsigned_64 (from),
               msg.authorityTag, Intel_Inspection_PID, Intel_Buffer_Arena_Granted)
            then
               -- Only the trusted driver can certify hardware/CPU retirement.
               -- This releases an exact-generation arena slice, never DMA RAM.
               Buffer_Allocations.Retire_Buffer
                 (Intel_Buffer_Pool, Intel_Inspection_PID,
                  Intel_GPU_Buffer_Backing.Slot (msg.words (0)),
                  Unsigned_32 (msg.words (1)), True, Granted);
            end if;
            if Granted then
               ret := Unsigned_64 (reply (from,
                 (tag => (Intel_GPU_Buffer_Backing.Retire_Request_Label, 4, 0, 0),
                  authorityTag => 0, words => [0,
                    Intel_GPU_Buffer_Backing.Allocation_Key
                      (Intel_GPU_Buffer_Backing.Slot (msg.words (0)), Unsigned_32 (msg.words (1))),
                    Intel_Inspection_PID, 0])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (Intel_GPU_Buffer_Backing.Budget_Request_Label, 4, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [Intel_GPU_Buffer_Backing.Budget_Version, 0, 0, 0]
      then
         declare
            Usage : constant Buffer_Allocations.Budget :=
              Buffer_Allocations.Memory_Budget (Intel_Buffer_Pool);
            Data : constant Intel_GPU_Buffer_Backing.Budget_Words :=
              Intel_GPU_Buffer_Backing.Budget_Reply
                (Usage.Known, Usage.Capacity, Usage.Retained,
                 Intel_Allocation_Growth.Record_Budget
                   (Intel_Allocations, Buffer_Allocations.Record_Capacity (Intel_Buffer_Pool),
                    Usage.Unassigned_Slots));
         begin
            if Data (0) = Intel_GPU_Buffer_Backing.Budget_Version then
               ret := Unsigned_64 (reply (from,
                 (tag => (Intel_GPU_Buffer_Backing.Budget_Request_Label, 4, 0, 0),
                  authorityTag => 0, words => [Data (0), Data (1), Data (2), Data (3)])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif Intel_GPU_Buffer_Backing.Extent_Request_Authorized
        (msg.tag.label, msg.tag.length, msg.tag.flags, msg.tag.reserved,
         Intel_GPU_Buffer_Backing.Budget_Words (msg.words), Unsigned_64 (from),
         msg.authorityTag, Intel_Inspection_PID,
         Intel_GPU_Extent_Directory.Byte_Count (Buffer_Allocations.Snapshot (Intel_Buffer_Pool)),
         Intel_Buffer_Arena_Granted)
      then
         declare
            Map : Intel_GPU_Extent_Directory.Borrowed_View;
            Part : Intel_GPU_Physical_Extents.Span;
            Granted : Boolean;
            Offset : constant Unsigned_64 := msg.words (0) * Intel_GPU_Physical_Extents.Block_Bytes;
         begin
            Map := Buffer_Allocations.Snapshot (Intel_Buffer_Pool);
            Granted := Intel_GPU_Extent_Directory.Valid (Map);
            Part := Intel_GPU_Extent_Directory.Resolve
              (Map, Offset, Intel_GPU_Physical_Extents.Block_Bytes);
            if Granted and then Part.Valid then
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F003#, 4, 0, 0), authorityTag => 0,
                  words => [msg.words (0), Part.Address,
                    Intel_GPU_Buffer_Backing.CPU_Base + Offset, Intel_Inspection_PID])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (Intel_GPU_ADS_Backing.Request_Label, 0, 0, 0) and then
        Intel_Inspection_PID /= 0 and then from = Intel_Inspection_PID and then
        msg.authorityTag = 16#4947# and then msg.words = [0, 0, 0, 0]
      then
         if not Intel_ADS_Attempted then
            Intel_ADS_Attempted := True;
            if Intel_Reset_Authorized and then Intel_Config_Frozen and then
              Intel_GGTT_Granted
            then
               Intel_ADS_Physical := syscall
                 (SYSCALL_ALLOC_DMA, from, Intel_GPU_ADS_Backing.Allocation_Order,
                  Intel_GPU_ADS_Backing.CPU_Address, 1, 2 ** 32);
               if not Intel_GPU_ADS_Backing.Valid_Physical (Intel_ADS_Physical) then
                  Intel_ADS_Physical := 0;
               end if;
            end if;
         end if;
         if Intel_ADS_Physical /= 0 then
            ret := Unsigned_64 (reply (from,
              (tag => (16#F000#, 3, 0, 0), authorityTag => 0,
               words => [Intel_ADS_Physical, Intel_GPU_ADS_Backing.CPU_Address,
                         Intel_GPU_ADS_Backing.Capacity, 0])));
         else
            ret := Unsigned_64 (reply (from,
              (tag => (16#F001#, 0, 0, 0), authorityTag => 0, words => [others => 0])));
         end if;
      elsif msg.tag.label = CuBit.Devices.OP_INVENTORY_COUNT then
         ret := Unsigned_64
           (reply
              (from,
               (tag => (label  => CuBit.Devices.REPLY_OK,
                        length => 2, flags => 0, reserved => 0),
                authorityTag => 0,
                words =>
                  [0 => Unsigned_64 (inventoryCount),
                   1 => (if inventoryOverflow then 1 else 0),
                   others => 0])));
      elsif msg.tag.label = CuBit.Devices.OP_INVENTORY_ITEM and then
            msg.tag.length >= 1 and then msg.words (0) >= 1 and then
            msg.words (0) <= Unsigned_64 (inventoryCount)
      then
         declare
            item : Inventory_Device renames
              inventory (Natural (msg.words (0)));
            driverPID : Unsigned_64 := 0;
            state : CuBit.Devices.Driver_State := CuBit.Devices.Unclaimed;
            locationWord : Unsigned_64;
            identityWord : Unsigned_64;
         begin
            case item.kind is
               when CuBit.Devices.Storage_Controller =>
                  if Same_Location (item.location, nvmeDev) then
                     driverPID := nvmePID;
                  elsif Same_Location (item.location, ataDev) then
                     driverPID := ataPID;
                  end if;
               when CuBit.Devices.Network_Controller =>
                  if Same_Location (item.location, netDev) then
                     driverPID := virtioNetPID;
                  end if;
               when CuBit.Devices.Display_Controller =>
                  if Same_Location (item.location, gpuDev) then
                     driverPID := virtioGpuPID;
                  end if;
               when CuBit.Devices.Audio_Controller =>
                  if Same_Location (item.location, hdaDev) then
                     driverPID := hdaPID;
                  end if;
               when CuBit.Devices.USB_Controller =>
                  if Same_Location (item.location, xhciDev) then
                     driverPID := xhciPID;
                  end if;
               when CuBit.Devices.Other_Device =>
                  null;
            end case;
            if driverPID /= 0 then
               state := CuBit.Devices.Driver_Active;
            end if;
            locationWord := Unsigned_64 (item.location.bus) or
              Shift_Left (Unsigned_64 (item.location.slot), 8) or
              Shift_Left (Unsigned_64 (item.location.func), 16) or
              Shift_Left
                (Unsigned_64 (CuBit.Devices.Device_Kind'Enum_Rep (item.kind)),
                 24) or
              Shift_Left
                (Unsigned_64 (CuBit.Devices.Driver_State'Enum_Rep (state)),
                 32);
            identityWord := Unsigned_64 (item.vendorID) or
              Shift_Left (Unsigned_64 (item.deviceID), 16) or
              Shift_Left (Unsigned_64 (item.classCode), 32) or
              Shift_Left (Unsigned_64 (item.progIf), 48);
            ret := Unsigned_64
              (reply
                 (from,
                  (tag => (label => CuBit.Devices.REPLY_OK,
                           length => 4, flags => 0, reserved => 0),
                   authorityTag => 0,
                   words =>
                     [0 => locationWord, 1 => identityWord,
                      2 => driverPID, 3 => 1])));
         end;
      elsif msg.tag.label = CuBit.Devices.OP_PUBLISH_XHCI_STATS and then
            from = xhciPID and then msg.tag.length >= 3
      then
         xhciDiagnostics.valid := True;
         xhciDiagnostics.decodedReports :=
           msg.words (0) and 16#FFFF_FFFF#;
         xhciDiagnostics.motionReports := Shift_Right (msg.words (0), 32);
         xhciDiagnostics.buttonTransitions :=
           Unsigned_32 (msg.words (1) and 16#FFFF_FFFF#);
         xhciDiagnostics.completionErrors :=
           Unsigned_32 (Shift_Right (msg.words (1), 32));
         xhciDiagnostics.lastReport :=
           Unsigned_32 (msg.words (2) and 16#FFFF_FFFF#);
         xhciDiagnostics.lastLength :=
           Unsigned_8 (Shift_Right (msg.words (2), 32) and 16#FF#);
         xhciDiagnostics.lastCompletion :=
           Unsigned_8 (Shift_Right (msg.words (2), 40) and 16#FF#);
         case Shift_Right (msg.words (2), 48) and 16#FF# is
            when 1 =>
               xhciDiagnostics.interruptMode :=
                 CuBit.Devices.Interrupt_MSI;
            when 2 =>
               xhciDiagnostics.interruptMode :=
                 CuBit.Devices.Interrupt_MSIX;
            when others =>
               xhciDiagnostics.interruptMode :=
                 CuBit.Devices.Interrupt_Polling;
         end case;
      elsif msg.tag.label = CuBit.Devices.OP_XHCI_DIAGNOSTICS then
         ret := Unsigned_64
           (reply
              (from,
               (tag =>
                  (label => CuBit.Devices.REPLY_OK,
                   length => 4, flags => 0, reserved => 0),
                authorityTag => 0,
                words =>
                  [0 => xhciDiagnostics.decodedReports,
                   1 => xhciDiagnostics.motionReports,
                   2 => Unsigned_64 (xhciDiagnostics.buttonTransitions) or
                     Shift_Left
                       (Unsigned_64 (xhciDiagnostics.completionErrors), 32),
                   3 => Unsigned_64 (xhciDiagnostics.lastReport) or
                     Shift_Left
                       (Unsigned_64 (xhciDiagnostics.lastLength), 32) or
                     Shift_Left
                       (Unsigned_64 (xhciDiagnostics.lastCompletion), 40) or
                     Shift_Left
                       (Unsigned_64
                          (CuBit.Devices.Interrupt_Mode'Enum_Rep
                             (xhciDiagnostics.interruptMode)), 48) or
                     (if xhciDiagnostics.valid then 2**56 else 0)])));
      else
         debugPrint ("devmgr: rejected unknown message" & LF);
         ret := Unsigned_64
           (reply
              (from,
               (tag => (label => CuBit.Devices.REPLY_ERROR,
                        length => 0, flags => 0, reserved => 0),
                authorityTag => 0, words => [others => 0])));
      end if;
   end loop;

end main;
