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
with Devmgr_Children;
with CuBit.Child_Exits;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;
with CuBit.Service_Exchange;
with CuBit.Startup_Tables;
with Devmgr_Authority;
with Devmgr_Launches;
with Fs_Attachments;
with Native_GPU_Probe_Protocol;
with Boot_RTC;
with CCL.Configurations;
with CCL.Declarations;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with CuBit.System_Facts;
with Launch_Configuration;
with CuBit.Network_Authority;
with CuBit.Devices;
with CuBit.Virtio_Net_Control;
with CuBit.Virtio_Input_Control;
with CuBit.NVMe_Control;
with CuBit.Device_Interrupts;
with Device_Vector_ABI;
with CCL.Resource_Sections;
with Driver_Manifests;
with Devmgr_Bindings;
with CCL.Device_Sections;
with CCL.Device_Index;
with CuBit.Driver_Readiness;
with Intel_GPU_ADLN_Devices;
with PCI_Message_Interrupts;
with PCI_Message_Functions;
with CuBit.Filesystems;
with CuBit.Memory_Grants;
with CuBit.File_Access;
with Cpio;
with Deferred_Warnings;
with XHCI_DMA_Layout;
with XHCI_Capabilities;
with Intel_GPU_Probe;
with Intel_GPU_Display_Pages;
with Intel_GPU_Pipe_Pages;
with Intel_GPU_Modeset_Pages;
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

   --  Address where kernel mapped the initrd into our space; we map it at
   --  the same address into the filesystem and the RAM block driver.
   INITRD_BASE : constant Unsigned_64 := Launch_Configuration.Boot_Image_Address;

   package LC renames Launch_Configuration;
   --  The boot framebuffer's physical pages (the kernel's launch
   --  configuration for devmgr), passed on to procmgr.
   bootFramebuffer : LC.Physical_Span;

   --  DMA virtual base for drivers
   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;

   --  A legacy PCI interrupt line's vector (the IOAPIC routes line N to
   --  LEGACY_VECTOR_BASE + N).
   LEGACY_VECTOR_BASE : constant Unsigned_64 := 32;

   --  PCI config I/O ports
   PCI_CONFIG_ADDR : constant Unsigned_16 := 16#0CF8#;
   PCI_CONFIG_DATA : constant Unsigned_16 := 16#0CFC#;

   --  PCI base classes and class codes (base class << 8 | subclass).
   CLASS_BASE_STORAGE    : constant Unsigned_8 := 16#01#;
   CLASS_BASE_NETWORK    : constant Unsigned_8 := 16#02#;
   CLASS_BASE_DISPLAY    : constant Unsigned_8 := 16#03#;
   CLASS_BASE_MULTIMEDIA : constant Unsigned_8 := 16#04#;
   CLASS_BASE_SERIAL_BUS : constant Unsigned_8 := 16#0C#;
   CLASS_STORAGE_IDE   : constant Unsigned_16 := 16#0101#;
   CLASS_MULTIMEDIA_HDA : constant Unsigned_16 := 16#0403#;
   CLASS_DISPLAY_VGA    : constant Unsigned_16 := 16#0300#;
   CLASS_SERIAL_USB     : constant Unsigned_16 := 16#0C03#;

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
   PCI_HEADER_MULTIFUNCTION : constant Unsigned_8 := 16#80#;
   PCI_PROG_IF_XHCI         : constant Unsigned_8 := 16#30#;

   --  VirtIO modern PCI capability types
   PCI_CAP_ID_VENDOR_SPECIFIC : constant Unsigned_8 := 9;
   VIRTIO_PCI_CAP_COMMON_CFG  : constant Unsigned_8 := 1;
   VIRTIO_PCI_CAP_NOTIFY_CFG  : constant Unsigned_8 := 2;
   VIRTIO_PCI_CAP_ISR_CFG     : constant Unsigned_8 := 3;
   VIRTIO_PCI_CAP_DEVICE_CFG  : constant Unsigned_8 := 4;

   --  devmgr's authority (CAP-002): the roots the kernel placed in its
   --  table, reported in its launch configuration. Every capability it
   --  hands out is a narrowed copy of one of them or of a SPAWN result.
   package H renames CuBit.Handles;
   package HC renames CuBit.Handles.Control;
   package IPC renames CuBit.Handles.IPC;
   package DL renames Devmgr_Launches;
   package ST renames CuBit.Startup_Tables;
   use type IPC.Reply_Outcome;
   use type DL.Child;
   --  Set first thing in the body (Devmgr_Authority.Initialize).
   Root : CuBit.Handles.Boot_Roots.Roots;

   Read_Write_Grant : constant H.Rights := (Read | Write | Grant => True, others => False);

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
   --  NETLINK interim (until devmgr starts one instance per device for its
   --  dedicated setups, docs/device-matching.md "Follow-ups"): every
   --  virtio-net function gets its own driver. netDev and the net* state
   --  below describe the one being set up; these keep each one's.
   Maximum_Net_Devices : constant := 8;
   subtype Net_Device_Count is Natural range 0 .. Maximum_Net_Devices;
   subtype Net_Device_Index is Positive range 1 .. Maximum_Net_Devices;
   netDevs     : array (Net_Device_Index) of PCIDeviceInfo;
   netDevCount : Net_Device_Count := 0;
   netPIDs     : array (Net_Device_Index) of Process_ID := [others => No_Process];
   hdaDev    : PCIDeviceInfo;
   xhciDev   : PCIDeviceInfo;
   gpuDev    : PCIDeviceInfo;
   inputDev  : PCIDeviceInfo;
   intelDev : PCIDeviceInfo;
   Intel_Inspection_PID : Process_ID := No_Process;
   --  intel-gpu's launch: devmgr's process control and untagged endpoint
   --  (Grant: admitted applications get copies badged with their session),
   --  and devmgr's own copy badged Intel_GPU_Boot.Broker_Tag, through which
   --  it submits configuration and admission requests.
   --  The badge on intel-gpu's endpoint to devmgr ("device-manager"): its
   --  runtime requests are recognized by it.
   Intel_Ready_Tag : constant Unsigned_64 := 16#4947#;
   Intel_Child   : DL.Child := DL.No_Child;
   Intel_Control : H.Process;
   Intel_Mailbox : H.Endpoint;
   Intel_Broker  : H.Endpoint;
   package GP renames Native_GPU_Probe_Protocol;
   function Intel_Driver return H.Endpoint is (Intel_Broker);
   function Intel_Driver_PID return Unsigned_64 is (To_Word (Intel_Inspection_PID));
   function Intel_Render_Source return H.Endpoint is (Intel_Mailbox);
   package GPU_Launch is new Intel_Render_Broker
     (Intel_Driver, Intel_Driver_PID, Intel_Render_Source, 16#4750_1000#, 16#4750_1FFF#);
   GPU_Launches : GPU_Launch.Broker;
   Launch_Receipt : aliased CompletionEntry;
   Request_Found, Receipt_Consumed : Boolean;
   Launch_Now : Unsigned_64;
   Launch_Activity : Activity_Result;
   GPU_Viewer_PID : Process_ID := No_Process;
   GPU_Viewer_Child : DL.Child := DL.No_Child;
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
   Intel_Pipe_Granted : array (Intel_GPU_Pipe_Pages.Page_Index) of Boolean := [others => False];
   Intel_Modeset_Granted : array (Intel_GPU_Modeset_Pages.Page) of Boolean := [others => False];
   Intel_GGTT_Granted : Boolean := False;
   Intel_GGTT_BAR, Intel_GGTT_Bytes : Unsigned_64 := 0;
   Intel_GGTT_Write_Attempted : Boolean := False;
   Intel_Buffer_Attempted : Boolean := False;
   Intel_Buffer_Physical : Unsigned_64 := 0;
   Intel_ADS_Attempted : Boolean := False;
   Intel_ADS_Physical : Unsigned_64 := 0;
   function Context_Allocation_Ready return Boolean is
     (Intel_Inspection_PID /= No_Process and then Intel_Reset_Authorized and then
      Intel_Config_Frozen and then Intel_GGTT_Granted);
   function Allocate_Buffer_Block (CPU : Unsigned_64) return Unsigned_64;
   package Buffer_Allocations is new Intel_GPU_Extent_Allocator
     (Context_Allocation_Ready, Allocate_Buffer_Block);
   Intel_Buffer_Pool : Buffer_Allocations.Pool;
   Intel_Heap_Configured : Boolean := False;
   Intel_Requested_Bytes : Unsigned_64 := 0;
   --  DMA modes (kernel ALLOC_DMA): mapped write-back, and mapped below
   --  the ceiling.
   DMA_Mode_Retained   : constant Unsigned_64 := 1;
   DMA_Mode_Buffer     : constant Unsigned_64 := 3;
   --  The retained buffer intel-gpu asks for once (16#022C#): 2^8 pages
   --  mapped at a fixed address, below 4 GiB like every Intel DMA block.
   Intel_Retained_Order   : constant Unsigned_64 := 8;
   Intel_Retained_Virtual : constant Unsigned_64 := 16#6100_0000#;
   Intel_Retained_Bytes   : constant Unsigned_64 := 1024 * 1024;
   Intel_DMA_Ceiling      : constant Unsigned_64 := 2 ** 32;
   function Allocate_Buffer_Block (CPU : Unsigned_64) return Unsigned_64 is
     (HC.Allocate_DMA (Intel_Inspection_PID, Intel_Control,
       Intel_GPU_Physical_Extents.Allocation_Order, CPU, DMA_Mode_Buffer,
       Buffer_Allocations.DMA_Ceiling (Intel_Buffer_Pool)));
   package Intel_Extent_Growth is new Intel_GPU_Extent_Growth
     (Buffer_Allocations, Intel_Buffer_Pool, Intel_GPU_Metadata_Platform.Storage,
      Context_Allocation_Ready);
   Intel_Buffer_Arena_Granted : Boolean := False;
   -- Metadata policy is independent of BO/RAM size. Reserve VA lazily and
   -- commit only demanded chunks; this is not a preallocated million objects.
   Intel_Metadata_Quota : constant Unsigned_64 := 64 * 1024 * 1024;
   Intel_Record_Quota : constant Positive := 1_048_576;
   --  The saved reply of the allocation request in progress.
   Intel_Allocation_Reply : H.Reply := H.No_Reply;
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
      Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
   begin
      if Saved.Saved then
         Intel_Allocation_Reply := Saved.Handle;
      end if;
      return Saved.Saved;
   end Save_Intel_Allocation_Reply;
   procedure Acquire_Intel_Buffer
     (Index : Positive; Pages : Intel_GPU_Buffer_Backing.Page_Count;
      Generation : Unsigned_32; Buffer : out Intel_GPU_Buffer_Reply.Extent_View;
      Success, Pending : out Boolean) is
      Empty : Intel_GPU_Buffer_Reply.Extent_View;
   begin
      if not Intel_Heap_Configured then
         declare
            Policy : constant Intel_GPU_Buffer_Backing.Heap_Policy :=
              Intel_GPU_Buffer_Backing.Native_System_Heap
                (CuBit.System_Facts.Memory_Bytes);
         begin
            Buffer_Allocations.Configure_Heap
              (Intel_Buffer_Pool, Policy.Byte_Quota, Policy.DMA_Limit,
               Intel_Heap_Configured);
         end;
         if not Intel_Heap_Configured then
            Buffer := Empty;
            Success := False; Pending := False;
            return;
         end if;
      end if;
      Intel_Extent_Growth.Step
        (To_Word (Intel_Inspection_PID), Index, Pages, Generation, Buffer, Success, Pending);
   end Acquire_Intel_Buffer;
   procedure Reply_Intel_Allocation
     (Index : Positive; Generation : Unsigned_32;
      Buffer : Intel_GPU_Buffer_Reply.Extent_View; Success : Boolean) is
      Response : Message :=
        (tag => (label => 16#F001#, length => 0,
                 protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0]);
      Delivery : Boolean;
      pragma Unreferenced (Delivery);
   begin
      if Success then
         Intel_Buffer_Arena_Granted := True;
         Response :=
           (tag => (label => 16#F004#, length => 4,
                    protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
            words => [Intel_GPU_Buffer_Reply.CPU_Address (Buffer),
              Intel_GPU_Buffer_Reply.Byte_Count (Buffer), To_Word (Intel_Inspection_PID),
              Intel_GPU_Buffer_Backing.Allocation_Key (Index, Generation)]);
      else
         Response :=
           (tag => (label => 16#F001#, length => 4,
                    protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
            words => [Intel_GPU_Buffer_Backing.Denial_Version,
              Intel_GPU_Buffer_Backing.Allocation_Key (Index, Generation),
              Intel_Requested_Bytes,
              Unsigned_64 (Intel_GPU_Buffer_Backing.Allocation_Reason'Pos
                (Buffer_Allocations.Last_Allocation_Reason (Intel_Buffer_Pool)))]);
      end if;
      -- Delivery failure consumes the saved capability, not the allocation.
      -- Retained backing must not be recycled on ambiguous client delivery.
      Delivery := IPC.Answer (Intel_Allocation_Reply, Response);
      Intel_Allocation_Reply := H.No_Reply;
   end Reply_Intel_Allocation;
   package Intel_Allocation_Growth is new Intel_GPU_Allocation_Growth
     (Intel_Record_Growth, Context_Allocation_Ready, Save_Intel_Allocation_Reply,
      Acquire_Intel_Buffer, Reply_Intel_Allocation);
   Intel_Allocations : Intel_Allocation_Growth.Dispatcher;
   gpuIsPrimary : Boolean := False;

   --  Service PIDs
   filesystemPID : Process_ID := No_Process;
   ataPID        : Process_ID := No_Process;
   ramdiskPID    : Process_ID := No_Process;
   nvmePID       : Process_ID := No_Process;
   netstackPID   : Process_ID := No_Process;
   virtioNetPID  : Process_ID := No_Process;
   netIOBase : Unsigned_64 := 0;
   --  A modern (virtio 1.0) device: its register layout within the mapped
   --  BAR (CuBit.Virtio_Net_Control.Configure_Modern).
   netModern : Boolean := False;
   netCommonOff, netDeviceOff, netNotifyOff, netNotifyMult : Unsigned_64 := 0;
   netModernBytes : Unsigned_64 := 0;
   netTransportReady : Boolean := False;
   procmgrPID    : Process_ID := No_Process;
   shellPID      : Process_ID := No_Process;
   hdaPID        : Process_ID := No_Process;
   mixerPID      : Process_ID := No_Process;
   ps2PID        : Process_ID := No_Process;
   xhciPID       : Process_ID := No_Process;
   configPID     : Process_ID := No_Process;
   netmgrPID     : Process_ID := No_Process;
   virtioGpuPID  : Process_ID := No_Process;
   virtioInputPID : Process_ID := No_Process;
   --  Their launches (Devmgr_Launches).
   filesystemChild, ataChild, ramdiskChild, nvmeChild, netstackChild,
     virtioNetChild, procmgrChild, hdaChild, mixerChild, ps2Child,
     xhciChild, configChild, netmgrChild, virtioGpuChild,
     virtioInputChild : DL.Child := DL.No_Child;

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
      ignore := portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, addr);
      return Unsigned_32 (portInp32 (Root.PCI_Ports, PCI_CONFIG_DATA) and 16#FFFF_FFFF#);
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
      ignore := portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, addr);
      data := portInp32 (Root.PCI_Ports, PCI_CONFIG_DATA);
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
      ignore := portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, addr);
      ignore := portOutp32 (Root.PCI_Ports, PCI_CONFIG_DATA, value);
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
      ignore := portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, addr);
      -- Never widen this into a DWORD read/modify/write: e.g. COMMAND's
      -- neighboring STATUS word contains write-one-to-clear bits. PCI
      -- mechanism 1 selects the word with the CFC/CFE data-port lane.
      ignore := portOutp16
        (Root.PCI_Ports, PCI_CONFIG_DATA + Unsigned_16 (offset and 2), value);
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
        not intelDev.found or else not Intel_GPU_ADLN_Devices.Supported_Identity (Intel_Claim_Identity)
      then return; end if;
      for Index in 0 .. 63 loop
         if portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, Intel_Config_Address (Index * 4)) /= 0 then return; end if;
         Raw := portInp32 (Root.PCI_Ports, PCI_CONFIG_DATA);
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
      if portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, Intel_Config_Address (Offset)) /= 0 then return; end if;
      Success := portOutp16 (Root.PCI_Ports, PCI_CONFIG_DATA + Unsigned_16 (Offset mod 4), Value) = 0;
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

         --  What kind of device it is, for the Devices app. Which driver
         --  drives it is the drivers' declared matches' decision
         --  (Bind_Devices), not this classification's.
         case Unsigned_8 (Shift_Right (classCode, 8)) is
            when CLASS_BASE_STORAGE => observedKind := CuBit.Devices.Storage_Controller;
            when CLASS_BASE_NETWORK => observedKind := CuBit.Devices.Network_Controller;
            when CLASS_BASE_DISPLAY => observedKind := CuBit.Devices.Display_Controller;
            when CLASS_BASE_MULTIMEDIA =>
               if classCode = CLASS_MULTIMEDIA_HDA then
                  observedKind := CuBit.Devices.Audio_Controller;
               end if;
            when CLASS_BASE_SERIAL_BUS =>
               if classCode = CLASS_SERIAL_USB and then progIf = PCI_PROG_IF_XHCI then
                  observedKind := CuBit.Devices.USB_Controller;
               end if;
            when others => null;
         end case;

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
   --  devmgr's own endpoints to the filesystem and config services (the
   --  SPAWN endpoints of those children).
   Filesystem_Endpoint : H.Endpoint := H.No_Endpoint;
   Config_Endpoint     : H.Endpoint := H.No_Endpoint;

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
           (Filesystem_Endpoint, Boot_Buffer, Natural (Boot_Buffer_Bytes / 4096), True,
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
                    length => 4, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0,
            words => [To_Word (Registered_Driver (DRIVER_DEVMGR)), 1,
                      Unsigned_64 (Boot_Grant.slot),
                      Unsigned_64 (Boot_Grant.generation)]);
         Tag := capCall (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever);
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
      Tag := capCall (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever);
      if Tag.label /= REPLY_OK or else Tag.length /= 2 then return 0; end if;
      Handle := CuBit.Filesystems.File_Handle (Request.words (0));
      Size := Request.words (1);
      Read_Bytes := 0;
      if Size in 1 .. Boot_Buffer_Bytes then
         Request := CuBit.Filesystems.Read_Request (Handle, Boot_Grant, Size);
         Tag := capCall (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever);
         if Tag.label = REPLY_OK and then Tag.length >= 1 and then
           Request.words (0) = Size
         then Read_Bytes := Size; end if;
      end if;
      Request := CuBit.Filesystems.Close_Request (Handle);
      Tag := capCall (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever);
      if Tag.label /= REPLY_OK then return 0; end if;
      return Read_Bytes;
   end Read_Boot_Image;

   ---------------------------------------------------------------------------
   --  Spawn a program from the boot image (or, for optical boot, the
   --  filesystem) suspended: No_Child on failure. What it is given is
   --  decided before it runs (Devmgr_Launches).
   ---------------------------------------------------------------------------
   function spawnFromBootStorage (name     : String;
                                  priority : Unsigned_64) return DL.Child
   is
      idx  : Natural;
      addr : Unsigned_64;
      size : Unsigned_64;
      Spawned : DL.Child;
   begin
      idx := Cpio.findFile (cpioArchive, name);
      if idx >= cpioArchive.count then
         size := Read_Boot_Image (name);
         if size = 0 then
            debugPrint ("devmgr: boot image unavailable: " & name & LF);
            return DL.No_Child;
         end if;
         addr := Unsigned_64 (To_Integer (Boot_Buffer));
         debugPrint ("devmgr: loaded from filesystem: " & name & LF);
      else
         addr := INITRD_BASE +
                 Unsigned_64 (cpioArchive.files (idx).dataOff);
         size := Unsigned_64 (cpioArchive.files (idx).dataSize);
      end if;
      Spawned := DL.Spawn (name, To_Address (Integer_Address (addr)), size, priority);
      --  A driver's own .cubit.devices must say what the image's driver
      --  index bound it by (HW-002): an index cannot bind a driver to a
      --  device the driver never declared.
      if Spawned /= DL.No_Child then
         declare
            Driver : constant CCL.Device_Index.Driver_Count := Devmgr_Bindings.Driver_Named (name);
            Own : constant CCL.Device_Sections.Match_Table := DL.Devices_Of (Spawned);
         begin
            if (if Driver = 0 then Own.Count > 0
                else not Devmgr_Bindings.Agrees (Driver, Own))
            then
               debugPrint ("devmgr: " & name & " declares devices other than the image's " &
                           "driver index lists; not started" & LF);
               DL.Abandon (Spawned);
               return DL.No_Child;
            end if;
         end;
      end if;
      return Spawned;
   end spawnFromBootStorage;

   function PID_Of (Item : DL.Child) return Process_ID is
     (if Item = DL.No_Child then No_Process else DL.PID (Item));

   ---------------------------------------------------------------------------
   -- Mint a capability into a target process
   ---------------------------------------------------------------------------
   --  Refused maps and mints reach logstore once it runs (the kernel's own
   --  "kernel: device-memory denied" record says why).
   --  logstore's untagged endpoint, once procmgr offered it
   --  (CuBit.Service_Exchange.Available), and the desktop's.
   Logstore_Endpoint : H.Endpoint := H.No_Endpoint;
   Desktop_Endpoint  : H.Endpoint := H.No_Endpoint;
   function Logstore return H.Endpoint is (Logstore_Endpoint);
   package Warnings is new Deferred_Warnings (Issuance => 3, Collector => Logstore);
   function Hex (Value : Unsigned_64) return String renames Warnings.Hex;
   --  Refused launches (a child's startup entry, spawn, install) as warnings.
   procedure Forward_Launch_Refusals is new DL.Forward_Refusals (Warnings.Warn);

   --  "devmgr: child <name> (<identity>) stopped (killed or faulted)" or
   --  "... exited code N": a driver or service devmgr started has ended.
   --  The kernel's "kernel: process killed" record says why when it faulted.
   Exit_Check_Interval_Ms : constant := 250;
   Next_Exit_Check : Unsigned_64 := 0;
   procedure Log_Child_Exits (Now : Unsigned_64) is
      Child  : Process_ID;
      Name   : Devmgr_Children.Child_Name;
      Length : Natural;
      Report : CuBit.Child_Exits.Report;
      Found  : Boolean;
      use type CuBit.Child_Exits.Termination_Kind;
   begin
      if Now < Next_Exit_Check then
         return;
      end if;
      Next_Exit_Check := (if Now <= Unsigned_64'Last - Exit_Check_Interval_Ms
                          then Now + Exit_Check_Interval_Ms else Unsigned_64'Last);
      loop
         Devmgr_Children.Take_Next_Exit (Child, Name, Length, Report, Found);
         exit when not Found;
         declare
            Line : constant String :=
              "devmgr: child " & Name (1 .. Length) & " (" & Hex (To_Word (Child)) & ") " &
              (if Report.Kind = CuBit.Child_Exits.Stopped
               then "stopped (killed or faulted)"
               else "exited code" & Unsigned_64'Image (Report.Code));
         begin
            if Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0 then
               Warnings.Warn (Line);
            else
               Warnings.Inform (Line);
            end if;
         end;
      end loop;
   end Log_Child_Exits;


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
   --  Devices bound by presence (HW-002, docs/device-matching.md). Every
   --  driver in the image is in its driver index; each present device binds
   --  the driver whose declared entry matches it most specifically. One
   --  line per device says which, or that nothing matched.
   ---------------------------------------------------------------------------
   package DS renames CCL.Device_Sections;
   package DI renames CCL.Device_Index;
   use type DI.Binding_Kind;
   use type DI.Decode_Status;
   DEVICE_INDEX_FILE : constant String := "devices.index";
   deviceBindings : array (Inventory_Index) of DI.Binding := [others => (others => <>)];
   --  Platform devices (devmgr's platform catalog). Assumed present: the
   --  PS/2 controller and the CMOS clock (no ACPI/FADT check yet). The
   --  primary ATA channel is present when PCI shows an IDE controller.
   ps2Present, ataPrimaryPresent : Boolean := False;

   function PCI_Function_Of (Item : Inventory_Device) return DS.PCI_Function is
     (Vendor    => Item.vendorID,
      Device    => Item.deviceID,
      Class     => Unsigned_8 (Shift_Right (Item.classCode, 8)),
      Subclass  => Unsigned_8 (Item.classCode and 16#FF#),
      Program   => Item.progIf);

   --  Drivers devmgr has dedicated setup for, one instance each; every
   --  other PCI driver gets the generic launch (Launch_Generic_PCI).
   function Has_Driver_Setup (Name : String) return Boolean is
     (strEq (Name, "nvme.drv") or else strEq (Name, "xhci.drv")
      or else strEq (Name, "hda.drv") or else strEq (Name, "virtio-net.drv")
      or else strEq (Name, "virtio-gpu.drv") or else strEq (Name, "virtio-input.drv")
      or else strEq (Name, "intel-gpu.drv") or else strEq (Name, "ata.drv")
      or else strEq (Name, "ps2.drv"));

   --  Which of several bound devices a single-instance setup starts: the
   --  first for these, the last in scan order for the rest (as before
   --  HW-002's matching).
   function Starts_First (Name : String) return Boolean is
     (strEq (Name, "virtio-input.drv") or else strEq (Name, "intel-gpu.drv"));

   --  The device bound to driver Name: the first one, or the last one
   --  (the PCI scan's order) when Last_Match.
   function Bound_Location (Name : String; Last_Match : Boolean) return PCIDeviceInfo is
      Driver : constant DI.Driver_Count := Devmgr_Bindings.Driver_Named (Name);
      Result : PCIDeviceInfo;
   begin
      if Driver = 0 then
         return Result;
      end if;
      for I in 1 .. inventoryCount loop
         if deviceBindings (I).Kind = DI.Bound and then deviceBindings (I).Driver = Driver then
            Result := inventory (I).location;
            exit when not Last_Match;
         end if;
      end loop;
      return Result;
   end Bound_Location;

   function Location_Text (Location : PCIDeviceInfo) return String is
     (Devmgr_Bindings.Hex (Unsigned_64 (Location.bus), 2) & ":" &
      Devmgr_Bindings.Hex (Unsigned_64 (Location.slot), 2) & "." &
      Devmgr_Bindings.Hex (Unsigned_64 (Location.func), 1));

   function Device_Text (Item : Inventory_Device) return String is
     (Location_Text (Item.location) & " " &
      Devmgr_Bindings.Hex (Unsigned_64 (Item.vendorID), 4) & ":" &
      Devmgr_Bindings.Hex (Unsigned_64 (Item.deviceID), 4) & " class " &
      Devmgr_Bindings.Hex (Unsigned_64 (Shift_Right (Item.classCode, 8)), 2) & "." &
      Devmgr_Bindings.Hex (Unsigned_64 (Item.classCode and 16#FF#), 2) & "." &
      Devmgr_Bindings.Hex (Unsigned_64 (Item.progIf), 2));

   --  Whether driver Name is the one bound to platform device P.
   function Platform_Bound (Name : String; P : DS.Platform_Device) return Boolean is
     (Devmgr_Bindings.Driver_Named (Name) /= 0
      and then Devmgr_Bindings.Platform_Driver (P) = Devmgr_Bindings.Driver_Named (Name));

   procedure Bind_Devices is
      Index_File : constant Natural := Cpio.findFile (cpioArchive, DEVICE_INDEX_FILE);
      Status : DI.Decode_Status := DI.Invalid_Length;
      Bound_Total, Unbound_Total : Natural := 0;
      Index : DI.Index;
   begin
      if Index_File < cpioArchive.count
        and then cpioArchive.files (Index_File).dataSize <= DI.MAX_INDEX_BYTES
      then
         declare
            Data : constant DI.Byte_Array (1 .. Natural (cpioArchive.files (Index_File).dataSize))
              with Import, Address => To_Address (Integer_Address
                (INITRD_BASE + Unsigned_64 (cpioArchive.files (Index_File).dataOff)));
         begin
            Devmgr_Bindings.Load (Data, Status);
         end;
      end if;
      if Status /= DI.Decoded then
         Warnings.Warn ("devmgr: no valid " & DEVICE_INDEX_FILE & " (" &
           DI.Status_Name (Status) & "); no driver is bound");
         return;
      end if;
      Index := Devmgr_Bindings.Index;
      for I in 1 .. inventoryCount loop
         Devmgr_Bindings.Bind (PCI_Function_Of (inventory (I)), deviceBindings (I));
      end loop;
      ataPrimaryPresent := (for some I in 1 .. inventoryCount =>
                              inventory (I).classCode = CLASS_STORAGE_IDE);
      ps2Present := True;

      --  The devices devmgr's single-instance setup will start.
      nvmeDev  := Bound_Location ("nvme.drv", not Starts_First ("nvme.drv"));
      netDev   := Bound_Location ("virtio-net.drv", not Starts_First ("virtio-net.drv"));
      --  Every function bound to virtio-net.drv, in bus order (NETLINK interim).
      declare
         Net_Driver : constant DI.Driver_Count := Devmgr_Bindings.Driver_Named ("virtio-net.drv");
      begin
         for I in 1 .. inventoryCount loop
            if Net_Driver /= 0 and then deviceBindings (I).Kind = DI.Bound and then
              deviceBindings (I).Driver = Net_Driver and then netDevCount < Maximum_Net_Devices
            then
               netDevCount := netDevCount + 1;
               netDevs (netDevCount) := inventory (I).location;
            end if;
         end loop;
      end;
      hdaDev   := Bound_Location ("hda.drv", not Starts_First ("hda.drv"));
      xhciDev  := Bound_Location ("xhci.drv", not Starts_First ("xhci.drv"));
      gpuDev   := Bound_Location ("virtio-gpu.drv", not Starts_First ("virtio-gpu.drv"));
      inputDev := Bound_Location ("virtio-input.drv", not Starts_First ("virtio-input.drv"));
      intelDev := Bound_Location ("intel-gpu.drv", not Starts_First ("intel-gpu.drv"));
      for I in 1 .. inventoryCount loop
         if Same_Location (inventory (I).location, gpuDev) then
            gpuIsPrimary := inventory (I).classCode = CLASS_DISPLAY_VGA;
         end if;
      end loop;

      for I in 1 .. inventoryCount loop
         declare
            B : constant DI.Binding := deviceBindings (I);
            Item : constant Inventory_Device := inventory (I);
         begin
            case B.Kind is
               when DI.Unbound =>
                  Unbound_Total := Unbound_Total + 1;
                  Warnings.Inform ("devmgr: match " & Device_Text (Item) & " -> unmatched");
               when DI.Ambiguous =>
                  Unbound_Total := Unbound_Total + 1;
                  Warnings.Warn ("devmgr: match " & Device_Text (Item) & " -> ambiguous: " &
                    Devmgr_Bindings.Name_Of (B.Driver) & " and " &
                    Devmgr_Bindings.Name_Of (B.Rival) & " match equally; neither started");
               when DI.Bound =>
                  Bound_Total := Bound_Total + 1;
                  declare
                     Name : constant String := Devmgr_Bindings.Name_Of (B.Driver);
                     Line : constant String :=
                       "devmgr: match " & Device_Text (Item) & " -> " & Name & " (" &
                       Devmgr_Bindings.Image (Index.Drivers (B.Driver).Matches.Entries (B.At_Entry)) & ")";
                     Started : constant Boolean :=
                       not Has_Driver_Setup (Name)
                       or else Same_Location
                         (Item.location, Bound_Location (Name, Last_Match => not Starts_First (Name)));
                  begin
                     if Started then
                        Warnings.Inform (Line);
                     else
                        Warnings.Inform (Line & "; not started: devmgr starts one " & Name);
                     end if;
                  end;
            end case;
         end;
      end loop;

      --  Platform devices, and entries devmgr does not enumerate.
      for D in 1 .. Index.Count loop
         for E in 1 .. Index.Drivers (D).Matches.Count loop
            declare
               M : constant DS.Device_Match := Index.Drivers (D).Matches.Entries (E);
               Name : constant String := Devmgr_Bindings.Name_Of (D);
               use type DS.Match_Kind;
               use type CCL.Resource_Sections.Platform_Device;
            begin
               case M.Kind is
                  when DS.Platform =>
                     if (M.Device = CCL.Resource_Sections.PS2_Controller and then ps2Present)
                       or else (M.Device = CCL.Resource_Sections.ATA_Primary and then ataPrimaryPresent)
                       or else M.Device = CCL.Resource_Sections.CMOS_RTC
                     then
                        Warnings.Inform ("devmgr: match platform " &
                                         Devmgr_Bindings.Platform_Name (M.Device) & " -> " & Name);
                     else
                        Warnings.Inform ("devmgr: match platform " &
                                         Devmgr_Bindings.Platform_Name (M.Device) & " absent; " &
                                         Name & " not started");
                     end if;
                  when DS.ACPI_ID | DS.USB_Class =>
                     debugPrint ("devmgr: " & Name & " declares " & Devmgr_Bindings.Image (M) &
                                 ": not enumerated by devmgr" & LF);
                  when DS.PCI_IDs | DS.PCI_Class =>
                     null;
               end case;
            end;
         end loop;
      end loop;
      Warnings.Inform ("devmgr: drivers bound to" & Natural'Image (Bound_Total) &
        " PCI devices;" & Natural'Image (Unbound_Total) & " matched no driver" &
        (if inventoryOverflow then " (inventory overflowed)" else ""));
   end Bind_Devices;


   ---------------------------------------------------------------------------
   -- Assign CPU affinity based on service type
   ---------------------------------------------------------------------------
   --  The CPU a service or driver is pinned to. A driver's message
   --  interrupts are delivered to the same CPU (docs/msi-interrupts.md).
   function cpuFor (name : String) return Natural is
      cpu : Unsigned_64 := 0;
   begin
      if numCPUs <= 1 then
         return 0;
      end if;

      --  Storage/FS/input services pinned to CPU 0
      if strEq (name, "filesystem.svc") or strEq (name, "ata.drv") or
         strEq (name, "nvme.drv") or strEq (name, "ramdisk.drv") or
         strEq (name, "procmgr.svc") or
         strEq (name, "ps2.drv") or strEq (name, "xhci.drv") or
         strEq (name, "virtio-input.drv")
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
      return Natural (cpu);
   end cpuFor;

   --  Where each placed process was pinned, so its device vectors can be
   --  delivered to the same CPU (cpuOf).
   type Placement is record
      PID : Process_ID := No_Process;
      CPU : Natural := 0;
   end record;
   MAX_PLACEMENTS : constant := 64;
   placements : array (1 .. MAX_PLACEMENTS) of Placement;
   placementCount : Natural range 0 .. MAX_PLACEMENTS := 0;

   procedure assignCPU (child : DL.Child;
                        name  : String)
   is
      ignore : Boolean;
      cpu : Natural;
      pid : constant Process_ID := PID_Of (child);
   begin
      if numCPUs <= 1 or else child = DL.No_Child then
         return;
      end if;
      cpu := cpuFor (name);
      ignore := HC.Set_CPU (pid, DL.Control (child), Unsigned_64 (cpu));
      if placementCount < MAX_PLACEMENTS then
         placementCount := placementCount + 1;
         placements (placementCount) := (pid, cpu);
      end if;
   end assignCPU;

   --  The CPU pid was pinned to; CPU 0 for an unplaced process.
   function cpuOf (pid : Process_ID) return Natural is
   begin
      for I in reverse 1 .. placementCount loop
         if placements (I).PID = pid then
            return placements (I).CPU;
         end if;
      end loop;
      return 0;
   end cpuOf;


   --  IPC ready protocol constants
   OP_READY       : constant Unsigned_32 := CuBit.Driver_Readiness.Ready_Label;
   OP_NOT_PRESENT : constant Unsigned_32 := CuBit.Driver_Readiness.Not_Present_Label;
   --  mixer.svc may reserve real-time CPU for one audio period: 1.5 ms of
   --  every 5 ms (docs/scheduler.md, "Soft real time").
   MIXER_REALTIME_BUDGET_US : constant Unsigned_64 := 1_500;
   MIXER_REALTIME_PERIOD_US : constant Unsigned_64 := 5_000;

   --  Our PID (discovered via registerDriver)
   myPID : Process_ID := No_Process;

   ---------------------------------------------------------------------------
   -- Wait for a driver to signal readiness via IPC.
   -- Returns True if driver sent OP_READY, False if OP_NOT_PRESENT.
   ---------------------------------------------------------------------------
   function waitReady (driverPID : Process_ID) return Boolean is
      sender : Process_ID;
      rdyMsg : Message;
      ignore : Unsigned_64;
   begin
      loop
         receive (sender, rdyMsg);
         if sender = driverPID then
            ignore := reply (sender, NULL_MESSAGE);
            return rdyMsg.tag.label = OP_READY;
         elsif sender = Intel_Inspection_PID and then sender /= No_Process and then
           rdyMsg.authorityTag = Intel_Ready_Tag
         then
            --  intel-gpu's runtime requests wait for the service loop: busy.
            ignore := reply (sender,
              (tag => (label => 16#F002#, length => 0,
                       protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0]));
         else
            ignore := reply (sender, NULL_MESSAGE);
         end if;
      end loop;
   end waitReady;

   ---------------------------------------------------------------------------
   -- Send a wildcard ACL to the filesystem server for a target process
   ---------------------------------------------------------------------------
   OP_SET_ACL : constant Unsigned_32 := 16#0080#;

   procedure Grant_Intel_Firmware (Launch : DL.Child) is
      Child : constant Process_ID := DL.PID (Launch);
      Path : constant String := "firmware/intel/tgl_guc_70.bin";
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if filesystemPID = No_Process or else not Boot_Granted or else
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
      Request.tag := (label => CuBit.Filesystems.OP_SET_ACL, length => 4,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words := [To_Word (Child), 1, Unsigned_64 (Boot_Grant.slot),
                        Unsigned_64 (Boot_Grant.generation)];
      Tag := capCall (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever);
      if Tag = (label => CuBit.Filesystems.REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0) and then
        Request.words = [0, 0, 0, 0]
      then
         DL.Give (Launch, "firmware", H.Copy (Filesystem_Endpoint, H.Read_Write));
         debugPrint ("devmgr: Intel firmware read scope installed" & LF);
      else
         debugPrint ("devmgr: Intel firmware read scope rejected" & LF);
      end if;
   end Grant_Intel_Firmware;

   procedure sendWildcardACL (targetPID : Process_ID) is
      aclMsg : Message;
      ignore : MessageTag;
   begin
      if filesystemPID = No_Process then
         return;
      end if;
      aclMsg.tag := (label  => OP_SET_ACL,
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0);
      aclMsg.authorityTag := 0;
      aclMsg.words := [0 => To_Word (targetPID), 1 => 0, 2 => 0, 3 => 0];
      ignore := capCall (Filesystem_Endpoint, aclMsg, CuBit.Messages.Wait_Forever);
   end sendWildcardACL;

   ---------------------------------------------------------------------------
   -- Send a wildcard ACL to the config store for a target process
   ---------------------------------------------------------------------------
   procedure sendWildcardACLConfig (targetPID : Process_ID) is
      aclMsg : Message;
      ignore : MessageTag;
   begin
      if configPID = No_Process then
         return;
      end if;
      aclMsg.tag := (label  => OP_SET_ACL,
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0);
      aclMsg.authorityTag := 0;
      aclMsg.words := [0 => To_Word (targetPID), 1 => 0, 2 => 0, 3 => 0];
      ignore := capCall (Config_Endpoint, aclMsg, CuBit.Messages.Wait_Forever);
   end sendWildcardACLConfig;

   ---------------------------------------------------------------------------
   --  Install the Config scopes a child's manifest declares in config.svc
   --  (CAP-003): exactly those, read from its ELF, never a wildcard. A
   --  child that declares none gets no ACL, so Config denies it.
   ---------------------------------------------------------------------------
   --  config.svc's ACL record (Config_Authority_Wire): rights mask, prefix
   --  length, six zero bytes, the prefix.
   ACL_ENTRY_BYTES   : constant := 72;
   ACL_RIGHTS_BYTE   : constant := 1;
   ACL_LENGTH_BYTE   : constant := 2;
   ACL_PREFIX_FIRST  : constant := 9;
   ACL_PAGE_BYTES    : constant := 4096;
   type ACL_Page is array (1 .. ACL_PAGE_BYTES) of Unsigned_8;
   pragma Compile_Time_Error
     (Driver_Manifests.MAX_SCOPES * ACL_ENTRY_BYTES > ACL_PAGE_BYTES, "ACL records exceed one page");
   ACL_Buffer : ACL_Page with Alignment => ACL_PAGE_BYTES;

   procedure sendDeclaredConfigScopes (Item : DL.Child; Name : String) is
      Count : constant Natural := DL.Config_Total (Item);
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Lent, Revoked : Boolean;
      aclMsg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if configPID = No_Process or else Count = 0 then
         return;
      end if;
      ACL_Buffer := [others => 0];
      for I in 1 .. Count loop
         declare
            Scope : constant Driver_Manifests.Config_Scope := DL.Config_Scope (Item, I);
            Base : constant Natural := (I - 1) * ACL_ENTRY_BYTES;
         begin
            ACL_Buffer (Base + ACL_RIGHTS_BYTE) := Scope.Rights;
            ACL_Buffer (Base + ACL_LENGTH_BYTE) := Unsigned_8 (Scope.Length);
            for J in 1 .. Scope.Length loop
               ACL_Buffer (Base + ACL_PREFIX_FIRST + J - 1) := Scope.Prefix (J);
            end loop;
         end;
      end loop;
      CuBit.Memory_Grants.Create_Via_Capability (Config_Endpoint, ACL_Buffer'Address, 1, False, Grant, Lent);
      if not Lent then
         debugPrint ("devmgr: config scopes not installed (no grant): " & Name & LF);
         return;
      end if;
      aclMsg.tag := (label => OP_SET_ACL, length => 4, protocol => 0, ipc => <>, reserved => 0);
      aclMsg.words := [0 => To_Word (DL.PID (Item)), 1 => Unsigned_64 (Count),
                       2 => Grant.slot, 3 => Grant.generation];
      Tag := capCall (Config_Endpoint, aclMsg, CuBit.Messages.Wait_Forever);
      if Tag.label /= REPLY_OK then
         debugPrint ("devmgr: config.svc refused the declared config scopes of " & Name & LF);
      end if;
      CuBit.Memory_Grants.Revoke (Grant, Revoked);
   end sendDeclaredConfigScopes;

   ---------------------------------------------------------------------------
   --  Attach a block driver devmgr started to the running filesystem: an
   --  offer of a Read+Write copy of the driver's endpoint (Fs_Attachments).
   ---------------------------------------------------------------------------
   function Attach_Storage
     (Which : Fs_Attachments.Device; Driver : DL.Child) return Boolean
   is
      Request : Message :=
        (tag => (label => Fs_Attachments.Attach_Label, length => Fs_Attachments.Attach_Words,
                 protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words => [Fs_Attachments.Device_Code (Which), 0, 0, 0]);
      Reply : MessageTag;
   begin
      if not H.Valid (Filesystem_Endpoint) or else Driver = DL.No_Child then
         return False;
      end if;
      Reply := IPC.Call_Offering
        (Filesystem_Endpoint, Request, CuBit.Messages.Wait_Forever,
         H.Copy (DL.Mailbox (Driver), H.Read_Write));
      if Reply.label /= REPLY_OK then
         debugPrint ("devmgr: filesystem refused a block device" & LF);
         return False;
      end if;
      return True;
   end Attach_Storage;

   ---------------------------------------------------------------------------
   --  Runtime grants (PLAN §9 item 4): a driver's request that accepts a
   --  capability is answered with REPLY_WITH_CAPABILITY.
   ---------------------------------------------------------------------------
   Accepting : constant IPC_Flags := (Accepts_Capability => True, others => False);

   --  The shared driver log budget and the issuances of devmgr's drivers
   --  in it (CuBit.Log_Protocol.Publisher_Tag).
   Driver_Log_Budget  : constant CuBit.Log_Protocol.Budget_Id := 15;
   XHCI_Log_Issuance  : constant CuBit.Log_Protocol.Publication_Issuance := 1;
   Intel_Log_Issuance : constant CuBit.Log_Protocol.Publication_Issuance := 2;
   --  netmgr's link and configuration lines (docs/net-link.md).
   Netmgr_Log_Issuance : constant CuBit.Log_Protocol.Publication_Issuance := 3;

   procedure Refuse (From : Process_ID; Answered : Boolean) is
      ignore : Unsigned_64;
   begin
      if not Answered then
         ignore := reply (From, (tag => (label => REPLY_ERR, length => 0,
                                         protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                                 words => [others => 0]));
      end if;
   end Refuse;

   --  Answer From's request (REPLY_OK, Length words) with Copy. True when
   --  it was transferred. Otherwise From was answered all the same: with
   --  the refusal the kernel reports, or REPLY_ERR when nothing was sent
   --  (the request did not accept a capability, or the copy would amplify).
   function Answer_With
     (From : Process_ID; Words : MessageWords; Length : Unsigned_8;
      Copy : H.Derivation) return Boolean
   is
      Result : constant IPC.Reply_Result := IPC.Reply_With
        (From, (tag => (label => REPLY_OK, length => Length,
                        protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => Words), Copy);
   begin
      case Result.Outcome is
         when IPC.Transferred => return True;
         when IPC.Delivered_Refused =>
            Warnings.Warn ("devmgr: grant refused by the requester's table: " &
              H.Refusal'Image (Result.Why));
            return False;
         when IPC.Refused =>
            Refuse (From, Answered => False);
            return False;
      end case;
   end Answer_With;

   procedure Close_Quietly (Item : H.Endpoint) is
      Ignore : constant IPC.Close_Result := IPC.Close (Item);
   begin
      null;
   end Close_Quietly;

   ---------------------------------------------------------------------------
   --  Message-signaled interrupts (docs/msi-interrupts.md)
   --
   --  For each driver whose manifest asks for MSI or MSI-X, devmgr allocates
   --  a block of vectors from the kernel through its vector authority. The
   --  kernel binds the block to the driver and installs the driver's CAP_IRQ
   --  in the slot the manifest named. devmgr then programs the function:
   --  every MSI-X entry written masked with the function masked, then
   --  unmasked once the driver is ready (PCI_Message_Functions). Vectors go
   --  to the local APIC of the CPU the driver is pinned to (cpuOf). A block
   --  is released only after the function is masked and disabled.
   ---------------------------------------------------------------------------
   --  Memory BARs up to Limit bytes (xHCI's arena bound by default).
   XHCI_BAR_LIMIT : constant Unsigned_64 := 1024 * 1024;
   function probeMemoryBARSize
     (dev : PCIDeviceInfo; bar : Unsigned_8; Limit : Unsigned_64 := XHCI_BAR_LIMIT)
      return Unsigned_64;

   --  PCI command register bits.
   PCI_COMMAND_INTX_DISABLE   : constant Unsigned_16 := 16#0400#;
   PCI_STATUS_CAPABILITIES    : constant Unsigned_16 := 16#0010#;
   --  Conventional capability list: pointers are dword-aligned, inside the
   --  device-specific header, and the walk is bounded against cycles.
   PCI_CAP_POINTER_MASK       : constant Unsigned_8 := 16#FC#;
   PCI_CAP_FIRST              : constant Unsigned_8 := 16#40#;
   PCI_CAP_LAST               : constant Unsigned_8 := 16#F4#;
   PCI_CAP_MAX_HOPS           : constant := 48;
   PCI_BAR_IO                 : constant Unsigned_32 := 16#0000_0001#;
   PCI_BAR_TYPE_MASK          : constant Unsigned_32 := 16#0000_0006#;
   PCI_BAR_TYPE_64            : constant Unsigned_32 := 16#0000_0004#;
   PCI_BAR_ADDRESS_MASK       : constant Unsigned_32 := 16#FFFF_FFF0#;
   PCI_BAR_BYTES              : constant := 4;
   PAGE_BYTES                 : constant := 4096;

   --  MSI-X tables are mapped into devmgr, one window per programmed
   --  function. A table is at most 2048 entries (32 KiB) and may start
   --  anywhere in its page, so a window holds nine pages.
   MSIX_TABLE_WINDOW_BASE  : constant Unsigned_64 := 16#0000_6000_2000_0000#;
   MSIX_TABLE_WINDOW_BYTES : constant Unsigned_64 := 16#0001_0000#;
   MAX_MESSAGE_FUNCTIONS   : constant := 16;
   subtype Table_Window is Natural range 0 .. MAX_MESSAGE_FUNCTIONS - 1;
   nextTableWindow : Natural := 0;

   type Message_Mode is (Polled, MSI_Mode, MSIX_Mode);

   --  One device function's message interrupts, as programmed.
   type Message_Function is record
      Mode       : Message_Mode := Polled;
      Device     : PCIDeviceInfo;
      Capability : Unsigned_8 := 0;
      --  devmgr's virtual address of MSI-X entry 0.
      Table      : Unsigned_64 := 0;
      Vectors    : CuBit.Device_Interrupts.Allocation;
      Owner      : DL.Child := DL.No_Child;
   end record;

   --  The function the hardware callbacks below act on.
   current : Message_Function;

   procedure Write_Entry
     (Index : PCI_Message_Interrupts.Entry_Index;
      Register : PCI_Message_Interrupts.Entry_Register;
      Value : Unsigned_32)
   is
      type Table_Register is new Unsigned_32 with Volatile_Full_Access;
      Target : Table_Register with Import, Address => To_Address (Integer_Address
        (current.Table + PCI_Message_Interrupts.Entry_Offset (Index, Register)));
   begin
      Target := Table_Register (Value);
   end Write_Entry;

   function Read_Function_16 (Offset : Natural) return Unsigned_16 is
     (pciReadConfig16 (current.Device.bus, current.Device.slot,
                       current.Device.func, current.Capability + Unsigned_8 (Offset)));

   procedure Write_Function_16 (Offset : Natural; Value : Unsigned_16) is
   begin
      pciWriteConfig16 (current.Device.bus, current.Device.slot,
                        current.Device.func, current.Capability + Unsigned_8 (Offset), Value);
   end Write_Function_16;

   procedure Write_Function_32 (Offset : Natural; Value : Unsigned_32) is
   begin
      pciWriteConfig32 (current.Device.bus, current.Device.slot,
                        current.Device.func, current.Capability + Unsigned_8 (Offset), Value);
   end Write_Function_32;

   package Message_Functions is new PCI_Message_Functions
     (Write_Entry, Read_Function_16, Write_Function_16, Write_Function_32);

   --  The offset of capability ID in dev's list, or zero.
   function Find_Capability (dev : PCIDeviceInfo; ID : Unsigned_8) return Unsigned_8 is
      at_offset : Unsigned_8;
   begin
      if (pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_STATUS) and
          PCI_STATUS_CAPABILITIES) = 0
      then
         return 0;
      end if;
      at_offset := pciReadConfig8 (dev.bus, dev.slot, dev.func, PCI_CAP_PTR) and
        PCI_CAP_POINTER_MASK;
      for Hop in 1 .. PCI_CAP_MAX_HOPS loop
         exit when at_offset not in PCI_CAP_FIRST .. PCI_CAP_LAST;
         if pciReadConfig8 (dev.bus, dev.slot, dev.func, at_offset) = ID then
            return at_offset;
         end if;
         at_offset := pciReadConfig8 (dev.bus, dev.slot, dev.func, at_offset + 1) and
           PCI_CAP_POINTER_MASK;
      end loop;
      return 0;
   end Find_Capability;

   --  A memory BAR's physical base and size, or zero bytes.
   procedure Memory_BAR
     (dev : PCIDeviceInfo; index : Natural; base, bytes : out Unsigned_64)
   is
      register : constant Unsigned_8 :=
        PCI_BASEADDR_0 + Unsigned_8 (index) * PCI_BAR_BYTES;
      low : constant Unsigned_32 :=
        pciReadConfig32 (dev.bus, dev.slot, dev.func, register);
      wide : constant Boolean := (low and PCI_BAR_TYPE_MASK) = PCI_BAR_TYPE_64;
   begin
      base := 0;
      bytes := 0;
      if index >= PCI_Message_Interrupts.BAR_COUNT or else
        (low and PCI_BAR_IO) /= 0 or else
        (wide and then index = PCI_Message_Interrupts.BAR_COUNT - 1)
      then
         return;
      end if;
      base := Unsigned_64 (low and PCI_BAR_ADDRESS_MASK);
      if wide then
         base := base or Shift_Left
           (Unsigned_64 (pciReadConfig32 (dev.bus, dev.slot, dev.func,
                                          register + PCI_BAR_BYTES)), 32);
      end if;
      bytes := probeMemoryBARSize (dev, register);
      if base = 0 or else base > Unsigned_64'Last - bytes then
         bytes := 0;
      end if;
   end Memory_BAR;

   --  Whether a table or PBA of length bytes at locator lies in a memory BAR.
   function Locates (dev : PCIDeviceInfo; locator : Unsigned_32; length : Unsigned_64;
                     address : out Unsigned_64) return Boolean
   is
      base, bytes : Unsigned_64;
      offset : constant Unsigned_64 := PCI_Message_Interrupts.Locator_Offset (locator);
   begin
      Memory_BAR (dev, PCI_Message_Interrupts.Locator_BAR (locator), base, bytes);
      address := base + offset;
      return bytes /= 0 and then length <= bytes and then offset <= bytes - length;
   end Locates;

   procedure Disable_INTx (dev : PCIDeviceInfo) is
   begin
      pciWriteConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND,
        pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND) or
          PCI_COMMAND_INTX_DISABLE);
   end Disable_INTx;

   procedure Report (F : Message_Function; Name : String) is
   begin
      if F.Mode = Polled then
         debugPrint ("devmgr: " & Name & " interrupts polled (no message vector)" & LF);
      else
         debugPrint ("devmgr: " & Name & " " &
           (if F.Mode = MSIX_Mode then "MSI-X" else "MSI") & " vector" &
           Natural'Image (F.Vectors.Head) &
           (if F.Vectors.Count > 1 then " +" & Natural'Image (F.Vectors.Count - 1)
            else "") &
           " -> APIC" & Natural'Image (F.Vectors.Destination) & LF);
      end if;
   end Report;

   ---------------------------------------------------------------------------
   --  Setup_Message_Interrupts
   --  Allocate and program dev's vectors for owner, as its manifest asks:
   --  MSI-X when the function has it, else one MSI. The function stays
   --  masked (MSI-X) until Open_Message_Interrupts. Enable_MSI False leaves
   --  MSI programmed but off, for a driver that enables it later.
   ---------------------------------------------------------------------------
   --  The block's interrupt capability, which the kernel placed in the
   --  owner when it allocated the block, answers the owner's interrupt
   --  binding.
   procedure Bind_Vectors (F : Message_Function) is
   begin
      if not DL.Give_First_Placed (F.Owner, ST.Interrupt, F.Vectors.Owner_Handle) then
         debugPrint ("devmgr: manifest declares no interrupt binding" & LF);
      end if;
   end Bind_Vectors;

   procedure Setup_Message_Interrupts
     (dev        : PCIDeviceInfo;
      owner      : DL.Child;
      name       : String;
      F          : out Message_Function;
      Enable_MSI : Boolean := True)
   is
      use PCI_Message_Interrupts;
      request : constant Driver_Manifests.Interrupt_Request :=
        DL.Interrupt_Of (owner);
      msix : constant Unsigned_8 := Find_Capability (dev, MSIX_CAPABILITY_ID);
      msi  : constant Unsigned_8 := Find_Capability (dev, MSI_CAPABILITY_ID);
      cpu  : constant Natural := cpuOf (DL.PID (owner));
   begin
      F := (Device => dev, Owner => owner, others => <>);
      if not request.Present then
         debugPrint ("devmgr: " & name & " manifest asks for no message interrupt" & LF);
         return;
      end if;

      if msix /= 0 and then request.Mode in CCL.Resource_Sections.MSI_X and then
        nextTableWindow <= Table_Window'Last
      then
         declare
            control : constant Unsigned_16 :=
              pciReadConfig16 (dev.bus, dev.slot, dev.func, msix + MSIX_CONTROL_OFFSET);
            count : constant Device_Vector_ABI.Block_Size :=
              Natural'Min (request.Vectors, Table_Size (control));
            table_locator : constant Unsigned_32 :=
              pciReadConfig32 (dev.bus, dev.slot, dev.func, msix + MSIX_TABLE_OFFSET);
            pba_locator : constant Unsigned_32 :=
              pciReadConfig32 (dev.bus, dev.slot, dev.func, msix + MSIX_PBA_OFFSET);
            --  One pending bit per entry, in QWORDs.
            pba_bytes : constant Unsigned_64 :=
              Unsigned_64 ((Table_Size (control) + 63) / 64 * 8);
            table, pba, page, window : Unsigned_64;
            pages : Unsigned_64;
         begin
            if not Locates (dev, table_locator,
                            Unsigned_64 (Table_Size (control)) * ENTRY_BYTES, table)
              or else not Locates (dev, pba_locator, pba_bytes, pba)
            then
               debugPrint ("devmgr: " & name & " MSI-X table or PBA outside its BAR" & LF);
            else
               page := table - table mod PAGE_BYTES;
               pages := (table mod PAGE_BYTES + Unsigned_64 (count) * ENTRY_BYTES +
                         PAGE_BYTES - 1) / PAGE_BYTES;
               window := MSIX_TABLE_WINDOW_BASE +
                 Unsigned_64 (nextTableWindow) * MSIX_TABLE_WINDOW_BYTES;
               if mapInto (myPID, page, window, pages, MAP_INTO_DEVICE, Root.Process_Root,
                           Devmgr_Authority.Device_Region (page, pages * PAGE_BYTES)) = reterr
               then
                  Warnings.Warn ("devmgr: " & name & " MSI-X table map refused " &
                    Hex (page) & " +" & Hex (pages * PAGE_BYTES));
               else
                  nextTableWindow := nextTableWindow + 1;
                  F.Vectors := CuBit.Device_Interrupts.Allocate
                    (DL.PID (owner), DL.Space (owner), count, cpu, Root.Vector_Authority);
                  if F.Vectors.Valid then
                     Bind_Vectors (F);
                     F.Mode := MSIX_Mode;
                     F.Capability := msix;
                     F.Table := window + table mod PAGE_BYTES;
                     current := F;
                     Message_Functions.Program_MSIX
                       (F.Vectors.Head, F.Vectors.Count, F.Vectors.Destination);
                     Disable_INTx (dev);
                  end if;
               end if;
            end if;
         end;
      end if;

      if F.Mode = Polled and then msi /= 0 then
         F.Vectors := CuBit.Device_Interrupts.Allocate
           (DL.PID (owner), DL.Space (owner), 1, cpu, Root.Vector_Authority);
         if F.Vectors.Valid then
            Bind_Vectors (F);
            F.Mode := MSI_Mode;
            F.Capability := msi;
            current := F;
            if Enable_MSI then
               Message_Functions.Program_MSI (F.Vectors.Head, F.Vectors.Destination);
               Disable_INTx (dev);
            else
               Message_Functions.Prepare_MSI (F.Vectors.Head, F.Vectors.Destination);
            end if;
         end if;
      end if;
      Report (F, name);
   end Setup_Message_Interrupts;

   --  The driver is ready: unmask its entries and the function. Messages
   --  raised while masked were held pending and are sent now.
   procedure Open_Message_Interrupts (F : Message_Function) is
   begin
      if F.Mode = MSIX_Mode then
         current := F;
         Message_Functions.Open_MSIX (F.Vectors.Count);
      end if;
   end Open_Message_Interrupts;

   --  Mask and disable the function, then release its vectors.
   procedure Close_Message_Interrupts (F : in out Message_Function) is
   begin
      current := F;
      case F.Mode is
         when MSIX_Mode => Message_Functions.Close_MSIX (F.Vectors.Count);
         when MSI_Mode  => Message_Functions.Close_MSI;
         when Polled    => return;
      end case;
      if not CuBit.Device_Interrupts.Release (F.Vectors.Head, Root.Vector_Authority) then
         debugPrint ("devmgr: device vector release refused" & LF);
      end if;
      F.Mode := Polled;
   end Close_Message_Interrupts;

   nvmeInterrupts, netInterrupts, hdaInterrupts, gpuInterrupts,
     xhciInterrupts, intelInterrupts : Message_Function;
   netInterruptSets : array (Net_Device_Index) of Message_Function;

   --  Which of the virtio-net functions Location is (0: none).
   function Net_Device_At (Location : PCIDeviceInfo) return Net_Device_Count is
   begin
      for K in 1 .. netDevCount loop
         if Same_Location (Location, netDevs (K)) then
            return K;
         end if;
      end loop;
      return 0;
   end Net_Device_At;

   NVME_REGISTER_BYTES : constant Unsigned_64 := 16#4000#;
   HDA_REGISTER_BYTES  : constant Unsigned_64 := 16#4000#;

   --  Answer a device-memory binding of child (Name, or its first one)
   --  with [Physical, Physical + Bytes) of the device-memory root covering
   --  it, with Kept rights.
   procedure Give_Registers
     (child : DL.Child; what : String; Physical, Bytes : Unsigned_64;
      Kept : H.Rights; Name : String := "")
   is
   begin
      if not Devmgr_Authority.Covered (Physical, Bytes) then
         Warnings.Warn ("devmgr: " & what & " registers outside device memory " &
           Hex (Physical) & " +" & Hex (Bytes));
      elsif Name'Length > 0 then
         DL.Give (child, Name, Devmgr_Authority.Device_Copy (Physical, Bytes, Kept));
      elsif not DL.Give_First
        (child, ST.Device_Memory, Devmgr_Authority.Device_Copy (Physical, Bytes, Kept))
      then
         debugPrint ("devmgr: " & what & " manifest declares no device memory" & LF);
      end if;
   end Give_Registers;

   --  Answer an interrupt binding of child with a legacy line (vector) and
   --  route the line to it.
   procedure Give_Line (child : DL.Child; Name : String; Vector : Unsigned_64;
                        Level_Triggered, Active_Low : Boolean := False) is
      ignore : Unsigned_64;
      Copy : constant H.Derivation :=
        H.Narrowed (Root.Legacy_Interrupts, H.Read_Only, Vector, 1);
   begin
      if Name'Length > 0 then
         DL.Give (child, Name, Copy);
      elsif not DL.Give_First (child, ST.Interrupt, Copy) then
         debugPrint ("devmgr: manifest declares no interrupt binding" & LF);
      end if;
      ignore := enableIrq (Vector, DL.PID (child), 0, DL.Control (child),
                           Root.Legacy_Interrupts, Level_Triggered, Active_Low);
   end Give_Line;

   --  Answer an I/O port binding of child.
   procedure Give_Ports (child : DL.Child; Name : String; First, Count : Unsigned_64) is
   begin
      DL.Give (child, Name, H.Narrowed (Root.IO_Ports_Root, H.Read_Write, First, Count));
   end Give_Ports;

   ---------------------------------------------------------------------------
   -- Setup NVMe driver: PCI config, DMA, capabilities
   ---------------------------------------------------------------------------
   procedure setupNvme is
      bar0Lo    : Unsigned_32;
      bar0Hi    : Unsigned_32;
      bar0Phys  : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;

      DMA_ORDER : constant Unsigned_64 := 8;  --  256 pages = 1 MiB
   begin
      if not nvmeDev.found or nvmePID = No_Process then
         return;
      end if;

      --  Read BAR0 (64-bit MMIO)
      bar0Lo := pciReadConfig32 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_BASEADDR_0);
      bar0Hi := pciReadConfig32 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_BASEADDR_1);
      bar0Phys := Shift_Left (Unsigned_64 (bar0Hi), 32) or
                  Unsigned_64 (bar0Lo and 16#FFFF_FFF0#);

      --  Enable bus master + memory space
      pciCmd := pciReadConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (nvmeDev.bus, nvmeDev.slot, nvmeDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);

      --  Completion interrupts by MSI-X, programmed here and left masked
      --  until the driver has created its queues; otherwise it polls.
      Setup_Message_Interrupts (nvmeDev, nvmeChild, "nvme.drv", nvmeInterrupts);

      --  Allocate DMA
      dmaPhys := allocDma (nvmePID, DMA_ORDER, DMA_VIRT_BASE, DL.Control (nvmeChild));
      if dmaPhys = reterr then
         debugPrint ("devmgr: NVMe DMA alloc failed" & LF);
         return;
      end if;

      --  Its register window (16 KiB of BAR0), as its manifest declares.
      Give_Registers (nvmeChild, "nvme.drv", bar0Phys, NVME_REGISTER_BYTES, H.Read_Write);

      --  BAR0 and the DMA region, for the driver alone.
      DL.Set_Payload
        (nvmeChild, LC.Encode (LC.Controller_Configuration'
           (Registers => bar0Phys, DMA => dmaPhys), LC.NVMe_Controller));

      debugPrint ("devmgr: NVMe setup complete" & LF);
   end setupNvme;

   ---------------------------------------------------------------------------
   -- Setup ATA driver: grant IOPORT + IRQ capabilities
   ---------------------------------------------------------------------------
   procedure setupAta is
      IDE1_VECTOR       : constant Unsigned_64 := 46;
      ATA_COMMAND_BLOCK : constant Unsigned_64 := 16#1F0#;
      ATA_COMMAND_PORTS : constant Unsigned_64 := 8;
      ATA_CONTROL       : constant Unsigned_64 := 16#3F6#;
   begin
      if ataPID = No_Process then
         return;
      end if;

      --  The primary channel's command block and control port, and its
      --  line (IDE1), as its manifest's platform resources declare.
      Give_Ports (ataChild, "command-block", ATA_COMMAND_BLOCK, ATA_COMMAND_PORTS);
      Give_Ports (ataChild, "control", ATA_CONTROL, 1);
      Give_Line (ataChild, "channel", IDE1_VECTOR);

      debugPrint ("devmgr: ATA setup complete" & LF);
   end setupAta;

   ---------------------------------------------------------------------------
   --  findModernNet - the virtio-net device's modern (1.0) register layout,
   --  if it has one in a single memory BAR (virtio 1.0 4.1.4).
   ---------------------------------------------------------------------------
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
      VIRTIO_NET_LEGACY_PORTS : constant Unsigned_64 := 32;
   begin
      if not netDev.found or virtioNetPID = No_Process then
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
      --  route. Require MSI-X, left masked until the driver has set its
      --  queue vectors. No periodic fallback.
      Setup_Message_Interrupts (netDev, virtioNetChild, "virtio-net.drv", netInterrupts);
      if netInterrupts.Mode /= MSIX_Mode then
         debugPrint ("devmgr: virtio-net requires MSI-X" & LF);
         Close_Message_Interrupts (netInterrupts);
         return;
      end if;

      --  Enable I/O + MMIO + bus master; disable legacy INTx output.
      pciCmd := pciReadConfig16 (netDev.bus, netDev.slot, netDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (netDev.bus, netDev.slot, netDev.func,
                        PCI_COMMAND, pciCmd or 16#0407#);

      --  Allocate DMA
      dmaPhys := allocDma (virtioNetPID, DMA_ORDER, DMA_VIRT_BASE, DL.Control (virtioNetChild));
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-net DMA alloc failed" & LF);
         return;
      end if;

      if netModern then
         --  The register BAR, mapped where the driver expects it.
         ret := mapInto (virtioNetPID, netModernBar,
           CuBit.Virtio_Net_Control.Modern_Virtual_Address,
           netModernBytes / 4096, MAP_INTO_DEVICE, DL.Control (virtioNetChild),
           Devmgr_Authority.Device_Region (netModernBar, netModernBytes));
         if ret = reterr then
            Warnings.Warn ("devmgr: virtio-net register BAR map refused " &
              Hex (netModernBar) & " +" & Hex (netModernBytes));
            return;
         end if;
         debugPrint ("devmgr: virtio-net modern (virtio 1.0) transport" & LF);
         DL.Withhold (virtioNetChild, "registers");
      else
         --  Its legacy register ports (BAR0), as its manifest declares.
         Give_Ports (virtioNetChild, "registers", bar0Base, VIRTIO_NET_LEGACY_PORTS);
      end if;

      --  Its DMA region's device address and its location (which names its
      --  interface), for the driver alone.
      DL.Set_Payload
        (virtioNetChild, LC.Encode (LC.Network_Device_Configuration'
           (DMA => dmaPhys,
            Location => Shift_Left (Unsigned_64 (netDev.bus), 8) or
                        Shift_Left (Unsigned_64 (netDev.slot), 3) or Unsigned_64 (netDev.func))));
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
      msiReady  : Boolean := False;

      DMA_ORDER : constant Unsigned_64 := 5;   --  32 pages = 128KB
   begin
      if not hdaDev.found or hdaPID = No_Process then
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
      irqVector := LEGACY_VECTOR_BASE + Unsigned_64 (irqLine);

      --  Enable bus master + memory space
      pciCmd := pciReadConfig16 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (hdaDev.bus, hdaDev.slot, hdaDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);

      --  Prefer an allocated MSI vector. Besides avoiding legacy PCI IRQ
      --  sharing, MSI is edge-triggered and therefore fits a userspace driver:
      --  the kernel may EOI immediately without a level line retriggering
      --  before HDA has cleared its stream status.
      Setup_Message_Interrupts (hdaDev, hdaChild, "hda.drv", hdaInterrupts);
      msiReady := hdaInterrupts.Mode /= Polled;

      if not msiReady then
         --  Compatibility fallback. The long-term legacy-INTx path needs a
         --  mask/ack/unmask protocol before it can offer the same latency.
         --  The line's capability answers the manifest's interrupt binding.
         Give_Line (hdaChild, "", irqVector);
         debugPrint ("devmgr: HDA using legacy INTx fallback" & LF);
      end if;

      --  Allocate DMA
      dmaPhys := allocDma (hdaPID, DMA_ORDER, DMA_VIRT_BASE, DL.Control (hdaChild));
      if dmaPhys = reterr then
         debugPrint ("devmgr: HDA DMA alloc failed" & LF);
         return;
      end if;

      --  Its register window (16 KiB of BAR0), as its manifest declares.
      Give_Registers (hdaChild, "hda.drv", bar0Phys, HDA_REGISTER_BYTES, H.Read_Write);

      --  BAR0 and the DMA region, for the driver alone.
      DL.Set_Payload
        (hdaChild, LC.Encode (LC.Controller_Configuration'
           (Registers => bar0Phys, DMA => dmaPhys), LC.HDA_Controller));

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
      barBytes : Unsigned_64;
      commonBytes, notifyBytes, isrBytes, deviceBytes : Unsigned_32 := 0;
      capBytes : Unsigned_8;
      capCount : Natural := 0;
      irqLine : Unsigned_8;
      irqVector : Unsigned_64;
      pciCmd : Unsigned_16;
      dmaPhys : Unsigned_64;
      ret : Unsigned_64;

      -- Two independently owned banks, one per head, within the allocator's
      -- supported block size. A 16 MiB bank holds two fixed scanout buffers
      -- of up to 1920x1080, so a mode change (a resized VM window) never
      -- reallocates; a machine without that much contiguous memory gets
      -- 8 MiB banks (up to 1280x800). Both banks have one size, which
      -- virtio-gpu reads from its launch configuration. A missing second
      -- bank must not prevent the boot output.
      secondDmaPhys : Unsigned_64;
      LARGE_DMA_ORDER : constant Unsigned_64 := 12;
      SMALL_DMA_ORDER : constant Unsigned_64 := 11;
      --  32 MiB of large banks is a desktop VM's budget, not a small VM's:
      --  below this much RAM (the kernel's system facts) the banks stay
      --  8 MiB, as before resizable modes, so 128 MiB guests keep their
      --  memory.
      LARGE_BANK_MINIMUM_MEMORY : constant Unsigned_64 := 1024 * 1024 * 1024;
      DMA_ORDER : Unsigned_64 :=
        (if CuBit.System_Facts.Memory_Bytes >= LARGE_BANK_MINIMUM_MEMORY
         then LARGE_DMA_ORDER else SMALL_DMA_ORDER);
      DMA_SIZE  : Unsigned_64;
      function Fits (Offset, Bytes, Limit : Unsigned_64) return Boolean is
        (Bytes /= 0 and then Offset <= Limit and then Bytes <= Limit - Offset);

      function readCap32 (offset : Unsigned_8) return Unsigned_32 is
      begin
         return pciReadConfig32 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 capPtr + offset);
      end readCap32;
   begin
      if not gpuDev.found or virtioGpuPID = No_Process then
         return;
      end if;

      capPtr := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                PCI_CAP_PTR) and 16#FC#;
      while capPtr /= 0 loop
         -- Conventional PCI capability space is finite. Reject cycles and
         -- vendor headers whose fields would wrap the 8-bit config offset.
         if capPtr < 64 or else capCount = 48 then return; end if;
         capCount := capCount + 1;
         capId := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                  capPtr);
         next := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 capPtr + 1) and 16#FC#;

         if capId = PCI_CAP_ID_VENDOR_SPECIFIC then
            if capPtr > 240 then return; end if;
            capBytes := pciReadConfig8
              (gpuDev.bus, gpuDev.slot, gpuDev.func, capPtr + 2);
            if capBytes < 16 or else Natural (capPtr) + Natural (capBytes) > 256
            then return; end if;
            cfgType := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                       capPtr + 3);
            barIndex := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                        capPtr + 4);
            capOff := readCap32 (8);

            case cfgType is
               when VIRTIO_PCI_CAP_COMMON_CFG =>
                  if commonBar /= 16#FF# then return; end if;
                  commonBar := barIndex;
                  commonOff := capOff;
                  commonBytes := readCap32 (12);
               when VIRTIO_PCI_CAP_NOTIFY_CFG =>
                  if notifyBar /= 16#FF# or else capBytes < 20 then return; end if;
                  notifyBar := barIndex;
                  notifyOff := capOff;
                  notifyBytes := readCap32 (12);
                  notifyMult := readCap32 (16);
               when VIRTIO_PCI_CAP_ISR_CFG =>
                  if isrBar /= 16#FF# then return; end if;
                  isrBar := barIndex;
                  isrOff := capOff;
                  isrBytes := readCap32 (12);
               when VIRTIO_PCI_CAP_DEVICE_CFG =>
                  if deviceBar /= 16#FF# then return; end if;
                  deviceBar := barIndex;
                  deviceOff := capOff;
                  deviceBytes := readCap32 (12);
               when others =>
                  null;
            end case;
         end if;

         capPtr := next;
      end loop;

      if commonBar = 16#FF# or else notifyBar = 16#FF# or else
         isrBar = 16#FF#
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

      -- BEGIN VIRTIO GPU BAR DECODE (exact-source hosted regression).
      if commonBar > 5 then
         debugPrint ("devmgr: virtio-gpu invalid common BAR index" & LF);
         return;
      end if;
      barRaw := pciReadConfig32
        (gpuDev.bus, gpuDev.slot, gpuDev.func,
         PCI_BASEADDR_0 + commonBar * 4);
      if (barRaw and 1) /= 0 or else
        ((barRaw and 6) /= 0 and then (barRaw and 6) /= 4) or else
        (commonBar = 5 and then (barRaw and 6) = 4)
      then
         debugPrint ("devmgr: virtio-gpu unsupported common BAR encoding" & LF);
         return;
      end if;
      barPhys := Unsigned_64 (barRaw and 16#FFFF_FFF0#);
      if (barRaw and 6) = 4 then
         barPhys := barPhys or Shift_Left
           (Unsigned_64 (pciReadConfig32
              (gpuDev.bus, gpuDev.slot, gpuDev.func,
               PCI_BASEADDR_0 + commonBar * 4 + 4)), 32);
      end if;
      if barPhys = 0 then
         debugPrint ("devmgr: virtio-gpu unassigned common BAR" & LF);
         return;
      end if;
      -- END VIRTIO GPU BAR DECODE.

      barBytes := probeMemoryBARSize
        (gpuDev, PCI_BASEADDR_0 + commonBar * 4);
      if barBytes = 0 or else barPhys mod 4096 /= 0 or else
        barPhys > Unsigned_64'Last - barBytes or else
        commonBytes < 56 or else notifyBytes < 2 or else isrBytes < 1 or else
        commonOff mod 4 /= 0 or else notifyOff mod 2 /= 0 or else
        not Fits (Unsigned_64 (commonOff), Unsigned_64 (commonBytes), barBytes) or else
        not Fits (Unsigned_64 (notifyOff), Unsigned_64 (notifyBytes), barBytes) or else
        not Fits (Unsigned_64 (isrOff), Unsigned_64 (isrBytes), barBytes) or else
        (deviceBar /= 16#FF# and then
         not Fits (Unsigned_64 (deviceOff), Unsigned_64 (deviceBytes), barBytes))
      then
         debugPrint ("devmgr: virtio-gpu invalid MMIO extent" & LF);
         return;
      end if;

      pciCmd := pciReadConfig16 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                        PCI_COMMAND, pciCmd or 16#0006#);
      --  MSI-X entries for the queues and configuration changes. Opened
      --  at once: the device signals nothing until the driver sets its
      --  queue vectors, and the doorbell latches anything earlier.
      Setup_Message_Interrupts (gpuDev, virtioGpuChild, "virtio-gpu.drv", gpuInterrupts);
      if gpuInterrupts.Mode = MSIX_Mode then
         Open_Message_Interrupts (gpuInterrupts);
      else
         --  No MSI-X: the legacy line, acknowledged through the ISR register.
         Close_Message_Interrupts (gpuInterrupts);
         irqLine := pciReadConfig8 (gpuDev.bus, gpuDev.slot, gpuDev.func,
                                    PCI_INTERRUPT_LINE);
         irqVector := LEGACY_VECTOR_BASE + Unsigned_64 (irqLine);
         Give_Line (virtioGpuChild, "", irqVector);
      end if;

      dmaPhys := allocDma (virtioGpuPID, DMA_ORDER, DMA_VIRT_BASE, DL.Control (virtioGpuChild));
      if dmaPhys = reterr and then DMA_ORDER /= SMALL_DMA_ORDER then
         DMA_ORDER := SMALL_DMA_ORDER;
         dmaPhys := allocDma (virtioGpuPID, DMA_ORDER, DMA_VIRT_BASE, DL.Control (virtioGpuChild));
      end if;
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-gpu DMA alloc failed" & LF);
         return;
      end if;
      DMA_SIZE := 2 ** Natural (DMA_ORDER) * 4096;

      secondDmaPhys := allocDma
        (virtioGpuPID, DMA_ORDER, DMA_VIRT_BASE + DMA_SIZE, DL.Control (virtioGpuChild));
      if secondDmaPhys = reterr then
         secondDmaPhys := 0;
         debugPrint ("devmgr: second GPU DMA bank unavailable" & LF);
      end if;

      --  Its modern MMIO BAR ("registers"), and the exact notification
      --  subregion ("notify", write only): inspection gives the driver the
      --  window without a new ABI, and it never widens the BAR authority.
      Give_Registers (virtioGpuChild, "virtio-gpu.drv", barPhys, barBytes, H.Read_Write,
                      Name => "registers");
      Give_Registers (virtioGpuChild, "virtio-gpu.drv",
                      barPhys + Unsigned_64 (notifyOff), Unsigned_64 (notifyBytes),
                      H.Write_Only, Name => "notify");

      --  The transport and its DMA banks, for the driver alone.
      DL.Set_Payload
        (virtioGpuChild, LC.Encode (LC.Virtio_GPU_Configuration'
           (Registers         => barPhys,
            DMA               => dmaPhys,
            Second_DMA        => secondDmaPhys,
            DMA_Bytes         => (if secondDmaPhys = 0 then DMA_SIZE else DMA_SIZE * 2),
            Common_Offset     => Unsigned_64 (commonOff),
            Notify_Offset     => Unsigned_64 (notifyOff),
            ISR_Offset        => Unsigned_64 (isrOff),
            Device_Offset     => Unsigned_64 (deviceOff),
            Notify_Multiplier => Unsigned_64 (notifyMult),
            Boot_Display      => gpuIsPrimary)));
      --  The boot-adapter handoff precedes starting its native driver: the
      --  kernel retires its firmware renderer now, and display.svc reads
      --  the handoff from the system facts publication.
      if gpuIsPrimary and then not CuBit.System_Facts.Publish
        (CuBit.System_Facts.SP.Boot_Display_Native, 1)
      then
         debugPrint ("devmgr: boot display handoff refused" & LF);
      end if;

      debugPrint ("devmgr: virtio-gpu setup complete" & LF);
   end setupVirtioGpu;

   ---------------------------------------------------------------------------
   --  Setup the virtio-input driver (absolute pointers: QEMU virtio-tablet).
   --  Modern transport only (virtio-input has no legacy interface): the
   --  common, notify and device configuration in one memory BAR, mapped
   --  where the driver expects it; MSI-X entry 0 on an allocated vector
   --  (Setup_Message_Interrupts), masked until the driver's Configure reply.
   ---------------------------------------------------------------------------
   inputCommonOff, inputDeviceOff, inputNotifyOff, inputNotifyMult : Unsigned_64 := 0;
   inputModernBytes : Unsigned_64 := 0;
   inputTransportReady : Boolean := False;
   inputInterrupts : Message_Function;

   procedure setupVirtioInput is
      package VIC renames CuBit.Virtio_Input_Control;
      NONE : constant Unsigned_8 := 16#FF#;
      dev : constant PCIDeviceInfo := inputDev;
      capPtr, capId, next, cfgType, barIndex : Unsigned_8;
      capOff : Unsigned_32;
      commonBar, notifyBar, deviceBar : Unsigned_8 := NONE;
      lo, hi : Unsigned_32;
      bar : Unsigned_8;
      modernBar, size, dmaPhys, ret : Unsigned_64;
      pciCmd : Unsigned_16;
      MEMORY_SPACE : constant Unsigned_16 := 16#0002#;
      BUS_MASTER   : constant Unsigned_16 := 16#0004#;
      INTX_DISABLE : constant Unsigned_16 := 16#0400#;
      PCI_STATUS_CAPABILITIES : constant Unsigned_16 := 16#0010#;
   begin
      inputTransportReady := False;
      if not dev.found or else virtioInputPID = No_Process then
         return;
      end if;
      if (pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_STATUS) and
          PCI_STATUS_CAPABILITIES) = 0
      then
         debugPrint ("devmgr: virtio-input has no capability list" & LF);
         return;
      end if;
      capPtr := pciReadConfig8 (dev.bus, dev.slot, dev.func, PCI_CAP_PTR) and 16#FC#;
      for hop in 1 .. 48 loop
         exit when capPtr < 16#40# or else capPtr > 16#F0#;
         capId := pciReadConfig8 (dev.bus, dev.slot, dev.func, capPtr);
         next := pciReadConfig8 (dev.bus, dev.slot, dev.func, capPtr + 1) and 16#FC#;
         if capId = PCI_CAP_ID_VENDOR_SPECIFIC then
            cfgType := pciReadConfig8 (dev.bus, dev.slot, dev.func, capPtr + 3);
            barIndex := pciReadConfig8 (dev.bus, dev.slot, dev.func, capPtr + 4);
            capOff := pciReadConfig32 (dev.bus, dev.slot, dev.func, capPtr + 8);
            case cfgType is
               when VIRTIO_PCI_CAP_COMMON_CFG =>
                  commonBar := barIndex;
                  inputCommonOff := Unsigned_64 (capOff);
               when VIRTIO_PCI_CAP_NOTIFY_CFG =>
                  notifyBar := barIndex;
                  inputNotifyOff := Unsigned_64 (capOff);
                  inputNotifyMult := Unsigned_64
                    (pciReadConfig32 (dev.bus, dev.slot, dev.func, capPtr + 16));
               when VIRTIO_PCI_CAP_DEVICE_CFG =>
                  deviceBar := barIndex;
                  inputDeviceOff := Unsigned_64 (capOff);
               when others =>
                  null;
            end case;
         end if;
         capPtr := next;
      end loop;
      if commonBar > 5 or else notifyBar /= commonBar or else deviceBar /= commonBar then
         debugPrint ("devmgr: virtio-input lacks a single modern BAR" & LF);
         return;
      end if;

      --  The register BAR.
      bar := PCI_BASEADDR_0 + commonBar * 4;
      lo := pciReadConfig32 (dev.bus, dev.slot, dev.func, bar);
      if (lo and 1) /= 0 or else (commonBar = 5 and then (lo and 6) = 4) then
         return;   --  an I/O BAR, or a 64-bit BAR with no high half
      end if;
      modernBar := Unsigned_64 (lo and 16#FFFF_FFF0#);
      if (lo and 6) = 4 then
         hi := pciReadConfig32 (dev.bus, dev.slot, dev.func, bar + 4);
         modernBar := modernBar or Shift_Left (Unsigned_64 (hi), 32);
      end if;
      size := probeMemoryBARSize (dev, bar);
      if modernBar = 0 or else size < 4096 or else modernBar mod 4096 /= 0 then
         debugPrint ("devmgr: virtio-input register BAR unusable" & LF);
         return;
      end if;
      inputModernBytes := Unsigned_64'Min (size, VIC.Maximum_Modern_Bytes);
      inputModernBytes := inputModernBytes - inputModernBytes mod 4096;

      --  MSI-X entry 0, masked until the driver has set its queue vector.
      Setup_Message_Interrupts (dev, virtioInputChild, "virtio-input.drv", inputInterrupts);
      if inputInterrupts.Mode /= MSIX_Mode then
         debugPrint ("devmgr: virtio-input requires MSI-X" & LF);
         Close_Message_Interrupts (inputInterrupts);
         return;
      end if;

      pciCmd := pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND);
      pciWriteConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND,
        pciCmd or MEMORY_SPACE or BUS_MASTER or INTX_DISABLE);

      dmaPhys := allocDma (virtioInputPID, 0, DMA_VIRT_BASE, DL.Control (virtioInputChild));
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-input DMA alloc failed" & LF);
         return;
      end if;
      ret := mapInto (virtioInputPID, modernBar, VIC.Modern_Virtual_Address,
        inputModernBytes / 4096, MAP_INTO_DEVICE, DL.Control (virtioInputChild),
        Devmgr_Authority.Device_Region (modernBar, inputModernBytes));
      if ret = reterr then
         Warnings.Warn ("devmgr: virtio-input register BAR map refused " &
           Hex (modernBar) & " +" & Hex (inputModernBytes));
         return;
      end if;

      --  Its register BAR, as its manifest declares (CAP-001: device
      --  authority names device pages only); its publication to the
      --  registered mouse consumer is its manifest's notification. The
      --  interrupt capability is the kernel's, placed by allocation. Its DMA
      --  page's device address is in its launch configuration.
      DL.Set_Payload
        (virtioInputChild, LC.Encode (LC.Queue_Configuration'(DMA => dmaPhys), LC.Virtio_Input));
      Give_Registers (virtioInputChild, "virtio-input.drv", modernBar, inputModernBytes,
                      H.Read_Write);
      inputTransportReady := True;
      debugPrint ("devmgr: virtio-input setup complete" & LF);
   end setupVirtioInput;

   ---------------------------------------------------------------------------
   -- Setup PS/2 driver: grant IOPORT + IRQ capabilities
   ---------------------------------------------------------------------------
   procedure setupPS2 is
      PS2_DATA       : constant Unsigned_64 := 16#60#;
      PS2_COMMAND    : constant Unsigned_64 := 16#64#;
      KEYBOARD_VECTOR : constant Unsigned_64 := 33;
      MOUSE_VECTOR    : constant Unsigned_64 := 44;
   begin
      if ps2PID = No_Process then
         return;
      end if;
      --  Its data and command ports and both lines, as its manifest's
      --  platform resources declare. IRQ ownership is not publication
      --  authority: its keyboard and mouse notifications (publish only)
      --  come from its manifest too.
      Give_Ports (ps2Child, "data", PS2_DATA, 1);
      Give_Ports (ps2Child, "command", PS2_COMMAND, 1);
      Give_Line (ps2Child, "keyboard-line", KEYBOARD_VECTOR);
      Give_Line (ps2Child, "mouse-line", MOUSE_VECTOR);
      debugPrint ("devmgr: PS/2 setup complete" & LF);
   end setupPS2;

   ---------------------------------------------------------------------------
   -- Probe a memory BAR's implemented size while its decode is disabled.
   -- Returns zero for an I/O BAR or a malformed/unsupported result.
   ---------------------------------------------------------------------------
   function probeMemoryBARSize
     (dev : PCIDeviceInfo; bar : Unsigned_8; Limit : Unsigned_64 := XHCI_BAR_LIMIT)
      return Unsigned_64
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

      if size < 4096 or else size > Limit or else
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
      interruptMode : XHCI_Interrupt_Mode := XHCI_INTERRUPT_POLLING;
      irqRet    : Unsigned_64;

      scratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      dmaLayout : XHCI_DMA_Layout.Allocation;
      --  Bootstrap-only, uncached capability-register mapping. devmgr already
      --  owns physical mapping authority; it never touches rings or payloads.
      CAPABILITY_PROBE_BASE : constant Unsigned_64 := 16#0000_6000_1000_0000#;
      OP_XHCI_CONFIGURE : constant Unsigned_32 := 16#0220#;
   begin
      if not xhciDev.found or else xhciPID = No_Process then
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

      --  xHCI is a latency-sensitive producer: an allocated vector lets its
      --  driver block on event-ring changes instead of polling on the
      --  millisecond tick. MSI-X (QEMU's controller) stays masked until the
      --  driver has enabled its interrupter.
      assignCPU (xhciChild, "xhci.drv");
      Setup_Message_Interrupts (xhciDev, xhciChild, "xhci.drv", xhciInterrupts);
      interruptMode :=
        (case xhciInterrupts.Mode is
            when MSIX_Mode => XHCI_INTERRUPT_MSIX,
            when MSI_Mode  => XHCI_INTERRUPT_MSI,
            when Polled    => XHCI_INTERRUPT_POLLING);

      if interruptMode = XHCI_INTERRUPT_POLLING then
         debugPrint ("devmgr: xHCI using queued polling fallback" & LF);
      end if;

      irqRet := mapInto (myPID, bar0Phys, CAPABILITY_PROBE_BASE, 1, MAP_INTO_DEVICE,
                         Root.Process_Root, Devmgr_Authority.Device_Region (bar0Phys, PAGE_BYTES));
      if irqRet = reterr then
         Warnings.Warn ("devmgr: xHCI capability page map refused " &
           Hex (bar0Phys) & " +" & Hex (PAGE_BYTES));
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
      dmaPhys := allocDma (xhciPID, Unsigned_64 (dmaLayout.Order), DMA_VIRT_BASE,
                           DL.Control (xhciChild));
      if dmaPhys = reterr then
         debugPrint ("devmgr: xHCI DMA allocation failed" & LF);
         return;
      end if;

      --  Its register BAR, as its manifest declares; its mouse and
      --  keyboard publication (to the registered consumers only; never
      --  registering or replacing them) and its endpoint to devmgr come
      --  from its manifest too.
      Give_Registers (xhciChild, "xhci.drv", bar0Phys, barPages * PAGE_BYTES, H.Read_Write);
      if not DL.Start (xhciChild) then
         Close_Message_Interrupts (xhciInterrupts);
         xhciPID := No_Process;
         return;
      end if;

      cfgMsg :=
        (tag => (label => OP_XHCI_CONFIGURE,
                 length => 4, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words =>
           [0 => bar0Phys,
            --  Low 32 bits: BAR pages. High 32 bits: allocation's scratchpad
            --  count. Driver checks it against a fresh HCSPARAMS2 read before
            --  accessing DMA; this does not grant arbitrary allocation size.
            1 => barPages or Shift_Left (Unsigned_64 (scratchpads), 32),
            2 => dmaPhys,
            --  The interrupt mode alone: devmgr programmed the message.
            3 => Unsigned_64 (XHCI_Interrupt_Mode'Enum_Rep (interruptMode))]);
      replyTag := capCall (DL.Mailbox (xhciChild), cfgMsg, CuBit.Messages.Wait_Forever);
      if replyTag.label = REPLY_OK then
         --  The interrupter is enabled: unmask the entry and the function.
         Open_Message_Interrupts (xhciInterrupts);
         debugPrint ("devmgr: xHCI controller started" & LF);
         --  Read-only optical block endpoint; no raw controller authority.
         if filesystemPID /= No_Process then
            Optical_Storage_Ready :=
              Attach_Storage (Fs_Attachments.Optical_Device, xhciChild);
         end if;
      else
         debugPrint ("devmgr: xHCI controller rejected setup" & LF);
         Close_Message_Interrupts (xhciInterrupts);
         xhciPID := No_Process;
      end if;
   end setupXHCI;

   ---------------------------------------------------------------------------
   --  The generic PCI launch (HW-002, docs/device-matching.md "Generic PCI
   --  launch"): a driver bound by its declared match that devmgr has no
   --  dedicated setup for. Everything it gets comes from its manifest: one
   --  window per device-memory request (at most its BAR and its max_bytes),
   --  its MSI or MSI-X interrupt, its one DMA region and its endpoint to
   --  devmgr. Where they are reaches it as a Generic_PCI launch
   --  configuration; MSI-X stays masked until it reports ready.
   ---------------------------------------------------------------------------
   DMA_MAX_ORDER : constant := 14;   --  2 ** 14 pages: CCL.Resource_Sections.MAX_DMA_BYTES
   --  Generic drivers launched, awaiting or past their readiness report.
   Generic_Capacity : constant := 16;
   type Generic_Child is record
      PID        : Process_ID := No_Process;
      Interrupts : Message_Function;
      Device     : Inventory_Device;
      Waiting    : Boolean := False;
   end record;
   Generic_Children : array (1 .. Generic_Capacity) of Generic_Child;
   Generic_Total : Natural range 0 .. Generic_Capacity := 0;
   Generic_Started : array (Inventory_Index) of Boolean := [others => False];
   --  Each generic driver's log publications (CuBit.Log_Protocol), after
   --  devmgr's own drivers' issuances.
   Generic_Log_Issuance_First : constant CuBit.Log_Protocol.Publication_Issuance := 16;
   PCI_COMMAND_MEMORY_BUS_MASTER : constant Unsigned_16 := 16#0006#;

   procedure Launch_Generic_PCI (Item : Inventory_Device; Name : String) is
      package RS renames CCL.Resource_Sections;
      use type RS.Resource_Kind;
      use type RS.Basis_Kind;
      use type RS.Rights_Kind;
      dev : constant PCIDeviceInfo := Item.location;
      Child : DL.Child;
      PID : Process_ID;
      Plan : RS.Section_Plan;
      Config : LC.Generic_PCI_Configuration;
      Interrupts : Message_Function;
      Window : LC.Window_Count := 0;
      DMA_Requests : Natural := 0;
      Failed : Boolean := False;

      procedure Refuse (Why : String) is
      begin
         Warnings.Warn ("devmgr: " & Name & " at " & Location_Text (dev) & " not started: " & Why);
         Failed := True;
      end Refuse;
   begin
      Child := spawnFromBootStorage (Name, 5);
      if Child = DL.No_Child then
         Warnings.Warn ("devmgr: " & Name & " bound to " & Location_Text (dev) & " but not spawned");
         return;
      end if;
      PID := DL.PID (Child);
      Plan := DL.Resources_Of (Child);
      if Plan.Basis.Kind /= RS.PCI_Basis and then Plan.Count > 0 then
         Refuse ("its resources are not relative to a PCI function");
      end if;
      Config.Location := Shift_Left (Unsigned_64 (dev.bus), 8) or
        Shift_Left (Unsigned_64 (dev.slot), 3) or Unsigned_64 (dev.func);
      Config.Vendor := Unsigned_64 (Item.vendorID);
      Config.Device := Unsigned_64 (Item.deviceID);
      Config.Revision := Unsigned_64 (pciReadConfig8 (dev.bus, dev.slot, dev.func, 8));
      Config.Class_Code := Shift_Left (Unsigned_64 (Item.classCode), 8) or Unsigned_64 (Item.progIf);
      pciWriteConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND,
        pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND) or PCI_COMMAND_MEMORY_BUS_MASTER);
      assignCPU (Child, Name);
      if H.Valid (Logstore_Endpoint) then
         DL.Give_Service (Child, Unsigned_32 (DRIVER_LOGSTORE),
           H.Badged (Logstore_Endpoint, H.Read_Write,
             CuBit.Log_Protocol.Publisher_Tag
               (Driver_Log_Budget, Generic_Log_Issuance_First + Unsigned_64 (Generic_Total))));
      end if;

      for I in 1 .. Plan.Count loop
         exit when Failed;
         declare
            R : constant RS.Resource := Plan.Entries (I);
         begin
            case R.Kind is
               when RS.Device_Memory =>
                  if Window = LC.Maximum_Windows then
                     Refuse ("more device-memory requests than PCI BARs");
                  else
                     declare
                        Offset : constant Unsigned_8 := PCI_BASEADDR_0 + Unsigned_8 (R.Index) * PCI_BAR_BYTES;
                        Low : constant Unsigned_32 := pciReadConfig32 (dev.bus, dev.slot, dev.func, Offset);
                        High : constant Unsigned_32 :=
                          (if (Low and PCI_BAR_TYPE_MASK) = PCI_BAR_TYPE_64 and then R.Index < 5
                           then pciReadConfig32 (dev.bus, dev.slot, dev.func, Offset + PCI_BAR_BYTES) else 0);
                        Base : constant Unsigned_64 :=
                          Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low and PCI_BAR_ADDRESS_MASK);
                        Size : constant Unsigned_64 :=
                          probeMemoryBARSize (dev, Offset, RS.MAX_DEVICE_MEMORY_BYTES);
                        Bytes : constant Unsigned_64 := Unsigned_64'Min (Size, R.Amount);
                     begin
                        if (Low and PCI_BAR_IO) /= 0 or else Base = 0 or else Size = 0 then
                           Refuse ("BAR" & Unsigned_32'Image (R.Index) & " is not an assigned memory BAR");
                        elsif not Devmgr_Authority.Covered (Base, Bytes) then
                           Refuse ("BAR" & Unsigned_32'Image (R.Index) & " outside device memory");
                        else
                           Window := Window + 1;
                           Config.Windows (Window) := (First => Base, Bytes => Bytes);
                           if not DL.Give_Nth (Child, ST.Device_Memory, Window,
                             Devmgr_Authority.Device_Copy
                               (Base, Bytes, (if R.Rights = RS.Read_Only then H.Read_Only else H.Read_Write)))
                           then
                              Refuse ("its manifest has no device-memory binding" & Natural'Image (Window));
                           end if;
                        end if;
                     end;
                  end if;
               when RS.DMA =>
                  DMA_Requests := DMA_Requests + 1;
                  if DMA_Requests > 1 then
                     Refuse ("more than one dma request");
                  else
                     declare
                        Pages : constant Unsigned_64 := (R.Amount + PAGE_BYTES - 1) / PAGE_BYTES;
                        Order : Unsigned_64 := 0;
                        Physical : Unsigned_64;
                     begin
                        while Shift_Left (Unsigned_64'(1), Natural (Order)) < Pages
                          and then Order < DMA_MAX_ORDER
                        loop
                           Order := Order + 1;
                        end loop;
                        Physical := allocDma (PID, Order, DMA_VIRT_BASE, DL.Control (Child));
                        if Physical = reterr then
                           Refuse ("DMA allocation of" & Unsigned_64'Image (R.Amount) & " bytes failed");
                        else
                           Config.DMA := (First => Physical,
                                          Bytes => Shift_Left (Unsigned_64'(1), Natural (Order)) * PAGE_BYTES);
                           Config.DMA_Virtual := DMA_VIRT_BASE;
                        end if;
                     end;
                  end if;
               when RS.IO_Ports =>
                  Refuse ("I/O port BARs are not given by the generic launch");
               when RS.Interrupt | RS.Scheduling =>
                  null;
            end case;
         end;
      end loop;
      Config.Window_Total := Window;

      if not Failed then
         Setup_Message_Interrupts (dev, Child, Name, Interrupts);
         case Interrupts.Mode is
            when Polled    => Config.Delivery := LC.Polled; Config.Vectors := 0;
            when MSI_Mode  => Config.Delivery := LC.MSI;    Config.Vectors := 1;
            when MSIX_Mode =>
               Config.Delivery := LC.MSI_X;
               Config.Vectors := Unsigned_64 (Interrupts.Vectors.Count);
         end case;
         DL.Set_Payload (Child, LC.Encode (Config));
      end if;

      if Failed or else not DL.Start (Child) then
         if not Failed then
            Close_Message_Interrupts (Interrupts);
         end if;
         DL.Abandon (Child);
         return;
      end if;
      --  It reports OP_READY or OP_NOT_PRESENT to the service loop
      --  (Generic_Report), which then unmasks or releases its vectors.
      if Generic_Total < Generic_Capacity then
         Generic_Total := Generic_Total + 1;
         Generic_Children (Generic_Total) :=
           (PID => PID, Interrupts => Interrupts, Device => Item, Waiting => True);
      end if;
      Warnings.Inform ("devmgr: " & Name & " launched for " & Device_Text (Item) & " (generic launch)");
   end Launch_Generic_PCI;

   --  A generic driver's readiness report, in the service loop: True when
   --  From is one and the message was its report (answered here).
   function Generic_Report (From : Process_ID; Msg : Message) return Boolean is
      Ignore : Unsigned_64;
   begin
      for I in 1 .. Generic_Total loop
         if Generic_Children (I).Waiting and then Generic_Children (I).PID = From
           and then Msg.tag.label in OP_READY | OP_NOT_PRESENT
         then
            Generic_Children (I).Waiting := False;
            if Msg.tag.label = OP_READY then
               Open_Message_Interrupts (Generic_Children (I).Interrupts);
               Warnings.Inform ("devmgr: generic driver ready for " &
                                Device_Text (Generic_Children (I).Device));
            else
               Close_Message_Interrupts (Generic_Children (I).Interrupts);
               Warnings.Inform ("devmgr: generic driver declined " &
                                Device_Text (Generic_Children (I).Device));
            end if;
            Ignore := reply (From, NULL_MESSAGE);
            return True;
         end if;
      end loop;
      return False;
   end Generic_Report;

   --  Generic drivers start once logstore runs, so their bring-up lines
   --  reach it from their first one (the bring-up logging convention): a
   --  (service "logstore" ...) request is answered with a publisher
   --  endpoint badged for that driver.
   procedure Launch_Pending_Generic is
   begin
      for I in 1 .. inventoryCount loop
         if deviceBindings (I).Kind = DI.Bound and then not Generic_Started (I) then
            declare
               Name : constant String := Devmgr_Bindings.Name_Of (deviceBindings (I).Driver);
            begin
               if not Has_Driver_Setup (Name) then
                  Generic_Started (I) := True;
                  Launch_Generic_PCI (inventory (I), Name);
               end if;
            end;
         end if;
      end loop;
   end Launch_Pending_Generic;

   ---------------------------------------------------------------------------
   -- Main entry point
   ---------------------------------------------------------------------------

   initrdSize   : Unsigned_64;
   initrdPages  : Unsigned_64;
   ret          : Unsigned_64;
   msg          : Message;
   from         : Process_ID;

begin
   debugPrint ("devmgr: starting" & LF);
   --  The handles the kernel placed before devmgr ran (its launch
   --  configuration); without them devmgr can do nothing.
   Devmgr_Authority.Initialize;
   if not Devmgr_Authority.Valid then
      debugPrint ("devmgr: launch configuration carries no handles, halting" & LF);
      ret := syscall (SYSCALL_EXIT);
      return;
   end if;
   Root := Devmgr_Authority.Roots;
   DL.Configure (Root.Process_Root, Root.Notifications);
   declare
      Accepted : Boolean;
   begin
      Intel_Allocation_Growth.Configure
        (Intel_Allocations, Intel_Metadata_Quota, Intel_Record_Quota, Accepted);
      if not Accepted then
         debugPrint ("devmgr: Intel allocation metadata policy rejected" & LF);
      end if;
   end;

   --  The boot image the kernel mapped at INITRD_BASE, from the kernel's
   --  launch configuration for devmgr; the CPU count from the system facts.
   declare
      Configuration : LC.Device_Manager_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      initrdSize := (if Configured then Configuration.Boot_Image else 0);
      bootFramebuffer := (if Configured then Configuration.Framebuffer else (0, 0));
   end;
   numCPUs    := CuBit.System_Facts.Processor_Count;

   if initrdSize = 0 then
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
   ret := registerDriver (DRIVER_DEVMGR, Root.Registration);
   myPID := Registered_Driver (DRIVER_DEVMGR);
   --  Its children's (service "device-manager" ...) requests resolve to
   --  devmgr's own endpoint (a copy with the rights they asked for).
   DL.Provide (Unsigned_32 (DRIVER_DEVMGR), Root.Self_Grantable);

   --  A writable live image is served by a separate Block.Device.V1 driver.
   --  It starts before FS, so FS never calls a not-yet-running provider.
   if Cpio.findFile (cpioArchive, "live-rw.ext2") < cpioArchive.count then
      ramdiskChild := spawnFromBootStorage ("ramdisk.drv", 5);
      ramdiskPID := PID_Of (ramdiskChild);
      if ramdiskPID /= No_Process then
         initrdPages := (initrdSize + 4095) / 4096;
         declare
            Mapped : Unsigned_64 := 0;
            Count : Unsigned_64;
         begin
            while Mapped < initrdPages loop
               Count := Unsigned_64'Min
                 (MAX_MAP_INTO_PAGES_PER_CALL, initrdPages - Mapped);
               ret := mapInto
                 (ramdiskPID, Mapped * 4096,
                  INITRD_BASE + Mapped * 4096, Count, MAP_INTO_BOOT_IMAGE,
                  DL.Control (ramdiskChild), H.No_Device_Memory);
               exit when ret = reterr;
               Mapped := Mapped + Count;
            end loop;
            if Mapped /= initrdPages then
               debugPrint ("devmgr: RAM block-driver seed mapping failed" & LF);
               ret := syscall (SYSCALL_EXIT);
               return;
            end if;
         end;
         DL.Set_Payload
           (ramdiskChild, LC.Encode (LC.Boot_Image_Configuration'(Boot_Image => initrdSize)));
         assignCPU (ramdiskChild, "ramdisk.drv");
         if not DL.Start (ramdiskChild) or else not waitReady (ramdiskPID) then
            ret := syscall (SYSCALL_EXIT);
            return;
         end if;
      else
         debugPrint ("devmgr: RAM block driver unavailable" & LF);
         ret := syscall (SYSCALL_EXIT);
         return;
      end if;
   end if;

   --  Scan PCI bus for devices, then bind each to the driver that
   --  declares it (devices.index).
   scanPCI;
   Bind_Devices;

   -----------------------------------------------------------------------
   -- Phase 1: Spawn filesystem server (auto-assign PID)
   -----------------------------------------------------------------------
   filesystemChild := spawnFromBootStorage ("filesystem.svc", 5);
   filesystemPID := PID_Of (filesystemChild);
   if filesystemPID = No_Process then
      debugPrint ("devmgr: filesystem.svc spawn failed" & LF);
   end if;

   if filesystemPID /= No_Process then
      --  Map initrd into FS server for serving ramdisk files: the boot
      --  image by offset (the kernel knows where it is; we never do).
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
               mappedPages * 4096,
               INITRD_BASE + mappedPages * 4096,
               pageCount,
               MAP_INTO_BOOT_IMAGE,
               DL.Control (filesystemChild), H.No_Device_Memory);
            exit when ret = reterr;
            mappedPages := mappedPages + pageCount;
         end loop;
      end;
      if ret = reterr then
         Warnings.Warn ("devmgr: boot image map into the filesystem refused" &
           " (pages" & Unsigned_64'Image (initrdPages) & ")");
      end if;

      --  The boot image's size, for the filesystem alone (it is mapped at
      --  INITRD_BASE only when the mapping above succeeded).
      DL.Set_Payload
        (filesystemChild,
         LC.Encode (LC.Boot_Image_Configuration'
           (Boot_Image => (if ret = reterr then 0 else initrdSize))));

      --  The RAM block driver serving the writable live image.
      if ramdiskPID /= No_Process then
         DL.Give (filesystemChild, "ramdisk", H.Copy (DL.Mailbox (ramdiskChild), H.Read_Write));
      else
         DL.Withhold (filesystemChild, "ramdisk");
      end if;
      assignCPU (filesystemChild, "filesystem.svc");
      if DL.Start (filesystemChild) then
         debugPrint ("devmgr: filesystem server started" & LF);
         --  Wait for FS to signal ready; then devmgr's own endpoint to it
         --  (ACL management, boot images) is the one SPAWN gave it.
         if not waitReady (filesystemPID) then
            filesystemPID := No_Process;
         end if;
      else
         filesystemPID := No_Process;
      end if;

      if filesystemPID /= No_Process then
         Filesystem_Endpoint := DL.Mailbox (filesystemChild);
         DL.Provide (Unsigned_32 (DRIVER_FS), Filesystem_Endpoint);
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2: Spawn and setup disk drivers
   -----------------------------------------------------------------------

   --  ATA driver, for a present primary channel it declares.
   if ataPrimaryPresent and then Platform_Bound ("ata.drv", CCL.Resource_Sections.ATA_Primary) then
      ataChild := spawnFromBootStorage ("ata.drv", 5);
   end if;
   ataPID := PID_Of (ataChild);
   if ataPID /= No_Process then
      setupAta;
      assignCPU (ataChild, "ata.drv");
      if DL.Start (ataChild) then
         debugPrint ("devmgr: ATA driver started" & LF);
         if not waitReady (ataPID) then
            ataPID := No_Process;
         end if;
      else
         ataPID := No_Process;
      end if;

      --  Attach the ATA driver to the filesystem.
      if filesystemPID /= No_Process and ataPID /= No_Process then
         if not Attach_Storage (Fs_Attachments.ATA_Device, ataChild) then
            debugPrint ("devmgr: ATA not attached to the filesystem" & LF);
         end if;
      end if;
   end if;

   --  NVMe driver, for the controller bound to it.
   if nvmeDev.found then
      nvmeChild := spawnFromBootStorage ("nvme.drv", 5);
   end if;
   nvmePID := PID_Of (nvmeChild);
   if nvmePID /= No_Process then
      --  Placed first: its completion vectors go to the CPU it runs on.
      assignCPU (nvmeChild, "nvme.drv");
      setupNvme;
      if not DL.Start (nvmeChild) then
         Close_Message_Interrupts (nvmeInterrupts);
         nvmePID := No_Process;
      else
         declare
            use CuBit.NVMe_Control;
            cfg : Message :=
              (tag => (label => Operation'Enum_Rep
                         (if nvmeInterrupts.Mode /= Polled then Configure_Interrupts
                          else Configure_Polled),
                       length => 0, protocol => 0, ipc => <>, reserved => 0),
               authorityTag => 0, words => [others => 0]);
            response : MessageTag;
         begin
            response := capCall (DL.Mailbox (nvmeChild), cfg, CuBit.Messages.Wait_Forever);
            if response.label = REPLY_OK then
               --  The queues exist: unmask the entries and the function.
               Open_Message_Interrupts (nvmeInterrupts);
               if nvmeInterrupts.Mode /= Polled then
                  debugPrint ("devmgr: NVMe using " &
                    (if nvmeInterrupts.Mode = MSIX_Mode then "MSI-X" else "MSI") & LF);
               end if;
            else
               --  The driver polls: mask, disable and release the vectors.
               Close_Message_Interrupts (nvmeInterrupts);
            end if;
         end;
         debugPrint ("devmgr: NVMe driver started" & LF);

         if not waitReady (nvmePID) then
            nvmePID := No_Process;
         end if;
      end if;

      --  Attach the NVMe driver to the filesystem.
      if filesystemPID /= No_Process and nvmePID /= No_Process then
         if not Attach_Storage (Fs_Attachments.NVMe_Device, nvmeChild) then
            debugPrint ("devmgr: NVMe not attached to the filesystem" & LF);
         end if;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2b: Spawn PS/2 keyboard + mouse driver
   -----------------------------------------------------------------------
   if ps2Present and then Platform_Bound ("ps2.drv", CCL.Resource_Sections.PS2_Controller) then
      ps2Child := spawnFromBootStorage ("ps2.drv", 5);
   end if;
   ps2PID := PID_Of (ps2Child);
   if ps2PID /= No_Process then
      setupPS2;
      assignCPU (ps2Child, "ps2.drv");
      if DL.Start (ps2Child) then
         debugPrint ("devmgr: PS/2 driver started" & LF);
         if not waitReady (ps2PID) then
            ps2PID := No_Process;
         end if;
      else
         ps2PID := No_Process;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2b': Spawn the virtio-input driver for a virtio tablet.
   -----------------------------------------------------------------------
   if inputDev.found then
      virtioInputChild := spawnFromBootStorage ("virtio-input.drv", 5);
      virtioInputPID := PID_Of (virtioInputChild);
      if virtioInputPID /= No_Process then
         --  Placed first: its vector goes to the CPU it runs on.
         assignCPU (virtioInputChild, "virtio-input.drv");
         setupVirtioInput;
      end if;
      if virtioInputPID /= No_Process and then inputTransportReady
        and then DL.Start (virtioInputChild)
      then
         declare
            use CuBit.Virtio_Input_Control;
            cfg : Message :=
              (tag => (label => Operation'Enum_Rep (Configure),
                       length => 3, protocol => 0, ipc => <>, reserved => 0),
               authorityTag => 0,
               words => [0 => Pair (inputCommonOff, inputDeviceOff),
                         1 => Pair (inputNotifyOff, inputNotifyMult),
                         2 => Pair (inputModernBytes, 0),
                         others => 0]);
            response : MessageTag;
         begin
            response := capCall (DL.Mailbox (virtioInputChild), cfg, CuBit.Messages.Wait_Forever);
            if response.label = REPLY_OK then
               --  The queue vector is set: unmask the entry and the function.
               Open_Message_Interrupts (inputInterrupts);
               debugPrint ("devmgr: virtio-input driver started (MSI-X)" & LF);
               if not waitReady (virtioInputPID) then
                  virtioInputPID := No_Process;
               end if;
            else
               --  The driver declined (not an absolute pointer) and exits.
               debugPrint ("devmgr: virtio-input driver declined the device" & LF);
               Close_Message_Interrupts (inputInterrupts);
               virtioInputPID := No_Process;
            end if;
         end;
      elsif virtioInputPID /= No_Process then
         debugPrint ("devmgr: virtio-input setup unavailable" & LF);
         Close_Message_Interrupts (inputInterrupts);
         DL.Abandon (virtioInputChild);
         virtioInputPID := No_Process;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2c: Spawn xHCI only when PCI discovery found a controller.
   -----------------------------------------------------------------------
   if xhciDev.found then
      xhciChild := spawnFromBootStorage ("xhci.drv", 5);
      xhciPID := PID_Of (xhciChild);
      if xhciPID /= No_Process then
         setupXHCI;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2d: Spawn config store service
   -----------------------------------------------------------------------
   configChild := spawnFromBootStorage ("config.svc", 5);
   configPID := PID_Of (configChild);
   if configPID /= No_Process then
      assignCPU (configChild, "config.svc");
      if DL.Start (configChild) then
         debugPrint ("devmgr: config server started" & LF);
         if not waitReady (configPID) then
            configPID := No_Process;
         end if;
      else
         configPID := No_Process;
      end if;

      --  devmgr's own endpoint to config is the one SPAWN gave it.
      if configPID /= No_Process then
         Config_Endpoint := DL.Mailbox (configChild);
         DL.Provide (Unsigned_32 (DRIVER_CONFIG), Config_Endpoint);
      end if;

      --  Grant config ACL for devmgr itself (wildcard, so we can SET)
      if configPID /= No_Process then
         sendWildcardACLConfig (myPID);
      end if;

      --  Seed config from system.ccl in CPIO initrd
      if configPID /= No_Process then
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
                    (Config_Endpoint, cfgBufAddr, 2, True, Config_Grant, cfgOk);

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
                                      protocol => 0, ipc => <>, reserved => 0),
                              authorityTag => 0,
                              words => [0 => Config_Grant.slot,
                                        1 => Config_Grant.generation,
                                        2 => Unsigned_64 (Key_Length),
                                        3 => Unsigned_64 (Value_Length)]);
                           cfgMsg.tag := capCall (Config_Endpoint, cfgMsg, CuBit.Messages.Wait_Forever);
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
                        Boot_RTC.Read (Root.IO_Ports_Root, UTC, Mono, Valid);
                        declare
                           Value : constant String :=
                             (if Valid then UTC'Image & Mono'Image else "unavailable");
                           Buffer : String (1 .. Key'Length + Value'Length)
                             with Import, Address => cfgBufAddr;
                        begin
                           Buffer := Key & Value;
                           cfgMsg :=
                             (tag => (label => OP_CONFIG_SET, length => 4,
                                      protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                              words => [Config_Grant.slot, Config_Grant.generation, Key'Length, Value'Length]);
                           cfgMsg.tag := capCall (Config_Endpoint, cfgMsg, CuBit.Messages.Wait_Forever);
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
   --  Only for a device PCI discovery found: with nothing to give its
   --  required "registers", a launch would only be refused.
   if gpuDev.found then
      virtioGpuChild := spawnFromBootStorage ("virtio-gpu.drv", 5);
   end if;
   virtioGpuPID := PID_Of (virtioGpuChild);
   if virtioGpuPID /= No_Process then
      --  Placed first: its vector goes to the CPU it runs on.
      assignCPU (virtioGpuChild, "virtio-gpu.drv");
      setupVirtioGpu;
      if DL.Start (virtioGpuChild) then
         debugPrint ("devmgr: virtio-gpu driver started" & LF);
         if not waitReady (virtioGpuPID) then
            virtioGpuPID := No_Process;
         end if;
      else
         virtioGpuPID := No_Process;
      end if;
      if virtioGpuPID /= No_Process then
         DL.Provide (Unsigned_32 (DRIVER_GPU), DL.Mailbox (virtioGpuChild));
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 3: Spawn networking services
   -----------------------------------------------------------------------

   --  Netstack service. Drivers attach to it whenever they come up
   --  (docs/net-link.md); it holds no endpoint to any of them.
   netstackChild := spawnFromBootStorage ("netstack.svc", 5);
   netstackPID := PID_Of (netstackChild);
   if netstackPID /= No_Process then
      assignCPU (netstackChild, "netstack.svc");
      if DL.Start (netstackChild) then
         debugPrint ("devmgr: netstack started" & LF);
         if not waitReady (netstackPID) then
            netstackPID := No_Process;
         end if;
      else
         netstackPID := No_Process;
      end if;
   end if;

   --  One virtio-net driver per virtio-net function (NETLINK interim):
   --  netDev and the net* state describe the one being set up.
   for K in 1 .. netDevCount loop
      exit when netstackPID = No_Process;
      netDev := netDevs (K);
      netInterrupts := (others => <>);
      netModern := False;
      netTransportReady := False;
      netIOBase := 0;
      virtioNetChild := spawnFromBootStorage ("virtio-net.drv", 5);
      virtioNetPID := PID_Of (virtioNetChild);
      if virtioNetPID /= No_Process then
         --  Placed first: its vector goes to the CPU it runs on.
         assignCPU (virtioNetChild, "virtio-net.drv");
         setupVirtioNet;

         --  The driver reaches netstack through an endpoint badged as the
         --  network driver's authority.
         DL.Give (virtioNetChild, "netstack",
           H.Badged (DL.Mailbox (netstackChild), H.Read_Write,
                     CuBit.Network_Authority.Driver_Authority_Tag));

         if netTransportReady and then DL.Start (virtioNetChild) then
            declare
               use CuBit.Virtio_Net_Control;
               cfg : Message :=
                 (if netModern then
                    (tag => (label => Operation'Enum_Rep (Configure_Modern),
                             length => 3, protocol => 0, ipc => <>, reserved => 0),
                     authorityTag => 0,
                     words => [0 => Pair (netCommonOff, netDeviceOff),
                               1 => Pair (netNotifyOff, netNotifyMult),
                               2 => Pair (netModernBytes, 0),
                               others => 0])
                  else
                    (tag => (label => Operation'Enum_Rep (Configure_MSIX),
                             length => 1, protocol => 0, ipc => <>, reserved => 0),
                     authorityTag => 0,
                     words => [0 => netIOBase, others => 0]));
               response : MessageTag;
            begin
               response := capCall (DL.Mailbox (virtioNetChild), cfg, CuBit.Messages.Wait_Forever);
               netTransportReady := response.label = REPLY_OK;
               if netTransportReady then
                  --  Queue vectors are set: unmask the entry and the function.
                  Open_Message_Interrupts (netInterrupts);
                  debugPrint ("devmgr: virtio-net using MSI-X" & LF);
               else
                  Close_Message_Interrupts (netInterrupts);
               end if;
            end;
            debugPrint ("devmgr: virtio-net driver started" & LF);

            if not netTransportReady or else not waitReady (virtioNetPID) then
               virtioNetPID := No_Process;
            end if;
         else
            debugPrint ("devmgr: virtio-net interrupt setup unavailable" & LF);
            Close_Message_Interrupts (netInterrupts);
            DL.Abandon (virtioNetChild);
            virtioNetPID := No_Process;
         end if;
      end if;
      netPIDs (K) := virtioNetPID;
      netInterruptSets (K) := netInterrupts;
   end loop;
   if netstackPID /= No_Process then
      DL.Provide (Unsigned_32 (DRIVER_NETSTACK), DL.Mailbox (netstackChild));
   end if;
   -----------------------------------------------------------------------
   -- Phase 3b: Spawn network manager service
   -----------------------------------------------------------------------
   netmgrChild := spawnFromBootStorage ("netmgr.svc", 5);
   netmgrPID := PID_Of (netmgrChild);
   if netmgrPID /= No_Process then
      --  Its endpoint to netstack (configuration and raw UDP), badged as
      --  the network manager's authority; config comes from its manifest.
      if netstackPID /= No_Process then
         DL.Give (netmgrChild, "netstack",
           H.Badged (DL.Mailbox (netstackChild), H.Read_Write,
                     CuBit.Network_Authority.Manager_Authority_Tag));
      else
         DL.Withhold (netmgrChild, "netstack");
      end if;

      --  Its declared Config scope (net., read only).
      sendDeclaredConfigScopes (netmgrChild, "netmgr.svc");

      assignCPU (netmgrChild, "netmgr.svc");
      if DL.Start (netmgrChild) then
         debugPrint ("devmgr: netmgr started" & LF);
         if not waitReady (netmgrPID) then
            netmgrPID := No_Process;
         end if;
      else
         netmgrPID := No_Process;
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
         Child : Process_ID;
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
            Intel_Child := spawnFromBootStorage ("intel-gpu.drv", 4);
            if Intel_Child /= DL.No_Child then
               Child := DL.PID (Intel_Child);
               Intel_Inspection_PID := Child;
               Intel_Control := DL.Control (Intel_Child);
               Intel_Mailbox := DL.Mailbox (Intel_Child);
               --  devmgr's own endpoint to it, badged as the broker: the
               --  driver recognizes configuration and admission by it.
               Intel_Broker := HC.Derive
                 (H.Badged (Intel_Mailbox, H.Read_Write, Intel_GPU_Boot.Broker_Tag));
               Grant_Intel_Firmware (Intel_Child);
               --  The display executor registers as the GPU service so
               --  display.svc's (request-service gpu) reaches it, unless
               --  virtio-gpu already is. Never two GPU services. procmgr
               --  asks devmgr for the role's provider (Service_Exchange):
               --  intel-gpu's untagged endpoint, as virtio-gpu's.
               if virtioGpuPID /= No_Process then
                  DL.Withhold (Intel_Child, "registration");
               else
                  DL.Provide (Unsigned_32 (DRIVER_GPU), Intel_Mailbox);
               end if;
               --  Its endpoint to devmgr ("device-manager", read by
               --  Intel_GPU_Supervisor), badged so its runtime requests
               --  are recognized, and its register window (read only).
               DL.Give (Intel_Child, "device-manager",
                 H.Badged (Root.Self_Grantable, H.Read_Write, Intel_Ready_Tag));
               Give_Registers (Intel_Child, "intel-gpu.drv", Plan.Physical_Base, Plan.Bytes,
                               H.Read_Only, Name => "registers");
               -- Optional boot diagnostic, created suspended. It reaches the
               -- driver and devmgr only through endpoints badged with the
               -- viewer tag; the desktop endpoint comes later by request.
               GPU_Viewer_Child := spawnFromBootStorage ("gpu-viewer.app", 5);
               if GPU_Viewer_Child /= DL.No_Child then
                  GPU_Viewer_PID := DL.PID (GPU_Viewer_Child);
                  DL.Give (GPU_Viewer_Child, "gpu-probe",
                    H.Badged (Intel_Mailbox, H.Read_Write, GP.Viewer_Tag));
                  DL.Give (GPU_Viewer_Child, "device-manager",
                    H.Badged (Root.Self_Grantable, H.Read_Write, GP.Viewer_Tag));
                  GPU_Viewer_Ready := DL.Install (GPU_Viewer_Child);
                  if not GPU_Viewer_Ready then
                     DL.Abandon (GPU_Viewer_Child);
                  end if;
               end if;
               Request.tag := (label => Intel_GPU_Boot.Configure_Label, length => 4,
                               protocol => 0, ipc => <>, reserved => 0);
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
               --  One MSI vector for the whole device, GT and display alike
               --  (docs/msi-interrupts.md): address and data programmed
               --  now, while configuration writes are still allowed, with
               --  MSI Enable left clear. Enabling it is the next step
               --  (DISP-000-irq / GPU-001 step 6), once intel-gpu has its
               --  interrupt sources masked and a handler ready.
               Setup_Message_Interrupts
                 (intelDev, Intel_Child, "intel-gpu.drv", intelInterrupts,
                  Enable_MSI => False);
               Intel_Config_Frozen := True;
               if DL.Start (Intel_Child) then
                  Submitted := capSubmit (Intel_Broker, Request, NO_COMPLETION_TOKEN);
               else
                  Submitted := False;
                  Intel_Inspection_PID := No_Process;
               end if;
               if GPU_Viewer_Ready and then not Submitted then
                  DL.Abandon (GPU_Viewer_Child);
               end if;
               GPU_Viewer_Ready := GPU_Viewer_Ready and then Submitted;
               if Submitted then
                  debugPrint ("devmgr: Intel inspection launched; firmware display retained" & LF);
               elsif Intel_Inspection_PID = No_Process then
                  Warnings.Warn ("devmgr: intel-gpu.drv not started; software rendering");
               else
                  Warnings.Warn ("devmgr: intel-gpu.drv bootstrap submission failed");
               end if;
            else
               Warnings.Warn ("devmgr: intel-gpu.drv not spawned; software rendering");
            end if;
         else
            Warnings.Warn ("devmgr: Intel GPU " & Hex (Unsigned_64 (pciReadConfig32
              (intelDev.bus, intelDev.slot, intelDev.func, 0))) & " not launched: registers " &
              Intel_GPU_Resources.Name (Plan.Status) & ", power " &
              Intel_GPU_PCI_Power.Name (Power));
         end if;
      end;
   end if;

   --  HDA driver
   if hdaDev.found then
      hdaChild := spawnFromBootStorage ("hda.drv", 5);
      hdaPID := PID_Of (hdaChild);
   end if;

   --  Mixer service
   mixerChild := spawnFromBootStorage ("mixer.svc", 5);
   mixerPID := PID_Of (mixerChild);
   if mixerPID /= No_Process then
      --  hda.drv reaches the mixer through the mixer's role.
      DL.Provide (Unsigned_32 (DRIVER_MIXER), DL.Mailbox (mixerChild));
   end if;

   if hdaPID /= No_Process and mixerPID /= No_Process then
      --  Placed first: its vector goes to the CPU it runs on.
      assignCPU (hdaChild, "hda.drv");
      setupHDA;

      --  The mixer reaches the HDA driver.
      DL.Give (mixerChild, "hda", H.Copy (DL.Mailbox (hdaChild), H.Read_Write));

      if DL.Start (hdaChild) then
         debugPrint ("devmgr: HDA driver started" & LF);
         if not waitReady (hdaPID) then
            hdaPID := No_Process;
         end if;
      else
         hdaPID := No_Process;
      end if;
   else
      if hdaPID /= No_Process then
         DL.Abandon (hdaChild);
         hdaPID := No_Process;
      end if;
      if mixerPID /= No_Process then
         --  Mixer without HDA (no audio hardware found)
         DL.Withhold (mixerChild, "hda");
      end if;
   end if;

   if mixerPID /= No_Process then
      --  mixer.svc may reserve real-time CPU for one audio period: 1.5 ms
      --  of every 5 ms (docs/scheduler.md, "Soft real time").
      DL.Give (mixerChild, "scheduling",
        H.Narrowed (Root.Scheduling_Root, MIXER_REALTIME_BUDGET_US, MIXER_REALTIME_PERIOD_US));
      assignCPU (mixerChild, "mixer.svc");
      if DL.Start (mixerChild) then
         debugPrint ("devmgr: mixer started" & LF);
         if not waitReady (mixerPID) then
            mixerPID := No_Process;
         end if;
      else
         mixerPID := No_Process;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 4b: drivers bound by their declared matches that devmgr has no
   -- dedicated setup for (docs/device-matching.md "Generic PCI launch").
   -----------------------------------------------------------------------
   --  They start in the service loop once procmgr offers logstore
   --  (Launch_Pending_Generic).

   -----------------------------------------------------------------------
   -- Phase 5: Spawn procmgr
   -- Disk drivers already waited in Phase 2, no duplicate wait needed.
   -----------------------------------------------------------------------
   procmgrChild := spawnFromBootStorage ("procmgr.svc", 5);
   procmgrPID := PID_Of (procmgrChild);
   if procmgrPID /= No_Process then
      --  procmgr's handles (Launch_Configuration.Process_Manager_Handles),
      --  placed through its CSPACE before it runs: process authority over
      --  every process it launches, the service roles it hands its
      --  children (it narrows them per child), untagged endpoints with
      --  Grant to the services devmgr started (it badges copies per
      --  child), the network policy endpoint, devmgr's launcher endpoint
      --  (render admission, service exchange) and the boot framebuffer.
      declare
         C : constant DL.Child := procmgrChild;
         function Endpoint_Of (Child : DL.Child; Alive : Boolean) return Unsigned_64 is
           (if Alive then DL.Place (C, H.Copy (DL.Mailbox (Child), Read_Write_Grant)) else 0);
         Handles : constant LC.Process_Manager_Handles :=
           (Self           => HC.Table_Word (DL.Child_Self (C)),
            Self_Process   => HC.Table_Word (DL.Child_Self_Process (C)),
            Self_Grantable => DL.Place (C, H.Copy (DL.Mailbox (C), Read_Write_Grant)),
            Process_Root   => DL.Place (C, H.Copy (Root.Process_Root,
                                (Read | Write | Execute | Grant => True, others => False))),
            Registration   => DL.Place (C, H.Narrowed (Root.Notifications, H.Write_Only,
                                                       DRIVER_PROCMGR, 1)),
            Notifications  => DL.Place (C, H.Copy (Root.Notifications, Read_Write_Grant)),
            Filesystem     => Endpoint_Of (filesystemChild, filesystemPID /= No_Process),
            Config         => Endpoint_Of (configChild, configPID /= No_Process),
            Netstack       => Endpoint_Of (netstackChild, netstackPID /= No_Process),
            Network_Policy =>
              (if netstackPID /= No_Process
               then DL.Place (C, H.Badged (DL.Mailbox (netstackChild), H.Read_Write,
                                          CuBit.Network_Authority.Policy_Authority_Tag))
               else 0),
            Mixer          => Endpoint_Of (mixerChild, mixerPID /= No_Process),
            Device_Manager => DL.Place (C, H.Badged (Root.Self_Grantable, H.Read_Write,
                                                     Intel_GPU_Broker_Request.Authority_Tag)),
            Ready          => DL.Place (C, H.Copy (Root.Self_Grantable, H.Read_Write)),
            Framebuffer    =>
              (if bootFramebuffer.Bytes /= 0
                 and then Devmgr_Authority.Covered (bootFramebuffer.First, bootFramebuffer.Bytes)
               --  With Grant: procmgr copies it, narrowed, to each
               --  requester (a copy needs Grant on its source).
               then DL.Place (C, Devmgr_Authority.Device_Copy
                                   (bootFramebuffer.First, bootFramebuffer.Bytes, Read_Write_Grant))
               else 0),
            --  Inventory queries ("device-manager" requests): untagged,
            --  with Grant so procmgr can hand narrowed copies on.
            Device_Inspection => DL.Place (C, H.Copy (Root.Self_Grantable, Read_Write_Grant)));
      begin
         if not LC.Process_Handles_Well_Formed (Handles) then
            debugPrint ("devmgr: procmgr handles incomplete; boot denied" & LF);
            DL.Abandon (C);
            procmgrPID := No_Process;
         else
            --  The boot framebuffer's pages and procmgr's handles.
            DL.Set_Payload
              (C, LC.Encode (LC.Process_Manager_Configuration'
                   (Framebuffer => bootFramebuffer, Handles => Handles)));
         end if;
      end;
   end if;
   if procmgrPID /= No_Process then
      sendWildcardACL (procmgrPID);
      sendWildcardACLConfig (procmgrPID);
      assignCPU (procmgrChild, "procmgr.svc");
      if DL.Start (procmgrChild) then
         debugPrint ("devmgr: procmgr started" & LF);
         if not waitReady (procmgrPID) then
            procmgrPID := No_Process;
         end if;
      else
         procmgrPID := No_Process;
      end if;
   end if;
   -- Do not expose the viewer's request to waitReady's startup-only handler.
   -- Its first call may now queue until the normal receive loop below.
   if GPU_Viewer_Ready and then not DL.Resume (GPU_Viewer_Child) then
      DL.Abandon (GPU_Viewer_Child);
      GPU_Viewer_Ready := False;
   end if;
   debugPrint ("devmgr: startup complete, entering service loop" & LF);

   loop
      -- One completion, one admission step and one request per iteration:
      -- incoming device traffic cannot starve pending GPU launch completion.
      Launch_Now := syscall (SYSCALL_GETTIME);
      Forward_Launch_Refusals;
      Warnings.Pump;
      Log_Child_Exits (Launch_Now);
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
              (if Warnings.Pending and then
                  Launch_Now <= Unsigned_64'Last - Warnings.Pump_Interval_Ms
               then Unsigned_64'Min (GPU_Launch.Next_Deadline (GPU_Launches),
                                     Launch_Now + Warnings.Pump_Interval_Ms)
               else GPU_Launch.Next_Deadline (GPU_Launches));
            if Launch_Activity = Unavailable then
               -- Exceptional kernel failure, not periodic broker polling.
               ret := syscall (SYSCALL_SLEEP, 1);
            end if;
         end if;
      elsif Generic_Report (from, msg) then
         null;
      elsif msg.tag.label = Intel_GPU_Broker_Request.Label then
         declare
            ID : GPU_Launch.Ticket;
         begin
            Launch_Now := syscall (SYSCALL_GETTIME);
            GPU_Launch.Begin_Request
              (GPU_Launches, To_Word (procmgrPID), To_Word (from), msg.authorityTag, msg,
               Launch_Now,
               (if Launch_Now > Unsigned_64'Last - 5_000 then Launch_Now
                else Launch_Now + 5_000), ID);
            --  Why a render session (Desktop's GPU rendering) will be
            --  refused, for the bring-up photo; intel-gpu's own stage lines
            --  say why it refuses one when it runs.
            if not intelDev.found or else Intel_Inspection_PID = No_Process then
               Warnings.Inform ("devmgr: render admission refused: " &
                 (if not intelDev.found then "no supported GPU bound (software rendering)"
                  else "intel-gpu.drv not running (software rendering)"));
            elsif ID = 0 then
               Warnings.Inform ("devmgr: render admission refused: request not admissible");
            end if;
            if ID = 0 then
               ret := reply (from,
                 (tag => (label => Intel_GPU_Broker_Request.Label, length => 4,
                          protocol => 0, ipc => <>, reserved => 0),
                  authorityTag => 0,
                  words => [Intel_GPU_Broker_Request.Version,
                            Intel_GPU_Broker_Request.Rejected,
                            msg.words (3), msg.words (1)]));
            end if;
         end;
      elsif msg.tag = (label => CuBit.Service_Exchange.Endpoint_Request, length => 1, protocol => 0, ipc => Accepting, reserved => 0)
        and then procmgrPID /= No_Process and then from = procmgrPID
        and then msg.authorityTag = Intel_GPU_Broker_Request.Authority_Tag
      then
         --  procmgr asks for the endpoint of a service devmgr started
         --  (an untagged copy with Grant, so it can badge copies for its
         --  children); only a running provider registered for the role.
         declare
            Role : constant Unsigned_64 := msg.words (0);
            Provider : constant H.Endpoint :=
              (if Role <= Unsigned_64 (Unsigned_32'Last)
                 and then Registered_Driver (Role) /= No_Process
               then DL.Service_Of (Unsigned_32 (Role)) else H.No_Endpoint);
         begin
            if not H.Valid (Provider) or else
              not Answer_With (from, [Role, 0, 0, 0], 1, H.Copy (Provider, Read_Write_Grant))
            then
               Refuse (from, Answered => H.Valid (Provider));
            end if;
         end;
      elsif msg.tag.label = CuBit.Service_Exchange.Available
        and then msg.tag.length = 1
        and then procmgrPID /= No_Process and then from = procmgrPID
        and then msg.authorityTag = Intel_GPU_Broker_Request.Authority_Tag
      then
         --  procmgr offers the endpoint of a service it launched that devmgr
         --  uses: logstore (driver log publishers, its own warnings) and the
         --  desktop (the GPU viewer's binding). Others are not taken.
         declare
            Role : constant Unsigned_64 := msg.words (0);
            Taken : IPC.Carried;
            Offered : IPC.Accepted_Endpoint;
         begin
            if Role in DRIVER_LOGSTORE | DRIVER_DESKTOP and then IPC.Offers (msg) then
               Taken := IPC.Accept_Offered (from);
               Offered := IPC.Accept_Endpoint (Taken);
               if Offered.OK then
                  if Role = DRIVER_LOGSTORE then
                     if H.Valid (Logstore_Endpoint) then
                        Close_Quietly (Logstore_Endpoint);
                     end if;
                     Logstore_Endpoint := Offered.Value;
                  else
                     if H.Valid (Desktop_Endpoint) then
                        Close_Quietly (Desktop_Endpoint);
                     end if;
                     Desktop_Endpoint := Offered.Value;
                  end if;
               end if;
            end if;
            ret := reply (from, (tag => (label => REPLY_OK, length => 0,
                                         protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                                 words => [others => 0]));
            --  After the reply: procmgr is not kept waiting while they spawn.
            if Role = DRIVER_LOGSTORE and then H.Valid (Logstore_Endpoint) then
               Launch_Pending_Generic;
            end if;
         end;
      elsif msg.tag.label = GP.Desktop_Binding_Label then
         --  The GPU viewer asks for the desktop endpoint (accepting it in the
         --  reply): code 0 with the endpoint, 2 while there is no desktop.
         declare
            Code : Unsigned_64 := 1;
            Replied : Boolean := False;
         begin
            if GPU_Viewer_Ready and then from = GPU_Viewer_PID and then
              msg.authorityTag = GP.Viewer_Tag and then
              msg.tag = (label => GP.Desktop_Binding_Label, length => 4, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words = [1, 0, 0, 0]
            then
               if GPU_Viewer_Desktop_Bound then Code := 0;
               elsif not H.Valid (Desktop_Endpoint) then Code := 2;
               else
                  declare
                     Result : constant IPC.Reply_Result := IPC.Reply_With
                       (from, (tag => (label => GP.Desktop_Binding_Label, length => 4,
                                       protocol => 0, ipc => <>, reserved => 0),
                               authorityTag => 0, words => [0, 1, 0, 0]),
                        H.Copy (Desktop_Endpoint, H.Read_Write));
                  begin
                     Replied := Result.Outcome /= IPC.Refused;
                     GPU_Viewer_Desktop_Bound := Result.Outcome = IPC.Transferred;
                  end;
               end if;
            end if;
            if not Replied then
               ret := reply (from, (tag => (label => GP.Desktop_Binding_Label, length => 4,
                                            protocol => 0, ipc => <>, reserved => 0),
                 authorityTag => 0, words => [Code, 1, 0, 0]));
            end if;
         end;
      elsif msg.tag = (label => 16#0228#, length => 0, protocol => 0, ipc => Accepting, reserved => 0) and then
        from /= No_Process and then
        (from = xhciPID or else from = netmgrPID) and then
        msg.words = [0, 0, 0, 0]
      then
         --  A log publisher for a child started before logstore (xhci,
         --  netmgr): a logstore endpoint badged with its publication tag,
         --  in the reply.
         if not H.Valid (Logstore_Endpoint) or else
           not Answer_With (from, [others => 0], 0,
             H.Badged (Logstore_Endpoint, H.Read_Write,
                       CuBit.Log_Protocol.Publisher_Tag
                         (Driver_Log_Budget,
                          (if from = xhciPID then XHCI_Log_Issuance
                           else Netmgr_Log_Issuance))))
         then
            Refuse (from, Answered => H.Valid (Logstore_Endpoint));
         end if;
      elsif msg.tag = (label => 16#0229#, length => 0, protocol => 0, ipc => Accepting, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         if not H.Valid (Logstore_Endpoint) or else
           not Answer_With (from, [others => 0], 0,
             H.Badged (Logstore_Endpoint, H.Read_Write,
                       CuBit.Log_Protocol.Publisher_Tag (Driver_Log_Budget, Intel_Log_Issuance)))
         then
            Refuse (from, Answered => H.Valid (Logstore_Endpoint));
         end if;
      elsif msg.tag = (label => 16#022A#, length => 0, protocol => 0, ipc => Accepting, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         --  One-page write authority for the GT forcewake request register.
         --  No PCI ports, GGTT, aperture, DMA or display ownership is granted.
         declare
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Answered : Boolean := False;
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
                  Answered := True;
                  if Devmgr_Authority.Covered (Plan.Physical_Base, PAGE_BYTES) and then
                    Answer_With (from, [others => 0], 0,
                      Devmgr_Authority.Device_Copy (Plan.Physical_Base, PAGE_BYTES, H.Read_Write))
                  then
                     Intel_Forcewake_Granted := True;
                     debugPrint ("devmgr: Intel forcewake page granted; PCI config frozen" & LF);
                  elsif not Devmgr_Authority.Covered (Plan.Physical_Base, PAGE_BYTES) then
                     Answered := False;
                  end if;
               end if;
            end if;
            Refuse (from, Answered);
         end;
      elsif msg.tag = (label => 16#022F#, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         declare
            Allowed, Valid : Boolean := False;
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            if Intel_Config_Frozen and then intelDev.found and then
              Intel_GPU_ADLN_Devices.Supported_Identity (Intel_Claim_Identity) and then
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
              (Intel_Display_Ownership, To_Word (Intel_Inspection_PID), To_Word (from),
               Badge_Valid => True, Device_Valid => Valid, Allowed => Allowed);
            -- Consume before replying, including lost replies. This designates
            -- the sole power-request manager; it grants NO writable MMIO and
            -- establishes NO hardware reference by itself. Any future power
            -- page grant must require this same retained owner.
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Allowed then 16#F000# else 16#F001#),
                length => 0, protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (label => Intel_GPU_PCI_Interrupts.Disable_Request_Label, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         declare
            Status : Intel_IRQ_Disable.Result;
            use type Intel_IRQ_Disable.Result;
         begin
            Intel_IRQ_Active := Intel_Config_Frozen and then intelDev.found and then
              Intel_Reset_Authorized and then Intel_Forcewake_Granted and then
              Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from);
            Intel_IRQ_Disable.Execute (Intel_IRQ_Active, Status);
            Intel_IRQ_Active := False;
            Intel_IRQ_Plan_Ready := False;
            -- Success is PCI control readback, NOT pending-vector drain.
            -- Repeated requests cannot replay a consumed executor after loss.
            ret := Unsigned_64 (reply (from,
              (tag => (label => (if Status = Intel_IRQ_Disable.Complete then 16#F000# else 16#F001#),
                length => 0, protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif msg.tag = (label => 16#022E#, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
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
              Intel_GPU_ADLN_Devices.Supported_Identity (Intel_Claim_Identity) and then
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
                length => 0, protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                words => [others => 0])));
         end;
      elsif ((msg.tag = (label => 16#022D#, length => 1, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_Reset_Pages.Page_Index'Last)) or else
             (msg.tag = (label => Intel_GPU_Display_Pages.Request_Label, length => 1, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_Display_Pages.Page_Index'Last)) or else
             (msg.tag = (label => Intel_GPU_PHY_Pages.Request_Label, length => 1, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_PHY_Pages.Page_Index'Last)) or else
             (msg.tag = (label => Intel_GPU_Pipe_Pages.Request_Label, length => 1, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words (0) <= Unsigned_64 (Intel_GPU_Pipe_Pages.Page_Index'Last)) or else
             (msg.tag = (label => Intel_GPU_Modeset_Pages.Request_Label, length => 1, protocol => 0, ipc => Accepting, reserved => 0) and then
              msg.words (0) <= Intel_GPU_Modeset_Pages.Index (Intel_GPU_Modeset_Pages.Page'Last))) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then
        msg.words (1 .. 3) = [0, 0, 0]
      then
         declare
            Display_Page : constant Boolean := msg.tag.label = Intel_GPU_Display_Pages.Request_Label;
            PHY_Page : constant Boolean := msg.tag.label = Intel_GPU_PHY_Pages.Request_Label;
            --  One pipe's register page (cursor and primary plane), for the
            --  display executor (DISP-003, Intel_GPU_Pipe_Pages).
            Pipe_Page : constant Boolean := msg.tag.label = Intel_GPU_Pipe_Pages.Request_Label;
            --  GMBUS, transcoder and combo PLL pages for DDC/EDID reads and
            --  modesetting (DISP-004/006, Intel_GPU_Modeset_Pages).
            Modeset_Page : constant Boolean := msg.tag.label = Intel_GPU_Modeset_Pages.Request_Label;
            Index : constant Intel_GPU_Reset_Pages.Page_Index := Natural (msg.words (0));
            Modeset_Item : constant Intel_GPU_Modeset_Pages.Page :=
              (if Modeset_Page then Intel_GPU_Modeset_Pages.Page'Val (Index)
               else Intel_GPU_Modeset_Pages.Page'First);
            Offset : constant Unsigned_64 :=
              (if Display_Page then Intel_GPU_Display_Pages.Offset (Index)
               elsif PHY_Page then Intel_GPU_PHY_Pages.Offset (Index)
               elsif Pipe_Page then Intel_GPU_Pipe_Pages.Offset (Index)
               elsif Modeset_Page then Intel_GPU_Modeset_Pages.Offset (Modeset_Item)
               else Intel_GPU_Reset_Pages.Offset (Index));
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Answered, Granted : Boolean := False;
            use type Intel_GPU_Resources.Admission_Status;
            use type Intel_GPU_PCI_Power.Power_Status;
         begin
            -- Never caller-selected addresses, lengths, slots or PCI identity.
            -- Partial grants persist under the static claim; no retry/rebind.
            if Intel_Config_Frozen and then intelDev.found and then
              Intel_Forcewake_Granted and then Intel_Claim_Base /= 0 and then
              (if Display_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from) and then
                 not Intel_Display_Granted (Index)
               elsif PHY_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from) and then
                 not Intel_PHY_Granted (Index)
               elsif Pipe_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from) and then
                 not Intel_Pipe_Granted (Index)
               elsif Modeset_Page then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from) and then
                 not Intel_Modeset_Granted (Modeset_Item)
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
                 Intel_GPU_ADLN_Devices.Supported_Identity (Intel_Claim_Identity) and then
                 pciReadConfig32 (intelDev.bus, intelDev.slot, intelDev.func, 0) =
                   Intel_Claim_Identity and then Plan.Status = Intel_GPU_Resources.Admitted and then
                 Plan.Physical_Base >= Intel_Claim_Base and then
                 Plan.Physical_Base - Intel_Claim_Base = Offset
               then
                  if Devmgr_Authority.Covered (Plan.Physical_Base, PAGE_BYTES) then
                     Answered := True;
                     Granted := Answer_With (from, [others => 0], 0,
                       Devmgr_Authority.Device_Copy (Plan.Physical_Base, PAGE_BYTES, H.Read_Write));
                  end if;
                  if Granted then
                     if Display_Page then Intel_Display_Granted (Index) := True;
                     elsif PHY_Page then Intel_PHY_Granted (Index) := True;
                     elsif Pipe_Page then Intel_Pipe_Granted (Index) := True;
                     elsif Modeset_Page then Intel_Modeset_Granted (Modeset_Item) := True;
                     else Intel_Reset_Granted (Index) := True;
                     end if;
                  end if;
               end if;
            end if;
            Refuse (from, Answered);
         end;
      elsif msg.tag = (label => Intel_GPU_GGTT_Access.Write_Request_Label, length => 0, protocol => 0, ipc => Accepting, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         declare
            Config : Intel_GPU_PCI_Power.Configuration := [others => 255];
            Plan : Intel_GPU_GGTT_Access.Grant_Plan;
            Raw : Unsigned_64 := reterr;
            Snapshot_OK : Boolean := True;
            Answered : Boolean := False;
            use type Intel_IRQ_Disable.Phase;
         begin
            if not Intel_GGTT_Write_Attempted then
               -- Consume before inspection/mint; lost replies cannot replay.
               Intel_GGTT_Write_Attempted := True;
               if Intel_Config_Frozen and then intelDev.found and then
                 Intel_GPU_ADLN_Devices.Supported_Identity (Intel_Claim_Identity) and then
                 Intel_Reset_Authorized and then Intel_GGTT_Granted and then
                 Intel_GPU_Display_Claim.Owner (Intel_Display_Ownership) = To_Word (from) and then
                 Intel_IRQ_Disable.State = Intel_IRQ_Disable.PCI_Disabled
               then
                  -- A single fresh snapshot supplies identity, BAR, table size,
                  -- power and interrupt state. No client-selected range/slot.
                  for Index in 0 .. 63 loop
                     if portOutp32 (Root.PCI_Ports, PCI_CONFIG_ADDR, Intel_Config_Address (Index * 4)) /= 0 then
                        Snapshot_OK := False; exit;
                     end if;
                     Raw := portInp32 (Root.PCI_Ports, PCI_CONFIG_DATA);
                     if Raw = reterr then Snapshot_OK := False; exit; end if;
                     for Byte in 0 .. 3 loop
                        Config (Index * 4 + Byte) := Unsigned_8
                          (Shift_Right (Raw, Byte * 8) and 255);
                     end loop;
                  end loop;
                  if Snapshot_OK then
                     Plan := Intel_GPU_GGTT_Access.Plan_Write
                       (Config, Intel_GGTT_BAR, Intel_GGTT_Bytes, True, True);
                     if Plan.Valid and then
                       Devmgr_Authority.Covered (Plan.Physical, Plan.Bytes)
                     then
                        Answered := True;
                        if not Answer_With (from, [Plan.Physical, Plan.Bytes, 0, 0], 2,
                          Devmgr_Authority.Device_Copy (Plan.Physical, Plan.Bytes, H.Read_Write))
                        then
                           debugPrint ("devmgr: Intel GGTT write grant not delivered" & LF);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
            Refuse (from, Answered);
         end;
      elsif msg.tag = (label => 16#022B#, length => 0, protocol => 0, ipc => Accepting, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         declare
            Config : Intel_GPU_PCI_Power.Configuration;
            Plan : Intel_GPU_Resources.Mapping_Plan;
            Physical : Unsigned_64 := 0;
            Table_Bytes : Unsigned_64 := 0;
            Answered : Boolean := False;
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
               if Physical /= 0 and then Devmgr_Authority.Covered (Physical, Table_Bytes) then
                  Answered := True;
                  Intel_GGTT_Granted := Answer_With (from, [Physical, Table_Bytes, 0, 0], 2,
                    Devmgr_Authority.Device_Copy (Physical, Table_Bytes, H.Read_Only));
                  if Intel_GGTT_Granted then
                     Intel_GGTT_BAR := Plan.Physical_Base;
                     Intel_GGTT_Bytes := Table_Bytes;
                     debugPrint ("devmgr: Intel GGTT inspection page granted read-only" & LF);
                  end if;
               end if;
            end if;
            Refuse (from, Answered);
         end;
      elsif msg.tag = (label => 16#022C#, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         -- Fixed supervisor-owned allocation policy, not caller-controlled
         -- addresses or sizes. Retain even if the driver subsequently dies.
         if not Intel_Buffer_Attempted then
            Intel_Buffer_Attempted := True;
            if Intel_GGTT_Granted then
               Intel_Buffer_Physical := HC.Allocate_DMA
                 (Intel_Inspection_PID, Intel_Control, Intel_Retained_Order,
                  Intel_Retained_Virtual, DMA_Mode_Retained, Intel_DMA_Ceiling);
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
              (tag => (label => 16#F000#, length => 3,
                       protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
               words => [Intel_Buffer_Physical, Intel_Retained_Virtual, Intel_Retained_Bytes, 0])));
         else
            ret := Unsigned_64 (reply (from,
              (tag => (label => 16#F001#, length => 0,
                       protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
         end if;
      elsif Intel_GPU_Buffer_Backing.Valid_Allocation_Request
        (msg.tag.label, msg.tag.length, CuBit.Messages.Flag_Byte (msg.tag), msg.tag.reserved,
         msg.words (0), msg.words (1), msg.words (2), msg.words (3)) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag
      then
         declare
            Accepted : Boolean;
         begin
            Intel_Allocation_Growth.Begin_Request
              (Intel_Allocations, Intel_GPU_Buffer_Backing.Slot (msg.words (0)),
               Intel_GPU_Buffer_Backing.Page_Count (msg.words (1)),
               Unsigned_32 (msg.words (2)), Accepted);
            if Accepted then
               -- Begin_Request only saves the request; Step runs later.
               -- Do not reset the pending request's evidence on a rejection.
               Intel_Requested_Bytes := msg.words (1) * 4096;
               Buffer_Allocations.Reset_Allocation_Diagnostic (Intel_Buffer_Pool);
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (label => 16#F001#, length => 0,
                          protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag.label = Intel_GPU_Buffer_Backing.Retire_Request_Label then
         declare
            Granted : Boolean := False;
         begin
            if Intel_GPU_Buffer_Backing.Retirement_Authorized
              (msg.tag.label, msg.tag.length, CuBit.Messages.Flag_Byte (msg.tag), msg.tag.reserved,
               Intel_GPU_Buffer_Backing.Budget_Words (msg.words), To_Word (from),
               msg.authorityTag, To_Word (Intel_Inspection_PID), Intel_Buffer_Arena_Granted)
            then
               -- Only the trusted driver can certify hardware/CPU retirement.
               -- This releases an exact-generation arena slice, never DMA RAM.
               Buffer_Allocations.Retire_Buffer
                 (Intel_Buffer_Pool, To_Word (Intel_Inspection_PID),
                  Intel_GPU_Buffer_Backing.Slot (msg.words (0)),
                  Unsigned_32 (msg.words (1)), True, Granted);
            end if;
            if Granted then
               ret := Unsigned_64 (reply (from,
                 (tag => (label => Intel_GPU_Buffer_Backing.Retire_Request_Label, length => 4,
                          protocol => 0, ipc => <>, reserved => 0),
                  authorityTag => 0, words => [0,
                    Intel_GPU_Buffer_Backing.Allocation_Key
                      (Intel_GPU_Buffer_Backing.Slot (msg.words (0)), Unsigned_32 (msg.words (1))),
                    To_Word (Intel_Inspection_PID), 0])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (label => 16#F001#, length => 0,
                          protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (label => Intel_GPU_Buffer_Backing.Budget_Request_Label, length => 4, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [Intel_GPU_Buffer_Backing.Budget_Version, 0, 0, 0]
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
                 (tag => (label => Intel_GPU_Buffer_Backing.Budget_Request_Label, length => 4,
                          protocol => 0, ipc => <>, reserved => 0),
                  authorityTag => 0, words => [Data (0), Data (1), Data (2), Data (3)])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (label => 16#F001#, length => 0,
                          protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif Intel_GPU_Buffer_Backing.Extent_Request_Authorized
        (msg.tag.label, msg.tag.length, CuBit.Messages.Flag_Byte (msg.tag), msg.tag.reserved,
         Intel_GPU_Buffer_Backing.Budget_Words (msg.words), To_Word (from),
         msg.authorityTag, To_Word (Intel_Inspection_PID),
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
                 (tag => (label => 16#F003#, length => 4,
                          protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
                  words => [msg.words (0), Part.Address,
                    Intel_GPU_Buffer_Backing.CPU_Base + Offset, To_Word (Intel_Inspection_PID)])));
            else
               ret := Unsigned_64 (reply (from,
                 (tag => (label => 16#F001#, length => 0,
                          protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
            end if;
         end;
      elsif msg.tag = (label => Intel_GPU_ADS_Backing.Request_Label, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
        Intel_Inspection_PID /= No_Process and then from = Intel_Inspection_PID and then
        msg.authorityTag = Intel_Ready_Tag and then msg.words = [0, 0, 0, 0]
      then
         if not Intel_ADS_Attempted then
            Intel_ADS_Attempted := True;
            if Intel_Reset_Authorized and then Intel_Config_Frozen and then
              Intel_GGTT_Granted
            then
               Intel_ADS_Physical := HC.Allocate_DMA
                 (Intel_Inspection_PID, Intel_Control, Intel_GPU_ADS_Backing.Allocation_Order,
                  Intel_GPU_ADS_Backing.CPU_Address, DMA_Mode_Retained, Intel_DMA_Ceiling);
               if not Intel_GPU_ADS_Backing.Valid_Physical (Intel_ADS_Physical) then
                  Intel_ADS_Physical := 0;
               end if;
            end if;
         end if;
         if Intel_ADS_Physical /= 0 then
            ret := Unsigned_64 (reply (from,
              (tag => (label => 16#F000#, length => 3,
                       protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
               words => [Intel_ADS_Physical, Intel_GPU_ADS_Backing.CPU_Address,
                         Intel_GPU_ADS_Backing.Capacity, 0])));
         else
            ret := Unsigned_64 (reply (from,
              (tag => (label => 16#F001#, length => 0,
                       protocol => 0, ipc => <>, reserved => 0), authorityTag => 0, words => [others => 0])));
         end if;
      elsif msg.tag.label = CuBit.Devices.OP_INVENTORY_COUNT then
         ret := Unsigned_64
           (reply
              (from,
               (tag => (label  => CuBit.Devices.REPLY_OK,
                        length => 2, protocol => 0, ipc => <>, reserved => 0),
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
            driverPID : Process_ID := No_Process;
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
                  if Net_Device_At (item.location) /= 0 then
                     driverPID := netPIDs (Net_Device_At (item.location));
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
            if driverPID /= No_Process then
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
                           length => 4, protocol => 0, ipc => <>, reserved => 0),
                   authorityTag => 0,
                   words =>
                     [0 => locationWord, 1 => identityWord,
                      2 => To_Word (driverPID), 3 => 1])));
         end;
      elsif msg.tag.label = CuBit.Devices.OP_INTERRUPT_VECTORS and then
            msg.tag.length >= 1 and then msg.words (0) >= 1 and then
            msg.words (0) <= Unsigned_64 (inventoryCount)
      then
         declare
            item : Inventory_Device renames
              inventory (Natural (msg.words (0)));
            F : constant Message_Function :=
              (if Same_Location (item.location, nvmeDev) then nvmeInterrupts
               elsif Net_Device_At (item.location) /= 0 then
                 netInterruptSets (Net_Device_At (item.location))
               elsif Same_Location (item.location, gpuDev) then gpuInterrupts
               elsif Same_Location (item.location, intelDev) then intelInterrupts
               elsif Same_Location (item.location, hdaDev) then hdaInterrupts
               elsif Same_Location (item.location, xhciDev) then xhciInterrupts
               elsif Same_Location (item.location, inputDev) then inputInterrupts
               else (others => <>));
            mode : constant CuBit.Devices.Interrupt_Mode :=
              (case F.Mode is
                  when MSIX_Mode => CuBit.Devices.Interrupt_MSIX,
                  when MSI_Mode  => CuBit.Devices.Interrupt_MSI,
                  when Polled    => CuBit.Devices.Interrupt_Polling);
            delivered : Unsigned_64 := 0;
            count : Unsigned_64;
         begin
            if F.Mode /= Polled then
               for V in F.Vectors.Head .. F.Vectors.Head + F.Vectors.Count - 1 loop
                  count := CuBit.Device_Interrupts.Read
                    (V, Device_Vector_ABI.Interrupt_Count, Root.Vector_Authority);
                  if count /= reterr then
                     delivered := delivered + count;
                  end if;
               end loop;
            end if;
            ret := Unsigned_64
              (reply
                 (from,
                  (tag => (label => CuBit.Devices.REPLY_OK,
                           length => 4, protocol => 0, ipc => <>, reserved => 0),
                   authorityTag => 0,
                   words =>
                     [0 => Unsigned_64 (CuBit.Devices.Interrupt_Mode'Enum_Rep (mode)),
                      1 => (if F.Mode = Polled then 0 else Unsigned_64 (F.Vectors.Head)),
                      2 => (if F.Mode = Polled then 0 else Unsigned_64 (F.Vectors.Count)),
                      3 => delivered])));
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
                   length => 4, protocol => 0, ipc => <>, reserved => 0),
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
                        length => 0, protocol => 0, ipc => <>, reserved => 0),
                authorityTag => 0, words => [others => 0])));
      end if;
   end loop;

end main;
