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
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Cpio;

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

   nvmeDev   : PCIDeviceInfo;
   ataDev    : PCIDeviceInfo;
   netDev    : PCIDeviceInfo;
   hdaDev    : PCIDeviceInfo;
   xhciDev   : PCIDeviceInfo;
   gpuDev    : PCIDeviceInfo;
   gpuIsPrimary : Boolean := False;

   --  Service PIDs
   filesystemPID : Unsigned_64 := 0;
   ataPID        : Unsigned_64 := 0;
   nvmePID       : Unsigned_64 := 0;
   netstackPID   : Unsigned_64 := 0;
   virtioNetPID  : Unsigned_64 := 0;
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
      data32 : Unsigned_32;
      ignore : Unsigned_64;
   begin
      addr := 16#8000_0000# or
              Shift_Left (Unsigned_32 (bus), 16) or
              Shift_Left (Unsigned_32 (pSlot), 11) or
              Shift_Left (Unsigned_32 (func), 8) or
              Unsigned_32 (offset and 16#FC#);
      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      data32 := Unsigned_32 (portInp32 (PCI_CONFIG_DATA));

      --  Modify the correct 16-bit word
      if (offset and 2) /= 0 then
         data32 := (data32 and 16#0000_FFFF#) or
                   Shift_Left (Unsigned_32 (value), 16);
      else
         data32 := (data32 and 16#FFFF_0000#) or Unsigned_32 (value);
      end if;

      ignore := portOutp32 (PCI_CONFIG_ADDR, addr);
      ignore := portOutp32 (PCI_CONFIG_DATA, data32);
   end pciWriteConfig16;

   ---------------------------------------------------------------------------
   -- PCI bus scan: find devices by class code
   ---------------------------------------------------------------------------
   procedure scanPCI is
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
            debugPrint (" NVMe");
         elsif classCode = CLASS_STORAGE_IDE then
            ataDev := location;
            debugPrint (" IDE");
         elsif classCode = CLASS_STORAGE_SATA and then
               progIf = PCI_PROG_IF_AHCI
         then
            debugPrint (" AHCI (unclaimed)");
         elsif classCode = CLASS_NET_ETHERNET then
            if vendorID = VENDOR_VIRTIO then
               netDev := location;
               debugPrint (" virtio-net");
            else
               debugPrint (" Ethernet (unclaimed)");
            end if;
         elsif classCode = CLASS_MULTIMEDIA_HDA then
            hdaDev := location;
            debugPrint (" HDA");
         elsif classCode = CLASS_SERIAL_USB and then
               progIf = PCI_PROG_IF_XHCI
         then
            xhciDev := location;
            debugPrint (" xHCI");
         elsif vendorID = VENDOR_VIRTIO and then
               (deviceID = DEVICE_VIRTIO_GPU or else
                classCode = CLASS_DISPLAY_VGA)
         then
            gpuDev := location;
            gpuIsPrimary := classCode = CLASS_DISPLAY_VGA;
            debugPrint (" virtio-gpu");
         end if;

         debugPrint (LF & "");
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
   function spawnFromCpio (name     : String;
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
         debugPrint ("devmgr: not found in CPIO: " & name & LF);
         return 0;
      end if;

      addr := INITRD_BASE +
              Unsigned_64 (cpioArchive.files (idx).dataOff);
      size := Unsigned_64 (cpioArchive.files (idx).dataSize);

      nameLen := name'Length;
      if nameLen > 16 then
         nameLen := 16;
      end if;
      for i in 0 .. nameLen - 1 loop
         nameBuf (i + 1) := name (name'First + i);
      end loop;

      return syscall (SYSCALL_SPAWN, addr, size, priority,
                      Unsigned_64 (To_Integer (nameBuf'Address)), reqPID);
   end spawnFromCpio;

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
                            badgePID  : Unsigned_64)
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
         strEq (name, "nvme.drv") or strEq (name, "procmgr.svc") or
         strEq (name, "ps2.drv") or strEq (name, "xhci.drv")
      then
         cpu := 0;
      --  Network services on CPU 1
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
      receive (sender, rdyMsg);
      ignore := reply (sender, NULL_MESSAGE);
      return rdyMsg.tag.label = OP_READY;
   end waitReady;

   ---------------------------------------------------------------------------
   -- Send a wildcard ACL to the filesystem server for a target process
   ---------------------------------------------------------------------------
   OP_SET_ACL : constant Unsigned_32 := 16#0080#;

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
                      badge  => 0);
      aclMsg.capBadge := 0;
      aclMsg.words := (0 => targetPID, 1 => 0, 2 => 0, 3 => 0);
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
                      badge  => 0);
      aclMsg.capBadge := 0;
      aclMsg.words := (0 => targetPID, 1 => 0, 2 => 0, 3 => 0);
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

      --  Enable IRQ routing
      ret := enableIrq (irqVector, nvmePID, 0);

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
   -- Setup virtio-net driver: PCI config, DMA, capabilities
   ---------------------------------------------------------------------------
   procedure setupVirtioNet is
      bar0Raw   : Unsigned_32;
      bar0Base  : Unsigned_64;
      irqLine   : Unsigned_8;
      irqVector : Unsigned_64;
      pciCmd    : Unsigned_16;
      dmaPhys   : Unsigned_64;
      ret       : Unsigned_64;

      DMA_ORDER : constant Unsigned_64 := 6;  --  64 pages = 256KB
      DMA_PAGES : constant Unsigned_64 := 64;
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
   begin
      if not netDev.found or virtioNetPID = 0 then
         return;
      end if;

      --  Read BAR0 (I/O space)
      bar0Raw := pciReadConfig32 (netDev.bus, netDev.slot, netDev.func,
                                  PCI_BASEADDR_0);
      bar0Base := Unsigned_64 (bar0Raw and 16#FFFC#);

      --  Read IRQ line
      irqLine := pciReadConfig8 (netDev.bus, netDev.slot, netDev.func,
                                 PCI_INTERRUPT_LINE);
      irqVector := 32 + Unsigned_64 (irqLine);

      --  Enable I/O space + bus master
      pciCmd := pciReadConfig16 (netDev.bus, netDev.slot, netDev.func,
                                 PCI_COMMAND);
      pciWriteConfig16 (netDev.bus, netDev.slot, netDev.func,
                        PCI_COMMAND, pciCmd or 16#0005#);

      --  Enable IRQ routing
      ret := enableIrq (irqVector, virtioNetPID, 0);

      --  Allocate DMA
      dmaPhys := allocDma (virtioNetPID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: virtio-net DMA alloc failed" & LF);
         return;
      end if;

      --  Slot 4: CAP_IOPORT for BAR0 (32 ports)
      mintCap (virtioNetPID, CAP_IOPORT, bar0Base, 32,
               RIGHT_READ or RIGHT_WRITE, 4);

      --  Slot 5: CAP_IRQ for device interrupt
      mintCap (virtioNetPID, CAP_IRQ, irqVector, 0, RIGHT_READ, 5);

      --  Slot 6: CAP_DEVICE_MEM for DMA region
      mintCap (virtioNetPID, CAP_DEVICE_MEM, 0, DMA_SIZE,
               RIGHT_READ or RIGHT_WRITE, 6);

      --  Publish BAR0 via sysinfo
      ret := setSysinfo (SYSINFO_NET_IOBASE, bar0Base);

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

      DMA_ORDER : constant Unsigned_64 := 11; -- 8 MiB
      DMA_PAGES : constant Unsigned_64 := 2048;
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

      --  Slot 4: virtio modern MMIO BAR.
      mintCap (virtioGpuPID, CAP_DEVICE_MEM, barPhys, BAR_MAP_SIZE,
               RIGHT_READ or RIGHT_WRITE, 4);
      --  Slot 5: IRQ.
      mintCap (virtioGpuPID, CAP_IRQ, irqVector, 0, RIGHT_READ, 5);
      --  Slot 6: DMA region.
      mintCap (virtioGpuPID, CAP_DEVICE_MEM, 0, DMA_SIZE,
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

      --  Register IRQ owners and enable IOAPIC routing
      ret := enableIrq (33, ps2PID, 0);
      ret := enableIrq (44, ps2PID, 0);

      debugPrint ("devmgr: PS/2 setup complete" & LF);
   end setupPS2;

   ---------------------------------------------------------------------------
   -- Probe a memory BAR's implemented size while its decode is disabled.
   -- Returns zero for an I/O BAR or a malformed/unsupported result.
   ---------------------------------------------------------------------------
   function probeMemoryBAR0Size
     (dev : PCIDeviceInfo) return Unsigned_64
   is
      command : constant Unsigned_16 :=
        pciReadConfig16 (dev.bus, dev.slot, dev.func, PCI_COMMAND);
      originalLo : constant Unsigned_32 :=
        pciReadConfig32 (dev.bus, dev.slot, dev.func, PCI_BASEADDR_0);
      originalHi : constant Unsigned_32 :=
        pciReadConfig32 (dev.bus, dev.slot, dev.func, PCI_BASEADDR_1);
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
        (dev.bus, dev.slot, dev.func, PCI_BASEADDR_0, 16#FFFF_FFFF#);
      if is64Bit then
         pciWriteConfig32
           (dev.bus, dev.slot, dev.func, PCI_BASEADDR_1, 16#FFFF_FFFF#);
      end if;

      probeLo := pciReadConfig32
        (dev.bus, dev.slot, dev.func, PCI_BASEADDR_0);
      if is64Bit then
         probeHi := pciReadConfig32
           (dev.bus, dev.slot, dev.func, PCI_BASEADDR_1);
      end if;

      --  Restore the BAR before re-enabling memory decoding.
      if is64Bit then
         pciWriteConfig32
           (dev.bus, dev.slot, dev.func, PCI_BASEADDR_1, originalHi);
      end if;
      pciWriteConfig32
        (dev.bus, dev.slot, dev.func, PCI_BASEADDR_0, originalLo);
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
   end probeMemoryBAR0Size;

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

      DMA_ORDER : constant Unsigned_64 := 6;  --  64 pages = 256 KiB
      DMA_SIZE  : constant Unsigned_64 := 64 * 4096;
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

      barSize := probeMemoryBAR0Size (xhciDev);
      if bar0Phys = 0 or else barSize = 0 then
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

      dmaPhys := allocDma (xhciPID, DMA_ORDER, DMA_VIRT_BASE);
      if dmaPhys = reterr then
         debugPrint ("devmgr: xHCI DMA allocation failed" & LF);
         return;
      end if;

      mintCap
        (xhciPID, CAP_DEVICE_MEM, bar0Phys, barPages * 4096,
         RIGHT_READ or RIGHT_WRITE, 4);
      mintCap
        (xhciPID, CAP_DEVICE_MEM, 0, DMA_SIZE,
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

      --  Slot 3 belongs to devmgr and reaches only this xHCI process.
      grantEndpoint (myPID, xhciPID, DEVMGR_XHCI_SLOT, myPID);
      assignCPU (xhciPID, "xhci.drv");
      resumeProc (xhciPID);

      cfgMsg :=
        (tag => (label => OP_XHCI_CONFIGURE,
                 length => 4, flags => 0, badge => 0),
         capBadge => 0,
         words =>
           (0 => bar0Phys,
            1 => barPages,
            2 => dmaPhys,
            3 =>
              Unsigned_64 (XHCI_Interrupt_Mode'Enum_Rep (interruptMode)) or
              Shift_Left (XHCI_MSI_VECTOR, 8) or
              Shift_Left (msixTableOffset, 16)));
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

   --  Scan PCI bus for devices
   scanPCI;

   -----------------------------------------------------------------------
   -- Phase 1: Spawn filesystem server (auto-assign PID)
   -----------------------------------------------------------------------
   filesystemPID := spawnFromCpio ("filesystem.svc", 5, 0);
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

      --  Register early to discover our own PID
      ret := registerDriver (DRIVER_DEVMGR);
      myPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR);

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
   ataPID := spawnFromCpio ("ata.drv", 5);
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
   nvmePID := spawnFromCpio ("nvme.drv", 5);
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
      resumeProc (nvmePID);
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
   ps2PID := spawnFromCpio ("ps2.drv", 5);
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
      xhciPID := spawnFromCpio ("xhci.drv", 5);
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
   configPID := spawnFromCpio ("config.svc", 5);
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

      --  Grant FS endpoint to config.svc at slot 1 (for persistence)
      if configPID /= 0 and filesystemPID /= 0 then
         grantEndpoint (configPID, filesystemPID, 1, configPID);
         --  Grant FS ACL for config.svc (wildcard RW)
         sendWildcardACL (configPID);
      end if;

      --  Grant config ACL for devmgr itself (wildcard, so we can SET)
      if configPID /= 0 then
         sendWildcardACLConfig (myPID);
      end if;

      --  Seed config from system.conf in CPIO initrd
      if configPID /= 0 then
         declare
            OP_CONFIG_SET  : constant Unsigned_32 := 16#0601#;
            OP_CONFIG_LOAD : constant Unsigned_32 := 16#0604#;
            confIdx    : Natural;
            confAddr   : Unsigned_64;
            confSize   : Unsigned_64;
            --  Grant buffer for sending key=value to config.svc
            cfgRawAddr : Unsigned_64;
            cfgAligned : Unsigned_64;
            cfgBufAddr : System.Address;
            cfgGid     : Unsigned_64;
            cfgOk      : Boolean;
            cfgMsg     : Message;
         begin
            confIdx := Cpio.findFile (cpioArchive, "system.conf");
            if confIdx < cpioArchive.count then
               confAddr := INITRD_BASE +
                 Unsigned_64 (cpioArchive.files (confIdx).dataOff);
               confSize := Unsigned_64 (cpioArchive.files (confIdx).dataSize);

               --  Allocate grant buffer (2 pages for alignment)
               cfgRawAddr := syscall (SYSCALL_SBRK, 2 * 4096);
               if cfgRawAddr /= Unsigned_64'Last then
                  cfgAligned := (cfgRawAddr + 4095) and
                    not Unsigned_64 (4095);
                  cfgBufAddr := To_Address (Integer_Address (cfgAligned));

                  --  Create grant to config.svc (1 page RW)
                  createGrant
                    (grantee   => configPID,
                     localAddr => cfgBufAddr,
                     numPages  => 1,
                     readWrite => True,
                     grantId   => cfgGid,
                     success   => cfgOk);

                  if cfgOk then
                     --  Parse system.conf line by line from CPIO memory
                     declare
                        data : array (0 .. Natural (confSize) - 1)
                          of Unsigned_8
                          with Import,
                               Address => To_Address (
                                 Integer_Address (confAddr));
                        pos      : Natural := 0;
                        lineStart : Natural;
                        eol      : Natural;
                        eqPos    : Integer;
                     begin
                        while pos < Natural (confSize) loop
                           lineStart := pos;

                           --  Find end of line
                           eol := pos;
                           while eol < Natural (confSize)
                             and then data (eol) /= 16#0A#
                           loop
                              eol := eol + 1;
                           end loop;

                           declare
                              lineEnd : Natural := eol;
                           begin
                              --  Trim CR
                              if lineEnd > lineStart
                                and then data (lineEnd - 1) = 16#0D#
                              then
                                 lineEnd := lineEnd - 1;
                              end if;

                              --  Advance past newline
                              if eol < Natural (confSize) then
                                 pos := eol + 1;
                              else
                                 pos := Natural (confSize);
                              end if;

                              --  Skip blank lines and comments
                              if lineEnd > lineStart
                                and then data (lineStart) /= 16#23#
                              then
                                 --  Find '='
                                 eqPos := -1;
                                 for e in lineStart .. lineEnd - 1 loop
                                    if data (e) = 16#3D# then
                                       eqPos := e;
                                       exit;
                                    end if;
                                 end loop;

                                 if eqPos > Integer (lineStart)
                                   and eqPos < Integer (lineEnd)
                                 then
                                    declare
                                       kLen : constant Natural :=
                                         Natural (eqPos) - lineStart;
                                       vLen : constant Natural :=
                                         lineEnd - Natural (eqPos) - 1;
                                       gBuf : array (0 .. kLen + vLen - 1)
                                         of Unsigned_8
                                         with Import,
                                              Address => cfgBufAddr;
                                    begin
                                       --  Copy key to grant buf
                                       for c in 0 .. kLen - 1 loop
                                          gBuf (c) :=
                                            data (lineStart + c);
                                       end loop;
                                       --  Copy value after key
                                       for c in 0 .. vLen - 1 loop
                                          gBuf (kLen + c) := data (
                                            Natural (eqPos) + 1 + c);
                                       end loop;

                                       --  Send OP_CONFIG_SET
                                       cfgMsg :=
                                         (tag => (label  => OP_CONFIG_SET,
                                                  length => 3,
                                                  flags  => 0,
                                                  badge  => 0),
                                          capBadge => 0,
                                          words =>
                                            (0 => cfgGid,
                                             1 => Unsigned_64 (kLen),
                                             2 => Unsigned_64 (vLen),
                                             others => 0));
                                       cfgMsg.tag :=
                                         capCall (2, cfgMsg);
                                    end;
                                 end if;
                              end if;
                           end;
                        end loop;
                     end;

                     debugPrint (
                       "devmgr: system.conf seeded into config" & LF);

                     --  Send OP_CONFIG_LOAD to trigger disk load
                     cfgMsg :=
                       (tag => (label  => OP_CONFIG_LOAD,
                                length => 0,
                                flags  => 0,
                                badge  => 0),
                        capBadge => 0,
                        words => (others => 0));
                     cfgMsg.tag := capCall (2, cfgMsg);
                  end if;
               end if;
            else
               debugPrint (
                 "devmgr: system.conf not found in CPIO" & LF);
            end if;
         end;
      end if;
   end if;

   -----------------------------------------------------------------------
   -- Phase 2d: Spawn VirtIO-GPU driver when QEMU/hardware exposes it
   -----------------------------------------------------------------------
   virtioGpuPID := spawnFromCpio ("virtio-gpu.drv", 5);
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
   netstackPID := spawnFromCpio ("netstack.svc", 5);
   if netstackPID = reterr then
      netstackPID := 0;
   end if;

   --  Virtio-net driver
   virtioNetPID := spawnFromCpio ("virtio-net.drv", 5);
   if virtioNetPID = reterr then
      virtioNetPID := 0;
   end if;

   --  Grant cross-endpoints between netstack and virtio-net
   if netstackPID /= 0 and virtioNetPID /= 0 then
      setupVirtioNet;

      --  Netstack slot 10 -> virtio-net driver
      grantEndpoint (netstackPID, virtioNetPID, 10, netstackPID);

      --  Virtio-net slot 7 -> netstack service
      grantEndpoint (virtioNetPID, netstackPID, 7, virtioNetPID);

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

      assignCPU (virtioNetPID, "virtio-net.drv");
      mintCap (virtioNetPID, CAP_ENDPOINT, myPID, 0,
               RIGHT_READ or RIGHT_WRITE, CAP_SLOT_READY);
      resumeProc (virtioNetPID);
      debugPrint ("devmgr: virtio-net driver started" & LF);

      if not waitReady (virtioNetPID) then
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
   netmgrPID := spawnFromCpio ("netmgr.svc", 5);
   if netmgrPID = reterr then
      netmgrPID := 0;
   end if;
   if netmgrPID /= 0 and netstackPID /= 0 then
      --  Slot 4: endpoint to netstack (config + raw UDP IPC)
      grantEndpoint (netmgrPID, netstackPID, 4, netmgrPID);

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

   --  HDA driver
   hdaPID := spawnFromCpio ("hda.drv", 5);
   if hdaPID = reterr then
      hdaPID := 0;
   end if;

   --  Mixer service
   mixerPID := spawnFromCpio ("mixer.svc", 5);
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
   procmgrPID := spawnFromCpio ("procmgr.svc", 5);
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
   debugPrint ("devmgr: startup complete, entering service loop" & LF);

   loop
      receive (from, msg);
      --  Reject unknown messages; devmgr has no runtime API yet
      debugPrint ("devmgr: rejected unknown message" & LF);
      ret := Unsigned_64 (reply (from,
                                 (tag => (label  => REPLY_ERR,
                                          length => 0,
                                          flags  => 0,
                                          badge  => 0),
                                  capBadge => 0,
                                  words => (others => 0))));
   end loop;

end main;
