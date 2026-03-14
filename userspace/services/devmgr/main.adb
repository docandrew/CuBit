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

   --  Well-known filesystem server PID (must match kernel config)
   SERVICE_FILESYSTEM_PID : constant Unsigned_64 := 10;

   --  Address where kernel mapped the initrd into our space
   INITRD_BASE : constant Unsigned_64 := 16#0000_5000_0000_0000#;

   --  DMA virtual base for drivers
   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;

   --  PCI config I/O ports
   PCI_CONFIG_ADDR : constant Unsigned_16 := 16#0CF8#;
   PCI_CONFIG_DATA : constant Unsigned_16 := 16#0CFC#;

   --  PCI class codes
   CLASS_STORAGE_IDE   : constant Unsigned_16 := 16#0101#;
   CLASS_STORAGE_NVME  : constant Unsigned_16 := 16#0108#;
   CLASS_NET_ETHERNET  : constant Unsigned_16 := 16#0200#;
   CLASS_MULTIMEDIA_HDA : constant Unsigned_16 := 16#0403#;

   --  PCI vendor ID for virtio devices
   VENDOR_VIRTIO       : constant Unsigned_16 := 16#1AF4#;

   --  PCI config space offsets
   PCI_VENDOR_ID       : constant Unsigned_8 := 0;
   PCI_COMMAND         : constant Unsigned_8 := 4;
   PCI_CLASS_DEVICE    : constant Unsigned_8 := 10;
   PCI_BASEADDR_0      : constant Unsigned_8 := 16;
   PCI_BASEADDR_1      : constant Unsigned_8 := 20;
   PCI_INTERRUPT_LINE  : constant Unsigned_8 := 60;

   --  Capability types (must match kernel/src/capabilities.ads)
   CAP_ENDPOINT     : constant Unsigned_64 := 1;
   CAP_NOTIFICATION : constant Unsigned_64 := 2;
   CAP_IOPORT       : constant Unsigned_64 := 4;
   CAP_IRQ          : constant Unsigned_64 := 5;
   CAP_PROCESS      : constant Unsigned_64 := 6;
   CAP_DEVICE_MEM   : constant Unsigned_64 := 7;

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
   configPID     : Unsigned_64 := 0;
   netmgrPID     : Unsigned_64 := 0;

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
      vendorID  : Unsigned_16;
      classCode : Unsigned_16;
   begin
      for bus in Unsigned_8 range 0 .. 0 loop
         for pSlot in Unsigned_8 range 0 .. 31 loop
            vendorID := pciReadConfig16 (bus, pSlot, 0, PCI_VENDOR_ID);
            if vendorID /= 16#FFFF# then
               --  class(byte 11) << 8 | subclass(byte 10)
               classCode := Unsigned_16 (
                   Shift_Right (pciReadConfig32 (bus, pSlot, 0,
                                                 PCI_CLASS_DEVICE), 16)
                   and 16#FFFF#);

               if classCode = CLASS_STORAGE_NVME then
                  nvmeDev := (found => True, bus => bus,
                              slot => pSlot, func => 0);
                  debugPrint ("devmgr: found NVMe at PCI " & LF);
               elsif classCode = CLASS_STORAGE_IDE then
                  ataDev := (found => True, bus => bus,
                             slot => pSlot, func => 0);
                  debugPrint ("devmgr: found IDE at PCI" & LF);
               elsif classCode = CLASS_NET_ETHERNET
                     and vendorID = VENDOR_VIRTIO then
                  netDev := (found => True, bus => bus,
                             slot => pSlot, func => 0);
                  debugPrint ("devmgr: found virtio-net at PCI" & LF);
               elsif classCode = CLASS_MULTIMEDIA_HDA then
                  hdaDev := (found => True, bus => bus,
                             slot => pSlot, func => 0);
                  debugPrint ("devmgr: found HDA at PCI" & LF);
               end if;
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
      ret := syscall (SYSCALL_MINT_CAP,
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
         strEq (name, "ps2.drv")
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

      DMA_ORDER : constant Unsigned_64 := 5;   --  32 pages = 128KB
      DMA_PAGES : constant Unsigned_64 := 32;
      DMA_SIZE  : constant Unsigned_64 := DMA_PAGES * 4096;
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

      --  Enable IRQ routing
      ret := enableIrq (irqVector, hdaPID, 0);

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
   -- Phase 1: Spawn filesystem server (well-known PID)
   -----------------------------------------------------------------------
   filesystemPID := spawnFromCpio ("filesystem.svc", 5,
                                   SERVICE_FILESYSTEM_PID);
   if filesystemPID = reterr then
      filesystemPID := 0;
      debugPrint ("devmgr: filesystem.svc spawn failed" & LF);
   end if;

   if filesystemPID /= 0 then
      --  Map initrd into FS server for serving ramdisk files
      --  Get the physical address: kernel mapped initrd at INITRD_BASE in
      --  our space, but the physical address was stored for us.
      initrdPhys := virtToPhys (To_Address (Integer_Address (INITRD_BASE)));
      initrdPages := (initrdSize + 4095) / 4096;

      ret := mapInto (filesystemPID, initrdPhys, INITRD_BASE,
                      initrdPages, MAP_FLAG_RO);
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
   -- Phase 2c: Spawn config store service
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
      --  Grant CAP_PROCESS (READ+EXECUTE+GRANT) at slot 4
      mintCap (procmgrPID, CAP_PROCESS, 0, 0,
               RIGHT_READ or RIGHT_EXECUTE or RIGHT_GRANT, 4);

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
