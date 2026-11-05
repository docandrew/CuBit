------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed launch configuration: what a spawner tells one child it starts,
--  installed before the child's first instruction (docs/syscall-retirement.md).
--
--  @description
--  The spawner installs the encoded words with
--  SYSCALL_INSTALL_LAUNCH_ARGUMENTS while the child is suspended and never
--  started; the kernel maps them read-only at Block_Address in that child
--  only, and starts it with RDI = the byte length. For devmgr, the first
--  process, the kernel installs its Device_Manager configuration itself.
--  It replaces the global SYSCALL_INFO / SET_SYSINFO board: a driver's
--  registers and DMA addresses are visible to that driver, nobody else.
--
--  Wire form, little-endian 64-bit words:
--
--     0  Magic ("CUBITCFG"; never a launch-argument format version)
--     1  Configuration_Kind
--     2  field count (Field_Count (Kind))
--     3  the fields, in the order of the Kind's record below
--
--  devmgr's configuration is the one variable-length kind: the kernel
--  reports one handle per device-memory and firmware-memory root it finds
--  in the boot classification, however many there are (LIM-001). Its two
--  root counts lead its fields, so a reader knows the length before any
--  root: Device_Manager_Fields (Device_Roots, Firmware_Roots) fields.
--
--  Decoding is total and proved free of run-time errors; a decoder refuses
--  any block that is not a header for its kind followed by in-range fields.
--  tests/launch-configuration proves that decoding an encoder's output
--  returns the value unchanged (Decode (Encode (X)) = X).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Launch_Configuration with Pure, SPARK_Mode is

   --  Where the kernel maps a launch block (kernel Process_Launch).
   Block_Address : constant := 16#0000_5A00_0000_0000#;
   --  Where devmgr, the filesystem and the RAM block driver see the boot
   --  image (initrd), read-only (kernel Modules, devmgr MAP_INTO).
   Boot_Image_Address : constant := 16#0000_5000_0000_0000#;

   Magic : constant Unsigned_64 := 16#4746_4354_4942_5543#;  --  "CUBITCFG"
   Word_Bytes   : constant := 8;
   Header_Words : constant := 3;
   --  The most fields of a fixed-length kind (Generic_PCI's), and of
   --  devmgr's configuration before its roots.
   Maximum_Fields : constant := 23;
   Maximum_Words  : constant := Header_Words + Maximum_Fields;
   --  The largest block: the kernel's ceiling on a launch configuration
   --  (Process_Launch, 64 KiB). A named ceiling, not a count of anything.
   Maximum_Block_Bytes : constant := 64 * 1024;
   Maximum_Block_Words : constant := Maximum_Block_Bytes / Word_Bytes;

   subtype Word_Count is Natural range 0 .. Maximum_Block_Words;
   subtype Word_Index is Positive range 1 .. Maximum_Block_Words;
   type Word_Array is array (Word_Index range <>) of Unsigned_64;

   --  A fixed-length kind's block (and devmgr's prefix) fits Maximum_Words.
   subtype Fixed_Word_Count is Word_Count range 0 .. Maximum_Words;
   subtype Fixed_Word_Index is Word_Index range 1 .. Maximum_Words;

   --  An encoded configuration: Words (1 .. Length).
   type Encoded is record
      Words  : Word_Array (Fixed_Word_Index) := [others => 0];
      Length : Fixed_Word_Count := 0;
   end record;

   --  x86-64 physical addresses have at most 52 bits.
   Physical_Bits : constant := 52;
   subtype Physical_Address is Unsigned_64 range 0 .. 2 ** Physical_Bits - 1;
   subtype Byte_Count is Unsigned_64 range 0 .. 2 ** Physical_Bits;
   --  A register offset or multiplier inside one BAR (virtio's are u32).
   subtype Register_Offset is Unsigned_64 range 0 .. 2 ** 32 - 1;

   --  A PCI function's location: bus << 8 | device << 3 | function (the
   --  PCI routing ID).
   subtype Routing_ID is Unsigned_64 range 0 .. 2 ** 16 - 1;

   --  "BB:DD.F" in hexadecimal, as lspci and devmgr print a location: how
   --  a driver started per device names its instance in its log lines.
   subtype Location_Text is String (1 .. 7);
   function Location_Image (Location : Routing_ID) return Location_Text;

   --  Physical pages: First and Bytes, or Bytes = 0 for none.
   type Physical_Span is record
      First : Physical_Address := 0;
      Bytes : Byte_Count := 0;
   end record;

   type Configuration_Kind is
     (Device_Manager, Process_Manager, Boot_Image_Reader, NVMe_Controller,
      HDA_Controller, Virtio_Input, Virtio_GPU, Generic_PCI, Network_Device,
      XHCI_Controller);
   for Configuration_Kind use
     (Device_Manager    => 1,
      Process_Manager   => 2,
      Boot_Image_Reader => 3,
      NVMe_Controller   => 4,
      HDA_Controller    => 5,
      Virtio_Input      => 7,
      Virtio_GPU        => 8,
      Generic_PCI       => 9,
      Network_Device    => 10,
      XHCI_Controller   => 11);

   --  The kinds whose configuration is a Controller_Configuration.
   function Is_Controller (Kind : Configuration_Kind) return Boolean is
     (Kind in NVMe_Controller | HDA_Controller | XHCI_Controller);

   --  devmgr's capability handles, placed by the kernel before it runs
   --  (CAP-002): handle words in its own table, never names or slot
   --  numbers. Self/Self_Process: its own endpoint and process.
   --  Self_Grantable: an untagged endpoint to itself it may badge and pass
   --  on (children's "ready" endpoints). The rest are the firmware/bus/
   --  object roots everything devmgr hands out is narrowed from: spawn and
   --  process control (CAP_PROCESS, all processes), PCI configuration
   --  ports, its own registration, the device-vector authority, the I/O
   --  port space, the legacy interrupt lines, every service role's
   --  notification, real-time scheduling, reading the sealed firmware-table
   --  snapshot (FIRMWARE_SNAPSHOT). After these named handles, the block
   --  carries one device-memory root per maximal run of device-mappable
   --  pages, ascending, then the firmware-memory roots (kernel
   --  Physical_Ranges.Next_Firmware_Run): ACPI NVS runs, the FACS page,
   --  the PCI configuration slice and table-named firmware data, each
   --  tagged with its kind (Capability_ABI.Firmware_*_Tag); devmgr claims
   --  them for the ACPI hardware grant. As many of each as the kernel
   --  found: the counts lead the fields.
   type Device_Manager_Handles is record
      Self              : Unsigned_64 := 0;
      Self_Process      : Unsigned_64 := 0;
      Self_Grantable    : Unsigned_64 := 0;
      Process_Root      : Unsigned_64 := 0;
      PCI_Ports         : Unsigned_64 := 0;
      Registration      : Unsigned_64 := 0;
      Vector_Authority  : Unsigned_64 := 0;
      IO_Ports          : Unsigned_64 := 0;
      Legacy_Interrupts : Unsigned_64 := 0;
      Notifications     : Unsigned_64 := 0;
      Scheduling        : Unsigned_64 := 0;
      Firmware_Snapshot : Unsigned_64 := 0;
   end record;
   Device_Manager_Handle_Fields : constant := 12;

   --  A handle word: its generation (high half) is never zero.
   function Is_Handle (Word : Unsigned_64) return Boolean is
     (Shift_Right (Word, 32) /= 0);

   --  Every named handle is one.
   function Handles_Well_Formed (H : Device_Manager_Handles) return Boolean is
     (Is_Handle (H.Self) and then Is_Handle (H.Self_Process)
      and then Is_Handle (H.Self_Grantable) and then Is_Handle (H.Process_Root)
      and then Is_Handle (H.PCI_Ports) and then Is_Handle (H.Registration)
      and then Is_Handle (H.Vector_Authority) and then Is_Handle (H.IO_Ports)
      and then Is_Handle (H.Legacy_Interrupts) and then Is_Handle (H.Notifications)
      and then Is_Handle (H.Scheduling) and then Is_Handle (H.Firmware_Snapshot));

   --  devmgr's words: the header, the two root counts, the boot image,
   --  the framebuffer (first, bytes), the named handles, then one word per
   --  device root and one per firmware root.
   Device_Roots_Word      : constant := Header_Words + 1;
   Firmware_Roots_Word    : constant := Header_Words + 2;
   Boot_Image_Word        : constant := Header_Words + 3;
   Framebuffer_First_Word : constant := Header_Words + 4;
   Framebuffer_Bytes_Word : constant := Header_Words + 5;
   First_Handle_Word      : constant := Header_Words + 6;
   Device_Manager_Prefix_Fields : constant := 5 + Device_Manager_Handle_Fields;
   Device_Manager_Prefix_Words  : constant := Header_Words + Device_Manager_Prefix_Fields;
   --  The roots one block can carry, together. Counts are words, as on
   --  the wire, so a decoded count is the word itself.
   Maximum_Roots : constant := Maximum_Block_Words - Device_Manager_Prefix_Words;
   subtype Root_Count is Unsigned_64 range 0 .. Maximum_Roots;
   subtype Root_Index is Positive range 1 .. Maximum_Roots;

   function Roots_Fit (Device_Roots, Firmware_Roots : Root_Count) return Boolean is
     (Device_Roots <= Maximum_Roots - Firmware_Roots);

   function Device_Manager_Fields
     (Device_Roots, Firmware_Roots : Root_Count) return Unsigned_64
   is (Device_Manager_Prefix_Fields + Device_Roots + Firmware_Roots)
   with Pre  => Roots_Fit (Device_Roots, Firmware_Roots),
        Post => Device_Manager_Fields'Result <= Maximum_Block_Words - Header_Words;

   --  The whole block's length in words.
   function Device_Manager_Last
     (Device_Roots, Firmware_Roots : Root_Count) return Word_Index
   is (Word_Index (Header_Words + Device_Manager_Fields (Device_Roots, Firmware_Roots)))
   with Pre => Roots_Fit (Device_Roots, Firmware_Roots);

   --  Where device root I's handle is, and firmware root I's after
   --  Device_Roots device roots.
   function Device_Root_Word (I : Root_Index) return Word_Index is
     (Device_Manager_Prefix_Words + I);
   function Firmware_Root_Word (Device_Roots : Root_Count; I : Root_Index) return Word_Index is
     (Word_Index (Device_Manager_Prefix_Words + Device_Roots + Unsigned_64 (I)))
   with Pre => Unsigned_64 (I) <= Maximum_Roots - Device_Roots;

   --  Kernel to devmgr: how many roots of each kind follow, the boot
   --  image's size, the boot framebuffer's physical pages, and its named
   --  handles.
   type Device_Manager_Configuration is record
      Device_Roots   : Root_Count := 0;
      Firmware_Roots : Root_Count := 0;
      Boot_Image     : Byte_Count := 0;
      Framebuffer    : Physical_Span;
      Handles        : Device_Manager_Handles;
   end record;

   function Device_Manager_Length (Value : Device_Manager_Configuration) return Word_Index is
     (Device_Manager_Last (Value.Device_Roots, Value.Firmware_Roots))
   with Pre => Roots_Fit (Value.Device_Roots, Value.Firmware_Roots);

   --  procmgr's handles, installed by devmgr (its launcher) before it runs
   --  (CAP-002): handle words in procmgr's table. procmgr launches every
   --  other program and hands them narrowed copies of these: endpoints it
   --  may badge (untagged, Grant) to the services devmgr started, its own
   --  endpoint, the service-role notifications, process authority, the
   --  boot framebuffer (zero when there is none).
   type Process_Manager_Handles is record
      Self             : Unsigned_64 := 0;
      Self_Process     : Unsigned_64 := 0;
      Self_Grantable   : Unsigned_64 := 0;
      Process_Root     : Unsigned_64 := 0;
      Registration     : Unsigned_64 := 0;
      Notifications    : Unsigned_64 := 0;
      Filesystem       : Unsigned_64 := 0;
      Config           : Unsigned_64 := 0;
      Netstack         : Unsigned_64 := 0;
      Network_Policy   : Unsigned_64 := 0;
      Mixer            : Unsigned_64 := 0;
      Device_Manager   : Unsigned_64 := 0;
      Ready            : Unsigned_64 := 0;
      Framebuffer      : Unsigned_64 := 0;
      --  devmgr's untagged endpoint with Grant: procmgr copies it, without
      --  Grant, to programs that ask for "device-manager" (inspection).
      Device_Inspection : Unsigned_64 := 0;
      --  The firmware snapshot reader, with Grant: procmgr installs a
      --  read-only copy in a program whose manifest declares it and whose
      --  startup entry approves it (Firmware_Snapshot_ABI).
      Firmware_Snapshot : Unsigned_64 := 0;
   end record;
   Process_Manager_Handle_Fields : constant := 16;

   --  The handles every procmgr needs are handles; the others (mixer, the
   --  framebuffer) are a handle or zero.
   function Process_Handles_Well_Formed (H : Process_Manager_Handles) return Boolean is
     (Is_Handle (H.Self) and then Is_Handle (H.Self_Process)
      and then Is_Handle (H.Self_Grantable) and then Is_Handle (H.Process_Root)
      and then Is_Handle (H.Registration) and then Is_Handle (H.Notifications)
      and then Is_Handle (H.Filesystem) and then Is_Handle (H.Config)
      and then Is_Handle (H.Netstack) and then Is_Handle (H.Network_Policy)
      and then (H.Mixer = 0 or else Is_Handle (H.Mixer))
      and then Is_Handle (H.Device_Manager) and then Is_Handle (H.Ready)
      and then (H.Framebuffer = 0 or else Is_Handle (H.Framebuffer))
      and then Is_Handle (H.Device_Inspection)
      and then Is_Handle (H.Firmware_Snapshot));

   --  devmgr to procmgr.
   type Process_Manager_Configuration is record
      Framebuffer : Physical_Span;
      Handles     : Process_Manager_Handles;
   end record;

   --  devmgr to the filesystem and the RAM block driver: the size of the
   --  boot image mapped at Boot_Image_Address.
   type Boot_Image_Configuration is record
      Boot_Image : Byte_Count := 0;
   end record;

   --  devmgr to NVMe, HDA and xHCI: the register BAR, the DMA region devmgr
   --  allocated for the driver, and the PCI location of the controller it
   --  drives (devmgr starts one driver per controller; the location names
   --  the instance in its log lines).
   type Controller_Configuration is record
      Registers : Physical_Address := 0;
      DMA       : Physical_Address := 0;
      Location  : Routing_ID := 0;
   end record;

   --  devmgr to virtio-input: its DMA region (queues and buffers), which
   --  devmgr allocated at the driver's fixed address.
   type Queue_Configuration is record
      DMA : Physical_Address := 0;
   end record;

   --  devmgr to a network driver (virtio-net): its DMA region, and where
   --  its device is, which names the interface (docs/net-link.md).
   type Network_Device_Configuration is record
      DMA      : Physical_Address := 0;
      Location : Routing_ID := 0;
   end record;

   --  devmgr to virtio-gpu: the modern transport and its DMA banks.
   type Virtio_GPU_Configuration is record
      Registers         : Physical_Address := 0;
      DMA               : Physical_Address := 0;
      Second_DMA        : Physical_Address := 0;   --  0: one bank
      DMA_Bytes         : Byte_Count := 0;         --  all banks together
      Common_Offset     : Register_Offset := 0;
      Notify_Offset     : Register_Offset := 0;
      ISR_Offset        : Register_Offset := 0;
      Device_Offset     : Register_Offset := 0;
      Notify_Multiplier : Register_Offset := 0;
      --  It drives the boot display adapter (the firmware framebuffer).
      Boot_Display      : Boolean := False;
   end record;

   --  devmgr to a PCI driver it launched by its declared match with no
   --  driver-specific setup (HW-002, docs/device-matching.md "Generic PCI
   --  launch"). Authority is never here: each register window's
   --  device-memory capability and the interrupt capability are the
   --  driver's startup entries (Startup.<binding>). This says where things
   --  are: the driver maps window K, the K-th device-memory request of its
   --  manifest, with Map_Device (Startup.<that binding>, Windows (K).First,
   --  a virtual address of its choice, pages), and the kernel refuses any
   --  page outside the capability.
   --
   --  Interrupts: MSI-X entries are programmed and stay masked, and MSI is
   --  programmed and enabled (it has no mask), until the driver sends
   --  OP_READY to its "device-manager" endpoint; devmgr then unmasks. So
   --  the driver arms its handler on the interrupt capability first, then
   --  reports ready, then enables interrupts in the device. OP_NOT_PRESENT
   --  instead declines the device: devmgr masks and releases the vectors.
   --  A PCI type-0 header has six base address registers (PCI Local Bus
   --  3.0, 6.2.5.1): one window per BAR at most.
   PCI_Type0_BAR_Count : constant := 6;
   Maximum_Windows : constant := PCI_Type0_BAR_Count;
   subtype Window_Count is Natural range 0 .. Maximum_Windows;
   subtype Window_Index is Positive range 1 .. Maximum_Windows;
   type Window_Array is array (Window_Index) of Physical_Span;
   subtype PCI_ID is Unsigned_64 range 0 .. 2 ** 16 - 1;
   subtype PCI_Revision is Unsigned_64 range 0 .. 2 ** 8 - 1;
   --  base class << 16 | subclass << 8 | programming interface.
   subtype PCI_Class_Code is Unsigned_64 range 0 .. 2 ** 24 - 1;
   --  Lower-half canonical user addresses (x86-64, 4-level paging).
   subtype Virtual_Address is Unsigned_64 range 0 .. 2 ** 47 - 1;
   type Interrupt_Delivery is (Polled, Line, MSI, MSI_X);
   for Interrupt_Delivery use (Polled => 0, Line => 1, MSI => 2, MSI_X => 3);
   Maximum_Vectors : constant := 32;
   subtype Vector_Count is Unsigned_64 range 0 .. Maximum_Vectors;

   type Generic_PCI_Configuration is record
      Location    : Routing_ID := 0;
      Vendor      : PCI_ID := 0;
      Device      : PCI_ID := 0;
      Revision    : PCI_Revision := 0;
      Class_Code  : PCI_Class_Code := 0;
      --  The granted part of each declared window: at most its BAR's size
      --  and the request's max_bytes. Unused windows are (0, 0).
      Windows     : Window_Array := [others => (0, 0)];
      Window_Total : Window_Count := 0;
      --  The DMA region devmgr allocated for the manifest's dma request,
      --  mapped at DMA_Virtual in the driver ((0, 0) and 0: none).
      DMA         : Physical_Span := (0, 0);
      DMA_Virtual : Virtual_Address := 0;
      --  How the interrupt request was met, and how many vectors
      --  (Polled: 0; Line and MSI: 1; MSI_X: 1 .. 32).
      Delivery    : Interrupt_Delivery := Polled;
      Vectors     : Vector_Count := 0;
   end record;

   function Valid_Span (Span : Physical_Span) return Boolean is
     (Span.Bytes <= Byte_Count'Last - Span.First);
   function Valid_Vectors (Delivery : Interrupt_Delivery; Vectors : Vector_Count)
     return Boolean is
     (case Delivery is
         when Polled => Vectors = 0,
         when Line | MSI => Vectors = 1,
         when MSI_X => Vectors >= 1);
   function Valid_Generic (Value : Generic_PCI_Configuration) return Boolean is
     ((for all K in Window_Index =>
         Valid_Span (Value.Windows (K))
         and then (if K > Value.Window_Total then Value.Windows (K) = (0, 0)))
      and then Valid_Span (Value.DMA)
      and then (if Value.DMA.Bytes = 0 then Value.DMA_Virtual = 0)
      and then Valid_Vectors (Value.Delivery, Value.Vectors));

   --  A fixed-length kind's field count (devmgr's before its roots).
   function Field_Count (Kind : Configuration_Kind) return Natural is
     (case Kind is
         when Device_Manager    => Device_Manager_Prefix_Fields,
         when Process_Manager   => 2 + Process_Manager_Handle_Fields,
         when Boot_Image_Reader => 1,
         when NVMe_Controller | HDA_Controller => 3,
         when Virtio_Input      => 1,
         when Virtio_GPU        => 10,
         when Generic_PCI       => 5 + 1 + 2 * Maximum_Windows + 3 + 2,
         when Network_Device    => 2,
         when XHCI_Controller   => 3)
   with Post => Field_Count'Result <= Maximum_Fields;

   --  Words (1 .. Length) is a header for fixed-length Kind with the
   --  right length (devmgr's: Device_Manager_Valid).
   function Header_Valid
     (Words : Word_Array; Kind : Configuration_Kind) return Boolean
   is (Kind /= Device_Manager
       and then Words'First = 1
       and then Words'Length = Header_Words + Field_Count (Kind)
       and then Words (1) = Magic
       and then Words (2) = Configuration_Kind'Enum_Rep (Kind)
       and then Words (3) = Unsigned_64 (Field_Count (Kind)));

   --  Encoders: the header for the kind, then the fields in order.
   --  devmgr's named handles' words, in field order.
   function Handle_Words (H : Device_Manager_Handles) return Word_Array is
     [H.Self, H.Self_Process, H.Self_Grantable, H.Process_Root, H.PCI_Ports,
      H.Registration, H.Vector_Authority, H.IO_Ports, H.Legacy_Interrupts,
      H.Notifications, H.Scheduling, H.Firmware_Snapshot]
   with Post => Handle_Words'Result'First = 1
                and then Handle_Words'Result'Length = Device_Manager_Handle_Fields;

   --  Words is a whole devmgr block: its header, counts that fit and match
   --  its length, in-range fields, and a handle in every named handle and
   --  every root word.
   function Device_Manager_Valid (Words : Word_Array) return Boolean is
     (Words'First = 1
      and then Words'Length >= Device_Manager_Prefix_Words
      and then Words (1) = Magic
      and then Words (2) = Configuration_Kind'Enum_Rep (Device_Manager)
      and then Words (Device_Roots_Word) <= Maximum_Roots
      and then Words (Firmware_Roots_Word) <= Maximum_Roots
      and then Roots_Fit (Words (Device_Roots_Word), Words (Firmware_Roots_Word))
      and then Words (3) = Device_Manager_Fields (Words (Device_Roots_Word),
                                                  Words (Firmware_Roots_Word))
      and then Words'Last = Device_Manager_Last (Words (Device_Roots_Word),
                                                 Words (Firmware_Roots_Word))
      and then Words (Boot_Image_Word) in Byte_Count
      and then Words (Framebuffer_First_Word) in Physical_Address
      and then Words (Framebuffer_Bytes_Word) in Byte_Count
      and then (for all I in First_Handle_Word .. Words'Last => Is_Handle (Words (I))));

   --  devmgr's block up to its roots; the caller appends Device_Roots then
   --  Firmware_Roots handle words (Device_Root_Word, Firmware_Root_Word).
   function Encode_Prefix (Value : Device_Manager_Configuration) return Encoded
   with Pre  => Roots_Fit (Value.Device_Roots, Value.Firmware_Roots),
        Post => Encode_Prefix'Result.Length = Device_Manager_Prefix_Words
                and then Encode_Prefix'Result.Words (1) = Magic
                and then Encode_Prefix'Result.Words (2) =
                           Configuration_Kind'Enum_Rep (Device_Manager)
                and then Encode_Prefix'Result.Words (3) =
                           Device_Manager_Fields (Value.Device_Roots, Value.Firmware_Roots)
                and then Encode_Prefix'Result.Words (Device_Roots_Word) = Value.Device_Roots
                and then Encode_Prefix'Result.Words (Firmware_Roots_Word) = Value.Firmware_Roots
                and then Encode_Prefix'Result.Words (Boot_Image_Word) = Value.Boot_Image
                and then Encode_Prefix'Result.Words (Framebuffer_First_Word) =
                           Value.Framebuffer.First
                and then Encode_Prefix'Result.Words (Framebuffer_Bytes_Word) =
                           Value.Framebuffer.Bytes
                and then (for all I in 1 .. Device_Manager_Handle_Fields =>
                            Encode_Prefix'Result.Words (First_Handle_Word - 1 + I) =
                              Handle_Words (Value.Handles) (I));

   --  The handles' words, in field order (Words (6 .. 21)).
   function Process_Handle_Words (H : Process_Manager_Handles) return Word_Array is
     [H.Self, H.Self_Process, H.Self_Grantable, H.Process_Root, H.Registration,
      H.Notifications, H.Filesystem, H.Config, H.Netstack, H.Network_Policy,
      H.Mixer, H.Device_Manager, H.Ready, H.Framebuffer, H.Device_Inspection,
      H.Firmware_Snapshot]
   with Post => Process_Handle_Words'Result'First = 1
                and then Process_Handle_Words'Result'Length = Process_Manager_Handle_Fields;

   function Encode (Value : Process_Manager_Configuration) return Encoded
   with Post => Encode'Result.Length = Header_Words + Field_Count (Process_Manager)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Process_Manager)
                and then Encode'Result.Words (4) = Value.Framebuffer.First
                and then Encode'Result.Words (5) = Value.Framebuffer.Bytes
                and then (for all I in 1 .. Process_Manager_Handle_Fields =>
                            Encode'Result.Words (5 + I) =
                              Process_Handle_Words (Value.Handles) (I));
   function Encode (Value : Boot_Image_Configuration) return Encoded
   with Post => Encode'Result.Length = Header_Words + Field_Count (Boot_Image_Reader)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Boot_Image_Reader)
                and then Encode'Result.Words (4) = Value.Boot_Image;
   function Encode
     (Value : Controller_Configuration; Kind : Configuration_Kind) return Encoded
   with Pre  => Is_Controller (Kind),
        Post => Encode'Result.Length = Header_Words + Field_Count (Kind)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length), Kind)
                and then Encode'Result.Words (4) = Value.Registers
                and then Encode'Result.Words (5) = Value.DMA
                and then Encode'Result.Words (6) = Value.Location;
   function Encode
     (Value : Queue_Configuration; Kind : Configuration_Kind) return Encoded
   with Pre  => Kind in Virtio_Input,
        Post => Encode'Result.Length = Header_Words + Field_Count (Kind)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length), Kind)
                and then Encode'Result.Words (4) = Value.DMA;
   function Encode (Value : Network_Device_Configuration) return Encoded
   with Post => Encode'Result.Length = Header_Words + Field_Count (Network_Device)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Network_Device)
                and then Encode'Result.Words (4) = Value.DMA
                and then Encode'Result.Words (5) = Value.Location;
   function Encode (Value : Virtio_GPU_Configuration) return Encoded
   with Post => Encode'Result.Length = Header_Words + Field_Count (Virtio_GPU)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Virtio_GPU)
                and then Encode'Result.Words (4) = Value.Registers
                and then Encode'Result.Words (5) = Value.DMA
                and then Encode'Result.Words (6) = Value.Second_DMA
                and then Encode'Result.Words (7) = Value.DMA_Bytes
                and then Encode'Result.Words (8) = Value.Common_Offset
                and then Encode'Result.Words (9) = Value.Notify_Offset
                and then Encode'Result.Words (10) = Value.ISR_Offset
                and then Encode'Result.Words (11) = Value.Device_Offset
                and then Encode'Result.Words (12) = Value.Notify_Multiplier
                and then Encode'Result.Words (13) = (if Value.Boot_Display then 1 else 0);

   function Encode (Value : Generic_PCI_Configuration) return Encoded
   with Pre  => Valid_Generic (Value),
        Post => Encode'Result.Length = Header_Words + Field_Count (Generic_PCI)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Generic_PCI);

   --  Decoders: Valid exactly when Words is a header for the kind followed
   --  by in-range fields; then Value holds the fields.
   procedure Decode
     (Words : Word_Array; Value : out Device_Manager_Configuration; Valid : out Boolean)
   with Post => Valid = Device_Manager_Valid (Words)
                and then (if Valid then
                            Value.Device_Roots = Words (Device_Roots_Word)
                            and then Value.Firmware_Roots = Words (Firmware_Roots_Word)
                            and then Value.Boot_Image = Words (Boot_Image_Word)
                            and then Value.Framebuffer.First = Words (Framebuffer_First_Word)
                            and then Value.Framebuffer.Bytes = Words (Framebuffer_Bytes_Word)
                            and then (for all I in 1 .. Device_Manager_Handle_Fields =>
                                        Handle_Words (Value.Handles) (I) =
                                          Words (First_Handle_Word - 1 + I))
                            and then Handles_Well_Formed (Value.Handles));
   procedure Decode
     (Words : Word_Array; Value : out Process_Manager_Configuration; Valid : out Boolean)
   with Post => Valid = (Header_Valid (Words, Process_Manager)
                         and then Words (4) in Physical_Address
                         and then Words (5) in Byte_Count
                         and then Process_Handles_Well_Formed
                           ((Self => Words (6), Self_Process => Words (7),
                             Self_Grantable => Words (8), Process_Root => Words (9),
                             Registration => Words (10), Notifications => Words (11),
                             Filesystem => Words (12), Config => Words (13),
                             Netstack => Words (14), Network_Policy => Words (15),
                             Mixer => Words (16), Device_Manager => Words (17),
                             Ready => Words (18), Framebuffer => Words (19),
                             Device_Inspection => Words (20),
                             Firmware_Snapshot => Words (21))))
                and then (if Valid then
                            Value.Framebuffer.First = Words (4)
                            and then Value.Framebuffer.Bytes = Words (5)
                            and then (for all I in 1 .. Process_Manager_Handle_Fields =>
                                        Process_Handle_Words (Value.Handles) (I) = Words (5 + I)));
   procedure Decode
     (Words : Word_Array; Value : out Boot_Image_Configuration; Valid : out Boolean)
   with Post => Valid = (Header_Valid (Words, Boot_Image_Reader)
                         and then Words (4) in Byte_Count)
                and then (if Valid then Value.Boot_Image = Words (4));
   procedure Decode
     (Words : Word_Array; Kind : Configuration_Kind;
      Value : out Controller_Configuration; Valid : out Boolean)
   with Pre  => Is_Controller (Kind),
        Post => Valid = (Header_Valid (Words, Kind)
                         and then Words (4) in Physical_Address
                         and then Words (5) in Physical_Address
                         and then Words (6) in Routing_ID)
                and then (if Valid then
                            Value.Registers = Words (4) and then Value.DMA = Words (5)
                            and then Value.Location = Words (6));
   procedure Decode
     (Words : Word_Array; Kind : Configuration_Kind;
      Value : out Queue_Configuration; Valid : out Boolean)
   with Pre  => Kind in Virtio_Input,
        Post => Valid = (Header_Valid (Words, Kind)
                         and then Words (4) in Physical_Address)
                and then (if Valid then Value.DMA = Words (4));
   procedure Decode
     (Words : Word_Array; Value : out Network_Device_Configuration; Valid : out Boolean)
   with Post => Valid = (Header_Valid (Words, Network_Device)
                         and then Words (4) in Physical_Address
                         and then Words (5) in Routing_ID)
                and then (if Valid then
                            Value.DMA = Words (4) and then Value.Location = Words (5));
   procedure Decode
     (Words : Word_Array; Value : out Virtio_GPU_Configuration; Valid : out Boolean)
   with Post => Valid = (Header_Valid (Words, Virtio_GPU)
                         and then Words (4) in Physical_Address
                         and then Words (5) in Physical_Address
                         and then Words (6) in Physical_Address
                         and then Words (7) in Byte_Count
                         and then (for all I in 8 .. 12 => Words (I) in Register_Offset)
                         and then Words (13) <= 1)
                and then (if Valid then
                            Value.Registers = Words (4)
                            and then Value.DMA = Words (5)
                            and then Value.Second_DMA = Words (6)
                            and then Value.DMA_Bytes = Words (7)
                            and then Value.Common_Offset = Words (8)
                            and then Value.Notify_Offset = Words (9)
                            and then Value.ISR_Offset = Words (10)
                            and then Value.Device_Offset = Words (11)
                            and then Value.Notify_Multiplier = Words (12)
                            and then Value.Boot_Display = (Words (13) = 1));
   procedure Decode
     (Words : Word_Array; Value : out Generic_PCI_Configuration; Valid : out Boolean)
   with Post => (if Valid then Header_Valid (Words, Generic_PCI)
                               and then Valid_Generic (Value));
end Launch_Configuration;
