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
   Maximum_Fields : constant := 24;
   Maximum_Words  : constant := Header_Words + Maximum_Fields;

   subtype Word_Count is Natural range 0 .. Maximum_Words;
   subtype Word_Index is Positive range 1 .. Maximum_Words;
   type Word_Array is array (Word_Index range <>) of Unsigned_64;

   --  An encoded configuration: Words (1 .. Length).
   type Encoded is record
      Words  : Word_Array (Word_Index) := [others => 0];
      Length : Word_Count := 0;
   end record;

   --  x86-64 physical addresses have at most 52 bits.
   Physical_Bits : constant := 52;
   subtype Physical_Address is Unsigned_64 range 0 .. 2 ** Physical_Bits - 1;
   subtype Byte_Count is Unsigned_64 range 0 .. 2 ** Physical_Bits;
   --  A register offset or multiplier inside one BAR (virtio's are u32).
   subtype Register_Offset is Unsigned_64 range 0 .. 2 ** 32 - 1;

   --  Physical pages: First and Bytes, or Bytes = 0 for none.
   type Physical_Span is record
      First : Physical_Address := 0;
      Bytes : Byte_Count := 0;
   end record;

   type Configuration_Kind is
     (Device_Manager, Process_Manager, Boot_Image_Reader, NVMe_Controller,
      HDA_Controller, Virtio_Net, Virtio_Input, Virtio_GPU, Generic_PCI);
   for Configuration_Kind use
     (Device_Manager    => 1,
      Process_Manager   => 2,
      Boot_Image_Reader => 3,
      NVMe_Controller   => 4,
      HDA_Controller    => 5,
      Virtio_Net        => 6,
      Virtio_Input      => 7,
      Virtio_GPU        => 8,
      Generic_PCI       => 9);

   --  devmgr's capability handles, placed by the kernel before it runs
   --  (CAP-002): handle words in its own table, never names or slot
   --  numbers. Self/Self_Process: its own endpoint and process.
   --  Self_Grantable: an untagged endpoint to itself it may badge and pass
   --  on (children's "ready" endpoints). The rest are the firmware/bus/
   --  object roots everything devmgr hands out is narrowed from: spawn and
   --  process control (CAP_PROCESS, all processes), PCI configuration
   --  ports, its own registration, the device-vector authority, the I/O
   --  port space, the legacy interrupt lines, every service role's
   --  notification, real-time scheduling, and one device-memory root per
   --  maximal run of device-mappable pages (largest first).
   Maximum_Device_Roots : constant := 8;
   subtype Device_Root_Count is Natural range 0 .. Maximum_Device_Roots;
   subtype Device_Root_Index is Positive range 1 .. Maximum_Device_Roots;
   type Root_Handles is array (Device_Root_Index) of Unsigned_64;

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
      Root_Count        : Device_Root_Count := 0;
      Device_Roots      : Root_Handles := [others => 0];
   end record;
   Device_Manager_Handle_Fields : constant := 11;

   --  A handle word: its generation (high half) is never zero.
   function Is_Handle (Word : Unsigned_64) return Boolean is
     (Shift_Right (Word, 32) /= 0);

   --  Every named handle is one, and exactly the roots in use are.
   function Handles_Well_Formed (H : Device_Manager_Handles) return Boolean is
     (Is_Handle (H.Self) and then Is_Handle (H.Self_Process)
      and then Is_Handle (H.Self_Grantable) and then Is_Handle (H.Process_Root)
      and then Is_Handle (H.PCI_Ports) and then Is_Handle (H.Registration)
      and then Is_Handle (H.Vector_Authority) and then Is_Handle (H.IO_Ports)
      and then Is_Handle (H.Legacy_Interrupts) and then Is_Handle (H.Notifications)
      and then Is_Handle (H.Scheduling)
      and then (for all R in Device_Root_Index =>
                  (if R <= H.Root_Count then Is_Handle (H.Device_Roots (R))
                   else H.Device_Roots (R) = 0)));

   --  Kernel to devmgr: the boot image's size, the boot framebuffer's
   --  physical pages, and its handles.
   type Device_Manager_Configuration is record
      Boot_Image  : Byte_Count := 0;
      Framebuffer : Physical_Span;
      Handles     : Device_Manager_Handles;
   end record;

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
   end record;
   Process_Manager_Handle_Fields : constant := 15;

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
      and then Is_Handle (H.Device_Inspection));

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

   --  devmgr to NVMe and HDA: the register BAR and the DMA region devmgr
   --  allocated for the driver.
   type Controller_Configuration is record
      Registers : Physical_Address := 0;
      DMA       : Physical_Address := 0;
   end record;

   --  devmgr to virtio-net and virtio-input: their DMA region (queues and
   --  buffers), which devmgr allocated at the driver's fixed address.
   type Queue_Configuration is record
      DMA : Physical_Address := 0;
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
   --  bus << 8 | device << 3 | function (the PCI routing ID).
   subtype Routing_ID is Unsigned_64 range 0 .. 2 ** 16 - 1;
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

   function Field_Count (Kind : Configuration_Kind) return Natural is
     (case Kind is
         when Device_Manager    => 3 + Device_Manager_Handle_Fields + 1 + Maximum_Device_Roots,
         when Process_Manager   => 2 + Process_Manager_Handle_Fields,
         when Boot_Image_Reader => 1,
         when NVMe_Controller | HDA_Controller => 2,
         when Virtio_Net | Virtio_Input => 1,
         when Virtio_GPU        => 10,
         when Generic_PCI       => 5 + 1 + 2 * Maximum_Windows + 3 + 2)
   with Post => Field_Count'Result <= Maximum_Fields;

   --  Words (1 .. Length) is a header for Kind with the right length.
   function Header_Valid
     (Words : Word_Array; Kind : Configuration_Kind) return Boolean
   is (Words'First = 1
       and then Words'Length = Header_Words + Field_Count (Kind)
       and then Words (1) = Magic
       and then Words (2) = Configuration_Kind'Enum_Rep (Kind)
       and then Words (3) = Unsigned_64 (Field_Count (Kind)));

   --  Encoders: the header for the kind, then the fields in order.
   --  The handles' words, in field order (Words (7 .. 26)).
   function Handle_Words (H : Device_Manager_Handles) return Word_Array is
     [H.Self, H.Self_Process, H.Self_Grantable, H.Process_Root, H.PCI_Ports,
      H.Registration, H.Vector_Authority, H.IO_Ports, H.Legacy_Interrupts,
      H.Notifications, H.Scheduling, Unsigned_64 (H.Root_Count),
      H.Device_Roots (1), H.Device_Roots (2), H.Device_Roots (3),
      H.Device_Roots (4), H.Device_Roots (5), H.Device_Roots (6),
      H.Device_Roots (7), H.Device_Roots (8)]
   with Post => Handle_Words'Result'First = 1
                and then Handle_Words'Result'Length =
                         Device_Manager_Handle_Fields + 1 + Maximum_Device_Roots;

   --  Every named handle is one, the root count is in range, the roots in
   --  use are handles and the rest are zero.
   function Handles_Valid (W : Word_Array) return Boolean is
     ((for all I in W'First .. W'First + (Device_Manager_Handle_Fields - 1) =>
         Is_Handle (W (I)))
      and then W (W'First + Device_Manager_Handle_Fields) <= Maximum_Device_Roots
      and then (for all R in Device_Root_Index =>
                  (if Unsigned_64 (R) <= W (W'First + Device_Manager_Handle_Fields)
                   then Is_Handle (W (W'First + Device_Manager_Handle_Fields + R))
                   else W (W'First + Device_Manager_Handle_Fields + R) = 0)))
   with Pre => W'Length = Device_Manager_Handle_Fields + 1 + Maximum_Device_Roots
               and then W'First = 7;

   function Encode (Value : Device_Manager_Configuration) return Encoded
   with Post => Encode'Result.Length = Header_Words + Field_Count (Device_Manager)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length),
                                       Device_Manager)
                and then Encode'Result.Words (4) = Value.Boot_Image
                and then Encode'Result.Words (5) = Value.Framebuffer.First
                and then Encode'Result.Words (6) = Value.Framebuffer.Bytes
                and then (for all I in 1 .. Device_Manager_Handle_Fields + 1 + Maximum_Device_Roots =>
                            Encode'Result.Words (6 + I) = Handle_Words (Value.Handles) (I));
   --  The handles' words, in field order (Words (6 .. 20)).
   function Process_Handle_Words (H : Process_Manager_Handles) return Word_Array is
     [H.Self, H.Self_Process, H.Self_Grantable, H.Process_Root, H.Registration,
      H.Notifications, H.Filesystem, H.Config, H.Netstack, H.Network_Policy,
      H.Mixer, H.Device_Manager, H.Ready, H.Framebuffer, H.Device_Inspection]
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
   with Pre  => Kind in NVMe_Controller | HDA_Controller,
        Post => Encode'Result.Length = Header_Words + Field_Count (Kind)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length), Kind)
                and then Encode'Result.Words (4) = Value.Registers
                and then Encode'Result.Words (5) = Value.DMA;
   function Encode
     (Value : Queue_Configuration; Kind : Configuration_Kind) return Encoded
   with Pre  => Kind in Virtio_Net | Virtio_Input,
        Post => Encode'Result.Length = Header_Words + Field_Count (Kind)
                and then Header_Valid (Encode'Result.Words (1 .. Encode'Result.Length), Kind)
                and then Encode'Result.Words (4) = Value.DMA;
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
   with Post => Valid = (Header_Valid (Words, Device_Manager)
                         and then Words (4) in Byte_Count
                         and then Words (5) in Physical_Address
                         and then Words (6) in Byte_Count
                         and then Handles_Valid (Words (7 .. 26)))
                and then (if Valid then
                            Value.Boot_Image = Words (4)
                            and then Value.Framebuffer.First = Words (5)
                            and then Value.Framebuffer.Bytes = Words (6)
                            and then (for all I in 1 .. 20 =>
                                        Handle_Words (Value.Handles) (I) = Words (6 + I)));
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
                             Device_Inspection => Words (20))))
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
   with Pre  => Kind in NVMe_Controller | HDA_Controller,
        Post => Valid = (Header_Valid (Words, Kind)
                         and then Words (4) in Physical_Address
                         and then Words (5) in Physical_Address)
                and then (if Valid then
                            Value.Registers = Words (4) and then Value.DMA = Words (5));
   procedure Decode
     (Words : Word_Array; Kind : Configuration_Kind;
      Value : out Queue_Configuration; Valid : out Boolean)
   with Pre  => Kind in Virtio_Net | Virtio_Input,
        Post => Valid = (Header_Valid (Words, Kind)
                         and then Words (4) in Physical_Address)
                and then (if Valid then Value.DMA = Words (4));
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
