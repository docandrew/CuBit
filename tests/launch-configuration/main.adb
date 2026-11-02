--  Hosted tests for Launch_Configuration and the System_Publications
--  layouts. Run by run.sh; exits nonzero on the first failure.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Launch_Configuration; use Launch_Configuration;
with System_Publications;
with Round_Trips;

procedure Main is
   pragma Warnings (Off, "condition is always True");
   Checks : Natural := 0;
   Failed : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   GPU : constant Virtio_GPU_Configuration :=
     (Registers => 16#FE00_0000#, DMA => 16#1234_5000#, Second_DMA => 16#2000_0000#,
      DMA_Bytes => 16#200_0000#, Common_Offset => 0, Notify_Offset => 16#3000#,
      ISR_Offset => 16#1000#, Device_Offset => 16#2000#, Notify_Multiplier => 4,
      Boot_Display => True);
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
begin
   --  Round trips for every kind. devmgr's block is variable-length: as
   --  many roots as the kernel found (LIM-001b), well past the old fixed 8
   --  device and 8 firmware roots.
   declare
      --  Handle words: generation in the high half, slot in the low half.
      function H (Slot : Unsigned_64) return Unsigned_64 is (2 ** 32 + Slot);
      Handles : constant Device_Manager_Handles :=
        (Self => H (0), Self_Process => H (1), Self_Grantable => H (2),
         Process_Root => H (3), PCI_Ports => H (4), Registration => H (5),
         Vector_Authority => H (6), IO_Ports => H (7), Legacy_Interrupts => H (8),
         Notifications => H (9), Scheduling => H (10), Firmware_Snapshot => H (14));
      Old_Fixed_Roots : constant := 8;
      Many_Device : constant := 5 * Old_Fixed_Roots;
      Many_Firmware : constant := 7 * Old_Fixed_Roots;
      All_Roots : constant Word_Array (1 .. Many_Device + Many_Firmware) :=
        [for I in 1 .. Many_Device + Many_Firmware => H (100 + Unsigned_64 (I))];
      V : Device_Manager_Configuration :=
        (Device_Roots => 3, Firmware_Roots => 2,
         Boot_Image => 40 * 1024 * 1024, Framebuffer => (16#FD00_0000#, 16#30_0000#),
         Handles => Handles);
      D : Device_Manager_Configuration; OK : Boolean;

      --  The whole block for V with All_Roots' first roots.
      function Block_Of (Value : Device_Manager_Configuration) return Word_Array is
         E : constant Encoded := Encode_Prefix (Value);
         R : constant Natural := Natural (Value.Device_Roots + Value.Firmware_Roots);
      begin
         return E.Words (1 .. E.Length) & All_Roots (1 .. R);
      end Block_Of;
   begin
      Round_Trips.Device_Manager (V, All_Roots (1 .. 5), D, OK);
      Check (OK and then D = V, "device manager round trip");
      V.Framebuffer := (0, 0);
      Round_Trips.Device_Manager (V, All_Roots (1 .. 5), D, OK);
      Check (OK and then D = V, "device manager without framebuffer");
      V.Device_Roots := 0;
      V.Firmware_Roots := 0;
      Round_Trips.Device_Manager (V, All_Roots (1 .. 0), D, OK);
      Check (OK and then D = V, "device manager without roots");
      V.Device_Roots := Many_Device;
      V.Firmware_Roots := Many_Firmware;
      Round_Trips.Device_Manager (V, All_Roots, D, OK);
      Check (OK and then D = V, "device manager with more roots than the old fixed block");
      declare
         W : constant Word_Array := Block_Of (V);
      begin
         Check (W'Last = Device_Manager_Length (V)
                and then W'Length > Maximum_Words,
                "devmgr's block grows with its roots");
         Check (W (Device_Root_Word (1)) = All_Roots (1)
                and then W (Device_Root_Word (Many_Device)) = All_Roots (Many_Device)
                and then W (Firmware_Root_Word (Many_Device, 1)) = All_Roots (Many_Device + 1)
                and then W (Firmware_Root_Word (Many_Device, Many_Firmware)) =
                           All_Roots (Many_Device + Many_Firmware),
                "root words where Device_Root_Word and Firmware_Root_Word say");
      end;
      Check (Roots_Fit (Maximum_Roots, 0) and then not Roots_Fit (Maximum_Roots, 1)
             and then Device_Manager_Last (Maximum_Roots, 0) = Maximum_Block_Words,
             "the most roots fill the block ceiling exactly");
      --  Refusals: a named handle or root that is not one, counts that do
      --  not match the length, counts past the ceiling, a kind mismatch.
      V := (Device_Roots => 3, Firmware_Roots => 4, Boot_Image => 1,
            Framebuffer => (0, 0), Handles => Handles);
      for Bad in First_Handle_Word .. Device_Manager_Length (V) loop
         declare
            W : Word_Array := Block_Of (V);
         begin
            W (Bad) := 5;   --  a refusal-sized word, generation zero
            Decode (W, D, OK);
            Check (not OK, "a handle or root word without a generation refused");
         end;
      end loop;
      declare
         W : Word_Array := Block_Of (V);
      begin
         Decode (W, D, OK);
         Check (OK and then D = V, "the refusal fixture itself decodes");
         W (Device_Roots_Word) := 4;
         Decode (W, D, OK);
         Check (not OK, "a device root count past the block refused");
         W (Device_Roots_Word) := 2;
         Decode (W, D, OK);
         Check (not OK, "a device root count short of the block refused");
         W (Device_Roots_Word) := 3;
         W (3) := W (3) + 1;
         Decode (W, D, OK);
         Check (not OK, "a field count that disagrees with the counts refused");
         W := Block_Of (V);
         W (Firmware_Roots_Word) := Unsigned_64 (Maximum_Roots) + 1;
         Decode (W, D, OK);
         Check (not OK, "a firmware root count past the ceiling refused");
         W := Block_Of (V);
         W (Device_Roots_Word) := Unsigned_64'Last;
         Decode (W, D, OK);
         Check (not OK, "a huge device root count refused");
         W := Block_Of (V);
         W (2) := Configuration_Kind'Enum_Rep (Process_Manager);
         Decode (W, D, OK);
         Check (not OK, "a process manager kind refused");
         W := Block_Of (V);
         Decode (W (1 .. W'Last - 1), D, OK);
         Check (not OK, "a short devmgr block refused");
         Decode (W & [H (999)], D, OK);
         Check (not OK, "a long devmgr block refused");
         Decode (W (1 .. Device_Manager_Prefix_Words - 1), D, OK);
         Check (not OK, "a block shorter than the prefix refused");
      end;
   end;
   declare
      function H (Slot : Unsigned_64) return Unsigned_64 is (2 ** 32 + Slot);
      V : Process_Manager_Configuration :=
        (Framebuffer => (16#FD00_0000#, 16#30_0000#),
         Handles => (Self => H (0), Self_Process => H (1), Self_Grantable => H (2),
                     Process_Root => H (3), Registration => H (4),
                     Notifications => H (5), Filesystem => H (6), Config => H (7),
                     Netstack => H (8), Network_Policy => H (9), Mixer => 0,
                     Device_Manager => H (10), Ready => H (11), Framebuffer => H (12),
                     Device_Inspection => H (13), Firmware_Snapshot => H (14)));
      D : Process_Manager_Configuration; OK : Boolean;
      E : Encoded;
   begin
      Round_Trips.Process_Manager (V, D, OK);
      Check (OK and then D = V, "process manager round trip");
      V.Handles.Framebuffer := 0;
      Round_Trips.Process_Manager (V, D, OK);
      Check (OK and then D = V, "process manager without framebuffer");
      E := Encode (V);
      declare
         W : Word_Array := E.Words (1 .. E.Length);
      begin
         W (9) := 7;   --  process root without a generation
         Decode (W, D, OK);
         Check (not OK, "process manager refuses a non-handle root");
         W := E.Words (1 .. E.Length);
         W (21) := 0;  --  no firmware snapshot reader
         Decode (W, D, OK);
         Check (not OK, "process manager refuses a missing firmware snapshot reader");
      end;
   end;
   declare
      D : Virtio_GPU_Configuration; OK : Boolean;
   begin
      Round_Trips.GPU (GPU, D, OK);
      Check (OK and then D = GPU, "virtio-gpu round trip");
   end;
   declare
      V : constant Network_Device_Configuration := (DMA => 16#7F00_0000#, Location => 16#0018#);
      D : Network_Device_Configuration; OK : Boolean;
      E : constant Encoded := Encode (V);
      W : Word_Array (1 .. E.Length) := E.Words (1 .. E.Length);
   begin
      Round_Trips.Network_Device (V, D, OK);
      Check (OK and then D = V, "network device round trip");
      W (5) := 16#1_0000#;
      Decode (W, D, OK);
      Check (not OK, "network device location past 16 bits refused");
   end;
   declare
      D : Controller_Configuration; OK : Boolean;
   begin
      Round_Trips.Controller ((16#FEB0_0000#, 16#4000_0000#, 16#0400#), NVMe_Controller, D, OK);
      Check (OK and then D = (16#FEB0_0000#, 16#4000_0000#, 16#0400#), "nvme round trip");
      --  xHCI: the IdeaPad's TCSS controller, 00:0d.0.
      Round_Trips.Controller ((16#6000_0000#, 16#1000_0000#, 16#0068#), XHCI_Controller, D, OK);
      Check (OK and then D.Location = 16#0068#, "xhci round trip");
      Check (Location_Image (16#0068#) = "00:0D.0", "location image TCSS");
      Check (Location_Image (16#00A0#) = "00:14.0", "location image PCH");
      Check (Location_Image (16#FFFF#) = "FF:1F.7", "location image maximum");
      Round_Trips.Kinds_Distinct ((1, 2, 3), OK);
      Check (not OK, "an NVMe block is not an HDA block");
      Round_Trips.XHCI_Distinct ((1, 2, 3), OK);
      Check (not OK, "an xHCI block is not an NVMe block");
      --  A location wider than a routing ID is refused.
      declare
         E : Encoded := Encode (Controller_Configuration'(1, 2, 3), HDA_Controller);
      begin
         E.Words (6) := 2 ** 16;
         Decode (E.Words (1 .. E.Length), HDA_Controller, D, OK);
         Check (not OK, "controller refuses a location past 16 bits");
      end;
   end;

   --  Generic_PCI (HW-002): what a driver launched by its declared match
   --  receives; decoding the encoder's output returns it, and windows past
   --  the count, vectors that do not fit the delivery, or DMA at a virtual
   --  address without a region are refused.
   declare
      Value : constant Generic_PCI_Configuration :=
        (Location => 16#0012#, Vendor => 16#10EC#, Device => 16#B852#, Revision => 0,
         Class_Code => 16#02_8000#,
         Windows => [1 => (16#8000_0000#, 16#10_0000#), 2 => (16#8010_0000#, 16#4000#),
                     others => (0, 0)],
         Window_Total => 2, DMA => (16#1_0000_0000#, 16#10_0000#), DMA_Virtual => 16#7000_0000_0000#,
         Delivery => MSI_X, Vectors => 4);
      E : constant Encoded := Encode (Value);
      W : Word_Array (1 .. E.Length);
      D : Generic_PCI_Configuration; OK : Boolean;
   begin
      Decode (E.Words (1 .. E.Length), D, OK);
      Check (OK and then D = Value, "generic PCI round trip");
      Check (E.Length = Header_Words + Field_Count (Generic_PCI), "generic PCI length");
      --  Word 15: window 3's first address, beyond Window_Total.
      W := E.Words (1 .. E.Length);
      W (14) := 16#9000_0000#;
      Decode (W, D, OK);
      Check (not OK, "a window past the count is refused");
      --  The last word: the vector count; MSI-X with 0 vectors.
      W := E.Words (1 .. E.Length);
      W (E.Length) := 0;
      Decode (W, D, OK);
      Check (not OK, "MSI-X without vectors is refused");
      W := E.Words (1 .. E.Length);
      W (E.Length) := Maximum_Vectors + 1;
      Decode (W, D, OK);
      Check (not OK, "too many vectors refused");
      --  The delivery word: an unknown code.
      W := E.Words (1 .. E.Length);
      W (E.Length - 1) := 9;
      Decode (W, D, OK);
      Check (not OK, "an unknown delivery refused");
      Decode (E.Words (1 .. E.Length - 1), D, OK);
      Check (not OK, "a short generic block refused");
   end;

   --  Refusals: header and field corruption.
   declare
      E : constant Encoded := Encode (GPU);
      W : Word_Array (1 .. E.Length);
      D : Virtio_GPU_Configuration; OK : Boolean;
   begin
      for Index in 1 .. E.Length loop
         W := E.Words (1 .. E.Length);
         W (Index) := (if Index = 13 then 2 elsif Index >= 8 then 2 ** 32
                       elsif Index >= 4 then 2 ** 53 else W (Index) + 1);
         Decode (W, D, OK);
         Check (not OK, "corrupted word" & Index'Image & " refused");
      end loop;
      Decode (E.Words (1 .. E.Length - 1), D, OK);
      Check (not OK, "short block refused");
      Decode (E.Words (2 .. E.Length), D, OK);
      Check (not OK, "block not starting at 1 refused");
      declare
         Long : constant Word_Array := E.Words (1 .. E.Length) & [0];
      begin
         Decode (Long, D, OK);
         Check (not OK, "long block refused");
      end;
   end;

   --  Random words never decode, and never raise (-gnata -gnato). Half
   --  carry a devmgr header whose counts match the block (2 + 3 roots).
   declare
      Random_Device_Roots : constant := 2;
      Random_Firmware_Roots : constant := 3;
      W : Word_Array (1 .. Device_Manager_Prefix_Words + Random_Device_Roots
                             + Random_Firmware_Roots);
      D : Device_Manager_Configuration; OK : Boolean;
      Accepted : Natural := 0;
   begin
      for Round in 1 .. 200_000 loop
         for Index in W'Range loop
            W (Index) := Next;
         end loop;
         if Round mod 2 = 0 then
            W (1) := Magic;
            W (2) := Configuration_Kind'Enum_Rep (Device_Manager);
            W (3) := Device_Manager_Fields (Random_Device_Roots, Random_Firmware_Roots);
            W (Device_Roots_Word) := Random_Device_Roots;
            W (Firmware_Roots_Word) := Random_Firmware_Roots;
         end if;
         Decode (W, D, OK);
         if OK then
            Accepted := Accepted + 1;
            Check (D.Boot_Image = W (Boot_Image_Word)
                   and then D.Framebuffer.First = W (Framebuffer_First_Word)
                   and then D.Device_Roots = Random_Device_Roots,
                   "accepted random block decodes its fields");
         end if;
      end loop;
      Check (Accepted < 200_000, "random blocks are mostly refused");
   end;

   --  The magic is never a launch-argument format version (u16 at 0).
   Check ((Magic and 16#FFFF#) > 16, "magic is not an argument block version");

   --  Layout facts the kernel and runtimes rely on.
   Check (System_Publications.Facts_Page'Size = 136 * 8, "facts page size");
   Check (System_Publications.Registry_Page'Size = (1 + 128) * 64, "registry size");
   Check (System_Publications.Statistics'Size = 24 * 8, "statistics size");
   Check (System_Publications.Wall_Clock_Address =
            16#0000_5B00_0000_3000#, "wall clock address");

   Put_Line ("launch-configuration:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
