--  Hosted tests for Launch_Configuration and the System_Publications
--  layouts. Run by run.sh; exits nonzero on the first failure.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Launch_Configuration; use Launch_Configuration;
with System_Publications;
with Process_Layout;
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
   --  Round trips for every kind.
   declare
      --  Handle words: generation in the high half, slot in the low half.
      function H (Slot : Unsigned_64) return Unsigned_64 is (2 ** 32 + Slot);
      Handles : constant Device_Manager_Handles :=
        (Self => H (0), Self_Process => H (1), Self_Grantable => H (2),
         Process_Root => H (3), PCI_Ports => H (4), Registration => H (5),
         Vector_Authority => H (6), IO_Ports => H (7), Legacy_Interrupts => H (8),
         Notifications => H (9), Scheduling => H (10), Root_Count => 3,
         Device_Roots => [H (11), H (12), H (13), others => 0]);
      V : Device_Manager_Configuration :=
        (Boot_Image => 40 * 1024 * 1024, Framebuffer => (16#FD00_0000#, 16#30_0000#),
         Handles => Handles);
      D : Device_Manager_Configuration; OK : Boolean;
      E : Encoded;
   begin
      Round_Trips.Device_Manager (V, D, OK);
      Check (OK and then D = V, "device manager round trip");
      V.Framebuffer := (0, 0);
      Round_Trips.Device_Manager (V, D, OK);
      Check (OK and then D = V, "device manager without framebuffer");
      V.Handles.Root_Count := Maximum_Device_Roots;
      V.Handles.Device_Roots := [for R in Device_Root_Index => H (20 + Unsigned_64 (R))];
      Round_Trips.Device_Manager (V, D, OK);
      Check (OK and then D = V, "device manager with every root");
      --  Refusals: a named handle that is not one, a root count past the
      --  array, a root in use that is zero, an unused root that is not.
      V := (Boot_Image => 1, Framebuffer => (0, 0), Handles => Handles);
      E := Encode (V);
      for Bad in 7 .. 17 loop
         declare
            W : Word_Array := E.Words (1 .. E.Length);
         begin
            W (Bad) := 5;   --  a refusal-sized word, generation zero
            Decode (W, D, OK);
            Check (not OK, "named handle without a generation refused");
         end;
      end loop;
      declare
         W : Word_Array := E.Words (1 .. E.Length);
      begin
         W (18) := Maximum_Device_Roots + 1;
         Decode (W, D, OK);
         Check (not OK, "root count past the array refused");
         W (18) := 4;
         Decode (W, D, OK);
         Check (not OK, "a zero root in use refused");
         W (18) := 2;
         Decode (W, D, OK);
         Check (not OK, "a handle in an unused root refused");
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
                     Device_Inspection => H (13)));
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
      end;
   end;
   declare
      D : Virtio_GPU_Configuration; OK : Boolean;
   begin
      Round_Trips.GPU (GPU, D, OK);
      Check (OK and then D = GPU, "virtio-gpu round trip");
   end;
   declare
      D : Controller_Configuration; OK : Boolean;
   begin
      Round_Trips.Controller ((16#FEB0_0000#, 16#4000_0000#), NVMe_Controller, D, OK);
      Check (OK and then D = (16#FEB0_0000#, 16#4000_0000#), "nvme round trip");
      Round_Trips.Kinds_Distinct ((1, 2), OK);
      Check (not OK, "an NVMe block is not an HDA block");
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

   --  Random words never decode, and never raise (-gnata -gnato).
   declare
      W : Word_Array (1 .. 26);
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
            W (3) := Unsigned_64 (Field_Count (Device_Manager));
         end if;
         Decode (W, D, OK);
         if OK then
            Accepted := Accepted + 1;
            Check (D.Boot_Image = W (4) and then D.Framebuffer.First = W (5),
                   "accepted random block decodes its fields");
         end if;
      end loop;
      Check (Accepted < 200_000, "random blocks are mostly refused");
   end;

   --  The magic is never a launch-argument format version (u16 at 0).
   Check ((Magic and 16#FFFF#) > 16, "magic is not an argument block version");

   --  Layout facts the kernel and runtimes rely on.
   Check (System_Publications.Facts_Page'Size = 112 * 8, "facts page size");
   Check (System_Publications.Registry_Page'Size = (1 + 128) * 64, "registry size");
   Check (System_Publications.Statistics'Size = 24 * 8, "statistics size");
   Check (System_Publications.Wall_Clock_Address =
            16#0000_5B00_0000_3000#, "wall clock address");
   Check (Process_Layout.Secondary_Stack_Address = 16#0000_7FFF_FF00_0000#,
          "secondary stack address");

   Put_Line ("launch-configuration:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
