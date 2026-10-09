------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process_ID: the one name for a process (KERN-003,
--  docs/process-objects.md).
--
--  @description
--  An identity names one life of one process and is never handed out
--  twice. It is opaque: equality is all a program may do with it. It is
--  not a number to count, order or compute with, so it is a private type;
--  only the ABI boundary turns it into a word (To_Word, From_Word), where a
--  message word or a system-call register carries it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Interfaces.C;

package CuBit.Process_IDs with Pure, SPARK_Mode is

   type Process_ID is private;
   No_Process : constant Process_ID;

   --  The word the kernel uses for it: for message words and system-call
   --  registers only.
   function To_Word (Process : Process_ID) return Interfaces.Unsigned_64;
   function From_Word (Word : Interfaces.Unsigned_64) return Process_ID;

   --  Whether it can name a process at all (not No_Process, and well
   --  formed). Whether that process still runs is the kernel's to say.
   function Is_Process (Process : Process_ID) return Boolean;

   --  For tables keyed by process (CuBit.Identity_Tables): spreads
   --  identities whose low bits are sequential.
   function Hash (Process : Process_ID) return Interfaces.Unsigned_64;

   --  Decimal, for diagnostics; the same number a person may type back:
   --  Text (1 .. Last). CuBit.Process_IDs.Text.Image returns it as a String
   --  for programs that may use the secondary stack.
   Image_Width : constant := 20;
   subtype Image_Text is String (1 .. Image_Width);
   procedure Image (Process : Process_ID; Text : out Image_Text; Last : out Positive);

   --  Image's inverse: Valid when Text is a decimal that fits 64 bits.
   procedure Parse (Text : String; Process : out Process_ID; Valid : out Boolean);

   --  pid_t for POSIX ports: the identity folded to 31 bits (its slot and
   --  the low 7 bits of its generation). The same in parent and child;
   --  unique among live processes, and a reused slot repeats it only 128
   --  lives later. The libc maps it back through the processes it knows.
   subtype POSIX_PID is Interfaces.C.int range 1 .. Interfaces.C.int'Last;
   function POSIX_Of (Process : Process_ID) return POSIX_PID
   with Pre => Is_Process (Process);

private
   use type Interfaces.Unsigned_64;

   type Process_ID is new Interfaces.Unsigned_64 with Default_Value => 0;
   No_Process : constant Process_ID := 0;

   --  The kernel's layout (kernel/src/process_identities.ads): slot in
   --  the low 24 bits, generation above. Only this package depends on it.
   Slot_Bits : constant := 24;
   POSIX_Generation_Bits : constant := 7;
   Slot_Mask : constant Process_ID := 2 ** Slot_Bits - 1;

   function To_Word (Process : Process_ID) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (Process));
   function From_Word (Word : Interfaces.Unsigned_64) return Process_ID is
     (Process_ID (Word));
   function Is_Process (Process : Process_ID) return Boolean is
     ((Process and Slot_Mask) /= 0);
   function Hash (Process : Process_ID) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (Process) * 16#9E37_79B9_7F4A_7C15#);
   function POSIX_Of (Process : Process_ID) return POSIX_PID is
     (POSIX_PID ((Process and Slot_Mask) or
                 Shift_Left (Shift_Right (Process, Slot_Bits) and
                               (2 ** POSIX_Generation_Bits - 1), Slot_Bits)));

end CuBit.Process_IDs;
