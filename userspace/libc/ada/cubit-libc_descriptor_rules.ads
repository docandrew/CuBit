------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The arithmetic and flag rules of the libc's descriptors
--  (docs/c-removal.md): open(2) flags to the filesystem service's open
--  options, lseek, fcntl's status flags, and stat's block count.
--
--  @description
--  Proved (tests/libc-ada): no overflow (the C this replaces added lseek
--  offsets unchecked, signed overflow included), and a seek never yields a
--  negative or unrepresentable offset.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;

package CuBit.Libc_Descriptor_Rules with Pure, SPARK_Mode is

   use type Interfaces.C.long;
   use type Interfaces.C.int;

   --  The filesystem service's open options (CuBit.Filesystems; checked
   --  by tests/libc-ada).
   OPEN_READ_ONLY  : constant Unsigned_64 := 0;
   OPEN_WRITE_ONLY : constant Unsigned_64 := 1;
   OPEN_READ_WRITE : constant Unsigned_64 := 2;
   OPEN_CREATE     : constant Unsigned_64 := 64;
   OPEN_TRUNCATE   : constant Unsigned_64 := 512;
   OPEN_EXCLUSIVE  : constant Unsigned_64 := 1024;

   function Bits (Value : long) return Unsigned_64 is (Unsigned_64'Mod (Value));
   function Has (Value, Flag : long) return Boolean is
     ((Bits (Value) and Bits (Flag)) /= 0);

   function Access_Mode (Flags : long) return Unsigned_64 is
     (Bits (Flags) and Bits (O_ACCMODE));

   function Writable (Flags : long) return Boolean is
     (Access_Mode (Flags) /= Bits (O_RDONLY));

   --  The service's options for open(2) Flags; not Valid for an access
   --  mode it lacks (O_PATH and the like).
   procedure Open_Options (Flags : long; Options : out Unsigned_64; Valid : out Boolean)
   with Post => (if Valid then (Options and 3) <= OPEN_READ_WRITE);

   --  lseek: Offset from the start, the current position or the end.
   --  Valid only when the result is a representable, non-negative offset.
   procedure Seek
     (Whence : int; Offset : Integer_64; Current, Size : Unsigned_64;
      Result : out Unsigned_64; Valid : out Boolean)
   with Post => (if Valid then Result <= Unsigned_64 (Integer_64'Last));

   --  F_SETFL: only O_NONBLOCK changes (only pipes and sockets block).
   --  Open flags are never negative.
   function Set_Status_Flags (Old, Argument : long) return long is
     (if Has (Argument, O_NONBLOCK) = Has (Old, O_NONBLOCK) then Old
      elsif Has (Argument, O_NONBLOCK) then Old + O_NONBLOCK
      else Old - O_NONBLOCK)
   with Pre => Old in 0 .. long'Last - O_NONBLOCK;

   --  stat's st_blocks: 512-byte units, rounded up.
   Block_Unit : constant := 512;
   function Blocks (Size : Unsigned_64) return Unsigned_64 is
     (Size / Block_Unit + (if Size mod Block_Unit = 0 then 0 else 1));

end CuBit.Libc_Descriptor_Rules;
