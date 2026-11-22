------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  select over poll (docs/c-removal.md): the descriptors named in select's
--  three sets become pollfds, and the poll results become the sets select
--  returns, with the count of set bits.
--
--  @description
--  Proved (tests/libc-ada): every index stays in range, only descriptors
--  below the caller's limit are polled or reported, a descriptor is
--  reported ready only in a set it was asked about, and the count is the
--  number of bits set. Hang-up and error count as readable, and error as
--  writable, as on Linux.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;

package CuBit.Libc_Select with Pure, SPARK_Mode is

   use type Interfaces.C.int;

   subtype Descriptor is Natural range 0 .. FD_SETSIZE - 1;
   subtype Limit is Natural range 0 .. FD_SETSIZE;

   --  fd_set: bit fd % 64 of word fd / 64, which is bit fd of the whole set
   --  on a little-endian machine.
   type Descriptor_Set is array (Descriptor) of Boolean
   with Pack, Size => FD_SETSIZE;
   function Empty (Set : Descriptor_Set) return Boolean is
     (for all D in Descriptor => not Set (D));

   type Poll_Array is array (Descriptor) of Poll_Descriptor;

   --  Which of select's sets were given (a null pointer is none).
   type Given is record
      Read, Write, Error : Boolean := False;
   end record;

   function Event_Bits (Read, Write, Error : Boolean) return Integer_16 is
     ((if Read then POLLIN else 0) + (if Write then POLLOUT else 0)
      + (if Error then POLLPRI else 0));

   --  The descriptors below Count named in a given set, in order.
   procedure Gather
     (Count : Limit; Sets : Given; Read, Write, Error : Descriptor_Set;
      Polls : out Poll_Array; Used : out Limit)
   with Post => Used <= Count
                and then (for all K in 0 .. Used - 1 =>
                            Polls (K).Descriptor in 0 .. Interfaces.C.int (Count) - 1
                            and then Polls (K).Events /= 0
                            and then Polls (K).Returned = 0);

   --  The sets select returns from the first Used poll results: Invalid if
   --  any descriptor was not open (POLLNVAL, select's EBADF).
   procedure Scatter
     (Polls : Poll_Array; Used : Limit; Sets : Given;
      Read, Write, Error : out Descriptor_Set; Ready : out Natural;
      Invalid : out Boolean)
   with Pre => (for all K in 0 .. Used - 1 =>
                  Polls (K).Descriptor in 0 .. FD_SETSIZE - 1),
        Post => Ready <= 3 * Used
                and then (if not Sets.Read then Empty (Read))
                and then (if not Sets.Write then Empty (Write))
                and then (if not Sets.Error then Empty (Error));

end CuBit.Libc_Select;
