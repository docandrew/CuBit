------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Owned memory's answers (RESERVE_OWNED_REGION, CHANGE_OWNED_RANGE,
--  RELEASE_OWNED_REGION; kernel Process.Owned_Memory): success, or why the
--  kernel refused, as a typed reason (MEM-007).
--
--  No size limit is part of the ABI. A caller asks for what it needs; the
--  kernel refuses what it cannot give, saying why (Too_Large, No_Address_
--  Space, Out_Of_Memory, ...), so no constant of the kernel's is compiled
--  into a program, and changing one never breaks old binaries (BUG-009: the
--  256 KiB commit step a program had compiled in stopped matching the
--  kernel's).
--
--  RESERVE answers a region's base or a refusal word; CHANGE and RELEASE
--  answer Done or a refusal word. A base is never a refusal word: every
--  base lies in the owned aperture, far above the codes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Owned_Memory_ABI with Pure, SPARK_Mode is

   Done : constant Unsigned_64 := 0;

   --  Codes are fixed; a new reason takes a new code.
   type Refusal is
     (Invalid_Range,       --  empty, not page-aligned, or wrapping
      Too_Large,           --  larger than the kernel's largest region
      Invalid_Alignment,   --  not 0 or a power of two the kernel can place
      Invalid_Operation,   --  not a CHANGE_OWNED_RANGE operation
      Not_Admitted,        --  the process is not running (starting or ending)
      No_Address_Space,    --  no free range that size and alignment in the aperture
      No_Record,           --  the process's owned-memory records are used up
      No_Region,           --  the range is not inside one live region
      Already_Backed,      --  a commit over committed pages
      Not_Backed,          --  an access change over pages not committed
      Out_Of_Memory,       --  no free frame or page-table page
      Over_Quota,          --  past the process's memory quota
      Retirement_Failed);  --  a page table could not be changed; the backing is kept
   for Refusal use
     (Invalid_Range     => 1,
      Too_Large         => 2,
      Invalid_Alignment => 3,
      Invalid_Operation => 4,
      Not_Admitted      => 5,
      No_Address_Space  => 6,
      No_Record         => 7,
      No_Region         => 8,
      Already_Backed    => 9,
      Not_Backed        => 10,
      Out_Of_Memory     => 11,
      Over_Quota        => 12,
      Retirement_Failed => 13);

   function Refusal_Word (Why : Refusal) return Unsigned_64 is
     (Unsigned_64 (Refusal'Enum_Rep (Why)));

   function Is_Refusal_Word (Word : Unsigned_64) return Boolean is
     (Word in Refusal_Word (Refusal'First) .. Refusal_Word (Refusal'Last));

   function To_Refusal (Word : Unsigned_64) return Refusal is
     (Refusal'Enum_Val (Word))
   with Pre  => Is_Refusal_Word (Word),
        Post => Refusal_Word (To_Refusal'Result) = Word;
end Owned_Memory_ABI;
