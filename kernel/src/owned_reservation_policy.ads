with Interfaces;
-- Owned memory, the one region API (ABI-002). The arithmetic of every
-- owned-memory call, proved (tests/owned-regions, SPARK level 2):
--
--   RESERVE_OWNED_REGION   reserve page-aligned virtual capacity, no RAM;
--   CHANGE_OWNED_RANGE     one operation on a page range of one region:
--                          commit it, decommit it, or set its access;
--   RELEASE_OWNED_REGION   give the whole region back.
--
-- No flags: each operation is one value of Range_Operation and does one
-- thing. Commit backs every page of the range with a fresh zero page,
-- read/write, and is refused if any page of it is already backed (no
-- double mapping, no silent overwrite). Decommit is madvise(MADV_DONTNEED)
-- with explicit recommit: the frames go back (and their charge to the
-- owner's quota), the range stays reserved, and the next commit gives
-- zero pages; touching a decommitted page before that faults, so an
-- allocator's use-after-decommit is caught rather than silently re-backed.
-- The access operations change committed pages only, and never beyond the
-- region's grant: read/write, never executable.
--
-- Ownership, generation, overlap between regions, frame allocation and TLB
-- retirement remain obligations of the kernel adapter
-- (Process.Owned_Memory), not properties of this policy.
package Owned_Reservation_Policy with SPARK_Mode => On is
   use Interfaces;
   Page_Bytes : constant Unsigned_64 := 4096;
   Maximum_Reserved_Bytes : constant Unsigned_64 := 2 * 1024 * 1024 * 1024;
   -- The most pages one locked step backs or retires. A commit zeroes and
   -- maps every page, and a release unmaps and frees them, under grantLock
   -- and the owned-memory and address-space locks with interrupts off;
   -- every CPU's wait and grant paths take grantLock. So this bounds how
   -- long one process's heap growth can hold up the whole system (audio
   -- periods, input). 16 MiB steps (4096 pages) held them for up to
   -- 141 ms under nested KVM (2026-11).
   Pages_Per_Locked_Step : constant := 64;
   -- One commit backs at most this much; larger ranges are committed in
   -- steps (CuBit.Owned_Reservations, libc's mmap, CuAlloc).
   Maximum_Commit_Bytes : constant Unsigned_64 := Pages_Per_Locked_Step * Page_Bytes;
   Maximum_Region_Pages : constant := 2 * 1024 * 1024 * 1024 / 4096;
   subtype Page_Count is Unsigned_64 range 0 .. Maximum_Region_Pages;

   -- Capacity consumes virtual addresses only.
   function Valid_Capacity (Capacity : Unsigned_64) return Boolean is
     (Capacity > 0 and then Capacity <= Maximum_Reserved_Bytes and then
      Capacity mod Page_Bytes = 0);

   -- A region's base may be asked to be a multiple of an alignment:
   -- 0 (callers that pass none) or a power of two up to 1 GiB; anything up
   -- to a page means a page. CuAlloc places its arenas this way, so the
   -- arena of an address is the address with its low bits cleared.
   Maximum_Alignment : constant Unsigned_64 := 2 ** 30;
   function Valid_Alignment (Alignment : Unsigned_64) return Boolean is
     (Alignment = 0 or else
      (Alignment <= Maximum_Alignment and then (Alignment and (Alignment - 1)) = 0));
   function Effective_Alignment (Alignment : Unsigned_64) return Unsigned_64 is
     (Unsigned_64'Max (Alignment, Page_Bytes))
   with Pre => Valid_Alignment (Alignment),
        Post => Effective_Alignment'Result in Page_Bytes .. Maximum_Alignment and then
                (Effective_Alignment'Result and (Effective_Alignment'Result - 1)) = 0;

   -- The first multiple of Alignment at or above Candidate; 0 when there
   -- is none (it would wrap).
   function Align_Up (Candidate, Alignment : Unsigned_64) return Unsigned_64
   with Pre => Alignment in Page_Bytes .. Maximum_Alignment and then
               (Alignment and (Alignment - 1)) = 0,
        Post => (if Align_Up'Result /= 0 or else Candidate = 0 then
                   Align_Up'Result >= Candidate and then
                   (Align_Up'Result and (Alignment - 1)) = 0 and then
                   Align_Up'Result - Candidate < Alignment);

   ---------------------------------------------------------------------------
   -- CHANGE_OWNED_RANGE's operation: its wire value is the position.
   ---------------------------------------------------------------------------
   type Range_Operation is
     (Commit, Decommit, Make_Inaccessible, Make_Read_Only, Make_Read_Write);
   subtype Access_Operation is Range_Operation
     range Make_Inaccessible .. Make_Read_Write;

   -- Untrusted wire value to operation: only the five positions decode.
   procedure Decode (Raw : Unsigned_64; Operation : out Range_Operation;
                     Valid : out Boolean)
   with Post => Valid = (Raw <= Range_Operation'Pos (Range_Operation'Last))
                and then (if Valid then Range_Operation'Pos (Operation) = Raw);

   -- What a committed page may be: there is no executable value, so no
   -- operation can make owned memory executable.
   type Page_Access is (Inaccessible, Read_Only, Read_Write);
   -- The most access any page of a region may have (its grant). Commit
   -- installs it; the access operations may only narrow or restore it.
   Region_Grant : constant Page_Access := Read_Write;

   function Access_Of (Operation : Access_Operation) return Page_Access is
     (case Operation is
        when Make_Inaccessible => Inaccessible,
        when Make_Read_Only    => Read_Only,
        when Make_Read_Write   => Read_Write)
   with Post => Access_Of'Result <= Region_Grant
                and then (Access_Of'Result = Read_Write) = (Operation = Make_Read_Write)
                and then (Access_Of'Result = Inaccessible) = (Operation = Make_Inaccessible);

   ---------------------------------------------------------------------------
   -- Ranges.
   ---------------------------------------------------------------------------
   -- A live region: page-aligned, valid capacity, not wrapping.
   function Valid_Region (Base, Bytes : Unsigned_64) return Boolean is
     (Base mod Page_Bytes = 0 and then Valid_Capacity (Bytes) and then
      Base <= Unsigned_64'Last - Bytes);

   -- A nonempty, page-aligned range wholly inside the region.
   function Within (Region_Base, Region_Bytes, Address, Bytes : Unsigned_64)
                    return Boolean is
     (Valid_Region (Region_Base, Region_Bytes) and then
      Bytes > 0 and then Bytes mod Page_Bytes = 0 and then
      Address mod Page_Bytes = 0 and then Address >= Region_Base and then
      Address - Region_Base < Region_Bytes and then
      Bytes <= Region_Bytes - (Address - Region_Base))
   with Post => (if Within'Result then
                   Address <= Unsigned_64'Last - Bytes and then
                   Address + Bytes <= Region_Base + Region_Bytes and then
                   Bytes / Page_Bytes in 1 .. Maximum_Region_Pages);

   -- A commit's size is bounded (Maximum_Commit_Bytes).
   function Commit_Admitted (Region_Base, Region_Bytes, Address, Bytes : Unsigned_64)
                             return Boolean is
     (Within (Region_Base, Region_Bytes, Address, Bytes) and then
      Bytes <= Maximum_Commit_Bytes)
   with Post => (if Commit_Admitted'Result then
                   Bytes / Page_Bytes in 1 .. Maximum_Commit_Bytes / Page_Bytes);

   -- A committed run: Pages backed pages from Base, inside a region.
   function Valid_Run (Base : Unsigned_64; Pages : Page_Count) return Boolean is
     (Base mod Page_Bytes = 0 and then Pages > 0 and then
      Base <= Unsigned_64'Last - Pages * Page_Bytes);

   -- Whether the half-open run and range share a page. Commit refuses a
   -- range that overlaps any run: no page is ever mapped twice.
   function Overlaps (Run_Base : Unsigned_64; Run_Pages : Page_Count;
                      Address, Bytes : Unsigned_64) return Boolean is
     (Address < Run_Base + Run_Pages * Page_Bytes and then
      Run_Base < Address + Bytes)
   with Pre => Valid_Run (Run_Base, Run_Pages) and then
               Bytes > 0 and then Address <= Unsigned_64'Last - Bytes,
        -- They share a page exactly when the later start precedes the
        -- earlier end.
        Post => Overlaps'Result =
                (Unsigned_64'Max (Run_Base, Address) <
                 Unsigned_64'Min (Run_Base + Run_Pages * Page_Bytes, Address + Bytes));

   -- A run that ends exactly where the range starts: a commit there may
   -- extend it in place (when its frames are adjacent, the kernel's check).
   function Adjoins (Run_Base : Unsigned_64; Run_Pages : Page_Count;
                     Address : Unsigned_64) return Boolean is
     (Run_Base + Run_Pages * Page_Bytes = Address)
   with Pre => Valid_Run (Run_Base, Run_Pages);

   -- A run cut by a range it overlaps, in pages from the run's base: the
   -- Lower pages below the range stay, the Dropped pages are the ones
   -- the range covers, the Upper pages above it stay.
   --
   -- The kernel keeps a run's frames in descending address order (the
   -- highest page first), so the same split names frame-list positions:
   -- the Upper pages are positions 0 .. Upper - 1, the dropped ones the
   -- next Dropped, the Lower ones the rest.
   type Run_Split is record
      Lower, Dropped, Upper : Page_Count;
   end record;

   function Split (Run_Base : Unsigned_64; Run_Pages : Page_Count;
                   Address, Bytes : Unsigned_64) return Run_Split
   with Pre => Valid_Run (Run_Base, Run_Pages) and then
               Bytes > 0 and then Bytes mod Page_Bytes = 0 and then
               Address mod Page_Bytes = 0 and then
               Address <= Unsigned_64'Last - Bytes and then
               Overlaps (Run_Base, Run_Pages, Address, Bytes),
        Post => Split'Result.Dropped > 0
          and then Split'Result.Lower + Split'Result.Dropped + Split'Result.Upper = Run_Pages
          -- The dropped pages start at the later of the two starts ...
          and then Run_Base + Split'Result.Lower * Page_Bytes =
                   Unsigned_64'Max (Run_Base, Address)
          -- ... and end at the earlier of the two ends: exactly the overlap.
          and then Run_Base + (Split'Result.Lower + Split'Result.Dropped) * Page_Bytes =
                   Unsigned_64'Min (Run_Base + Run_Pages * Page_Bytes,
                                    Address + Bytes)
          -- A middle cut (pages kept on both sides) needs the range
          -- strictly inside the run.
          and then (Split'Result.Lower > 0) = (Address > Run_Base)
          and then (Split'Result.Upper > 0) =
                   (Address + Bytes < Run_Base + Run_Pages * Page_Bytes);
end Owned_Reservation_Policy;
