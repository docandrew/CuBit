-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Address-space tags (x86 PCIDs): when a CPU may keep translations
--
-- Pure SPARK rules (tests/address-space-tags/README.md). With CR4.PCIDE set, the
-- TLB tags each translation with the 12-bit PCID of the address space that
-- made it, and a CR3 load may keep the loaded tag's translations instead of
-- dropping them. The kernel's own page tables use Kernel_Tag; each process
-- slot (PID) has a fixed Process_Tag. A PID is reused by a later process,
-- and so is its tag: translations a CPU cached for the earlier life must
-- never serve the later one.
--
-- Each tag has a life, advanced (Next_Life) whenever its slot gets a new
-- address space. Each CPU remembers, per tag, the life it last loaded the
-- tag for. A load keeps the tag's translations only when that is the tag's
-- current life (Keep); otherwise the load drops them, and the CPU records
-- the current life. The proved model (tests/address-space-tags) shows that
-- a CPU running a tag then holds no translation of any other life of it.
--
-- Removing a mapping from a live address space is not decided here: the
-- kernel then drops every tag's translations on every CPU (TLB_Shootdown,
-- TLB_Context.Flush_All), never only the current tag's.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Address_Space_Tags with SPARK_Mode => On, Pure is
    -- CR3 bits 11:0 hold the tag (Intel SDM Vol. 3A, 4.10.1).
    Tag_Bits : constant := 12;
    subtype Tag is Natural range 0 .. 2 ** Tag_Bits - 1;
    -- The kernel's own page tables: one life, never replaced.
    Kernel_Tag : constant Tag := 0;
    subtype Process_Tag is Tag range Kernel_Tag + 1 .. Tag'Last;

    type Life is new Unsigned_64;
    -- A tag never given out, or a CPU that never loaded it.
    No_Life : constant Life := 0;

    -- The tag's life once its slot has a new address space. A tag cannot
    -- run out of lives in practice (2**64 address spaces in one slot); the
    -- adapter refuses the address space if one ever did.
    function Next_Life (Current : Life) return Life is (Current + 1)
    with Pre => Current < Life'Last;

    -- A CPU that last loaded a tag for life Loaded may keep the tag's
    -- cached translations when loading it for life Current.
    function Keep (Loaded, Current : Life) return Boolean is
      (Current /= No_Life and then Loaded = Current);

    ---------------------------------------------------------------------------
    -- CR3 with CR4.PCIDE set: the root table's physical address in bits
    -- 51:12, the tag in bits 11:0, and on a load bit 63 set keeps the tag's
    -- translations (clear drops them). Bit 63 always reads back clear.
    ---------------------------------------------------------------------------
    Tag_Mask  : constant Unsigned_64 := 2 ** Tag_Bits - 1;
    Root_Mask : constant Unsigned_64 := 16#000F_FFFF_FFFF_F000#;
    Keep_Bit  : constant Unsigned_64 := 2 ** 63;

    function Valid_Root (Root : Unsigned_64) return Boolean is
      ((Root and not Root_Mask) = 0);

    function Tag_Of (CR3 : Unsigned_64) return Tag is
      (Tag (CR3 and Tag_Mask));

    function Root_Of (CR3 : Unsigned_64) return Unsigned_64 is
      (CR3 and Root_Mask);

    function Keeps (CR3 : Unsigned_64) return Boolean is
      ((CR3 and Keep_Bit) /= 0);

    function CR3_Value (Root : Unsigned_64; T : Tag; Keep_Translations : Boolean)
      return Unsigned_64
    with
        Pre  => Valid_Root (Root),
        Post => Root_Of (CR3_Value'Result) = Root and then
                Tag_Of (CR3_Value'Result) = T and then
                Keeps (CR3_Value'Result) = Keep_Translations;
end Address_Space_Tags;
