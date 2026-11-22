-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary ID bookkeeping for the dynamic process and thread tables
--
-- Pure SPARK ledger behind a two-level object table (docs/threads.md). It
-- decides which IDs are in use, keeps each ID's generation, and tells the
-- memory adapter when a page of records must be mapped (first entry
-- allocated) or may be returned (last entry released).
--
-- Generations live here, not in the records, so they survive page release:
-- a stale capability, reply or grant naming a released ID fails Current
-- instead of touching freed memory. A generation that would overflow retires
-- its ID forever, matching Capabilities.Operations.advanceGeneration.
--
-- IDs 1 .. Reserved_Last are handed out only by Allocate_Specific (per-CPU
-- idle threads, requested IDs). Allocate hands out the lowest free ID above
-- them, so pages stay dense and can empty.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

generic
    Max_Id           : Positive;
    Entries_Per_Page : Positive;
    Reserved_Last    : Natural;
    -- Highest generation an ID may reach; at the limit it is retired rather
    -- than reused. The process table uses 2**16 - 1 so a process generation
    -- fits the high half of a grant-slot generation (docs/threads.md).
    Generation_Limit : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
package Id_Ledger with SPARK_Mode => On is
    pragma Assert (Reserved_Last < Max_Id);

    subtype Id is Natural range 0 .. Max_Id;         -- 0: none
    subtype Valid_Id is Id range 1 .. Max_Id;
    subtype Page_Index is Natural range 0 .. Max_Id / Entries_Per_Page;
    subtype Generation is Unsigned_32;

    function Page_Of (I : Valid_Id) return Page_Index is
      (I / Entries_Per_Page);

    type Ledger is private;

    function Used (L : Ledger; I : Valid_Id) return Boolean;
    function Retired (L : Ledger; I : Valid_Id) return Boolean;
    function Generation_Of (L : Ledger; I : Valid_Id) return Generation;

    -- A reference (I, G) still names the live object it was made for.
    function Current (L : Ledger; I : Valid_Id; G : Generation) return Boolean
      is (Used (L, I) and then Generation_Of (L, I) = G);

    -- No other entry of I's page is in use.
    function Page_Empty_Except (L : Ledger; I : Valid_Id) return Boolean is
      (for all J in Valid_Id =>
         (if J /= I and then Page_Of (J) = Page_Of (I) then not Used (L, J)));

    function Page_Empty (L : Ledger; P : Page_Index) return Boolean is
      (for all J in Valid_Id => (if Page_Of (J) = P then not Used (L, J)));

    -- Ledger invariant: an ID in use was never retired (retirement happens
    -- only on release, and a retired ID is never allocated again).
    function Well_Formed (L : Ledger) return Boolean is
      (for all I in Valid_Id => (if Used (L, I) then not Retired (L, I)));

    function Free_For_Allocate (L : Ledger; I : Valid_Id) return Boolean is
      (I > Reserved_Last and then not Used (L, I) and then not Retired (L, I));

    procedure Initialize (L : out Ledger)
    with
        Global => null,
        Post   => Well_Formed (L) and then
                  (for all I in Valid_Id =>
                     not Used (L, I) and then not Retired (L, I) and then
                     Generation_Of (L, I) = 0);

    -- Lowest free non-reserved ID, or 0 if none. First_In_Page tells the
    -- adapter to map I's page before initializing the record.
    procedure Allocate (L : in out Ledger; I : out Id; First_In_Page : out Boolean)
    with
        Global => null,
        Pre    => Well_Formed (L),
        Post   =>
          Well_Formed (L) and then
          (if I = 0 then
               (for all J in Valid_Id => not Free_For_Allocate (L'Old, J))
               and then L = L'Old
           else
               Free_For_Allocate (L'Old, I)
               and then (for all J in Valid_Id range 1 .. I - 1 =>
                           not Free_For_Allocate (L'Old, J))
               and then Used (L, I) and then not Retired (L, I)
               and then First_In_Page = Page_Empty_Except (L'Old, I)
               and then Unchanged_Except (L'Old, L, I)
               and then Generation_Of (L, I) = Generation_Of (L'Old, I));

    -- Take a specific (typically reserved) ID if it is free.
    procedure Allocate_Specific (L             : in out Ledger;
                                 I             : Valid_Id;
                                 Success       : out Boolean;
                                 First_In_Page : out Boolean)
    with
        Global => null,
        Pre    => Well_Formed (L),
        Post   =>
          Well_Formed (L)
          and then Success = (not Used (L'Old, I) and then not Retired (L'Old, I))
          and then
          (if Success then
               Used (L, I) and then not Retired (L, I)
               and then First_In_Page = Page_Empty_Except (L'Old, I)
               and then Unchanged_Except (L'Old, L, I)
               and then Generation_Of (L, I) = Generation_Of (L'Old, I)
           else L = L'Old);

    -- Release a used ID. Its generation advances, so every reference made
    -- for the old object stops being Current; a saturated generation retires
    -- the ID instead. Last_In_Page tells the adapter it may return the page.
    -- Advance => False releases an ID whose generation Invalidate already
    -- advanced during teardown, so each life costs one generation.
    procedure Release (L            : in out Ledger;
                       I            : Valid_Id;
                       Last_In_Page : out Boolean;
                       Advance      : Boolean := True)
    with
        Global => null,
        Pre    => Well_Formed (L) and then Used (L, I),
        Post   =>
          Well_Formed (L)
          and then not Used (L, I)
          and then
          (if Generation_Of (L'Old, I) >= Generation_Limit then
               Retired (L, I)
               and then Generation_Of (L, I) = Generation_Of (L'Old, I)
           elsif Advance then
               Generation_Of (L, I) = Generation_Of (L'Old, I) + 1
               and then not Retired (L, I)
           else
               Generation_Of (L, I) = Generation_Of (L'Old, I)
               and then not Retired (L, I))
          and then Last_In_Page = Page_Empty_Except (L'Old, I)
          and then Unchanged_Except (L'Old, L, I);

    -- Advance a still-reserved ID's generation (teardown while grants are
    -- outstanding: references to the dead object must stop being Current
    -- before the ID is released). At the limit nothing changes and
    -- Saturated reports it; Release will then retire the ID.
    procedure Invalidate (L : in out Ledger; I : Valid_Id; Saturated : out Boolean)
    with
        Global => null,
        Pre    => Well_Formed (L) and then Used (L, I),
        Post   =>
          Well_Formed (L)
          and then Used (L, I) and then not Retired (L, I)
          and then Saturated = (Generation_Of (L'Old, I) >= Generation_Limit)
          and then
          (if Saturated then Generation_Of (L, I) = Generation_Of (L'Old, I)
           else Generation_Of (L, I) = Generation_Of (L'Old, I) + 1)
          and then Unchanged_Except (L'Old, L, I);

    -- Every other ID keeps its state.
    function Unchanged_Except (Before, After : Ledger; I : Valid_Id) return Boolean
      is (for all J in Valid_Id =>
            (if J /= I then
                Used (After, J) = Used (Before, J)
                and then Retired (After, J) = Retired (Before, J)
                and then Generation_Of (After, J) = Generation_Of (Before, J)))
      with Ghost;

    -- A released ID's old references are dead: whatever the ID is reused
    -- for, a reference made before the release never becomes Current again.
    procedure Prove_Release_Invalidates (L : Ledger; I : Valid_Id)
    with
        Ghost,
        Global => null,
        Pre    => Well_Formed (L) and then Used (L, I);

private
    type Entry_State is record
        Used       : Boolean := False;
        Retired    : Boolean := False;
        Generation : Interfaces.Unsigned_32 := 0;
    end record;

    type Ledger is array (Valid_Id) of Entry_State;

    function Used (L : Ledger; I : Valid_Id) return Boolean is (L (I).Used);
    function Retired (L : Ledger; I : Valid_Id) return Boolean is (L (I).Retired);
    function Generation_Of (L : Ledger; I : Valid_Id) return Generation is
      (L (I).Generation);
end Id_Ledger;
