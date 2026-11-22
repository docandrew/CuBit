-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The IOMMU domain ledger (IOMMU-001): which PCI requester has a DMA domain,
-- which process it is bound to, and every run of frames its I/O page tables
-- may map, with the unmap -> IOTLB invalidation -> release order.
--
-- @description
-- One domain per (remapping unit, requester ID), created when devmgr binds
-- the device or, for a device a reserved memory region (RMRR) names, at
-- IOMMU bring-up. A domain is never reused for another requester. Its owner
-- is the driver process the device is bound to (its process identity word);
-- when that process ends the domain keeps its requester and its retained
-- mappings but has no owner until the device is bound again.
--
-- A mapping is a run of frames one domain's page tables translate:
--  * Granted: DMA memory ALLOC_DMA gave the domain's owner for this device;
--  * Reserved_Region: an RMRR range the firmware named this requester for.
-- Its state moves only forward: Mapped (in the page tables) -> Stale (page
-- table entries cleared, the IOTLB may still translate it) -> Flushed (an
-- IOTLB invalidation for its domain completed after it went stale) ->
-- Unused (released; the frames may be reused).
--
-- Proved (gnatprove level 2, tests/iommu), as postconditions over the
-- ledger, every operation preserving Isolated:
--  * domain isolation: a mapping of domain D is either Granted, and then no
--    other live mapping in any domain and no reserved region overlaps it, or
--    Reserved_Region, and then a reserved region names D's requester on D's
--    unit and covers it. So a domain's page tables hold only frames granted
--    to that device plus its own RMRRs (given that the kernel glue writes
--    exactly the ledger's Mapped runs: Iommu, not proved, hosted-tested);
--  * one domain per (unit, requester);
--  * ordering: Release frees a run only from Flushed, Complete_Flush moves to
--    Flushed only runs that went Stale no later than the invalidation's
--    stamp, and only Begin_Unmap makes a run Stale; a released Granted run
--    overlaps no live mapping (Reusable), so its frames may be reused.
-- Assumed (kernel glue, Iommu): Complete_Flush is called only after the
-- unit's IOTLB invalidation for the domain, issued after the stamp was
-- taken, has completed.
--
-- Pure table operations, no locking: Iommu holds the one live ledger under
-- its spinlock.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with IO_Page_Tables;

package IOMMU_Domains with
    SPARK_Mode => On
is
    -- Named ceilings (LIM-001): PCI functions with a domain, live mapped
    -- runs, and (unit, requester, range) reserved-region pairs.
    Maximum_Domains  : constant := 128;
    Maximum_Mappings : constant := 512;
    Maximum_Reserved : constant := 32;
    Maximum_Units    : constant := 8;

    subtype Unit_Number is Positive range 1 .. Maximum_Units;
    subtype Requester_ID is IO_Page_Tables.Requester_ID;

    -- The owner's process identity word (Process_Identities.To_Word).
    type Owner_Key is new Unsigned_64;
    No_Owner : constant Owner_Key := 0;

    -- Physical frame numbers (52-bit physical addresses) and run lengths:
    -- up to 4 GiB in one run, more than any DMA block or RMRR.
    Frame_Limit : constant := 2 ** 40;
    subtype Frame_Number is Unsigned_64 range 0 .. Frame_Limit - 1;
    Maximum_Run_Pages : constant := 2 ** 20;
    subtype Page_Count is Unsigned_64 range 1 .. Maximum_Run_Pages;

    subtype Domain_Count is Natural range 0 .. Maximum_Domains;
    subtype Domain_Index is Positive range 1 .. Maximum_Domains;
    subtype Mapping_Index is Positive range 1 .. Maximum_Mappings;
    subtype Reserved_Count is Natural range 0 .. Maximum_Reserved;
    subtype Reserved_Index is Positive range 1 .. Maximum_Reserved;

    type Domain is record
        Unit      : Unit_Number  := 1;
        Requester : Requester_ID := 0;
        Owner     : Owner_Key    := No_Owner;
        -- Unmap stamps: each Begin_Unmap in this domain takes the next one.
        Epoch     : Unsigned_64  := 0;
    end record;
    type Domain_Table is array (Domain_Index) of Domain;

    type Mapping_State is (Unused, Mapped, Stale, Flushed);
    type Mapping_Kind is (Granted, Reserved_Region);

    type Mapping is record
        State    : Mapping_State := Unused;
        Kind     : Mapping_Kind  := Granted;
        Domain   : Domain_Index  := 1;
        First    : Frame_Number  := 0;
        Pages    : Page_Count    := 1;
        Writable : Boolean       := True;
        -- Granted: the process ALLOC_DMA gave the frames to.
        Grantee  : Owner_Key     := No_Owner;
        Stamp    : Unsigned_64   := 0;      -- the Begin_Unmap stamp
    end record;
    type Mapping_Table is array (Mapping_Index) of Mapping;

    type Reserved_Range is record
        Unit      : Unit_Number  := 1;
        Requester : Requester_ID := 0;
        First     : Frame_Number := 0;
        Pages     : Page_Count   := 1;
    end record;
    type Reserved_Table is array (Reserved_Index) of Reserved_Range;

    type Ledger is record
        Domain_Total   : Domain_Count := 0;
        Domains        : Domain_Table;
        Mappings       : Mapping_Table;
        Reserved_Total : Reserved_Count := 0;
        Reserved       : Reserved_Table;
    end record;

    ---------------------------------------------------------------------------
    -- Predicates
    ---------------------------------------------------------------------------
    function Overlap (A_First : Frame_Number; A_Pages : Page_Count;
                      B_First : Frame_Number; B_Pages : Page_Count) return Boolean is
      (A_First < B_First + B_Pages and then B_First < A_First + A_Pages);

    function Live (M : Mapping) return Boolean is (M.State /= Unused);

    -- Translated by the page tables (or possibly still by the IOTLB).
    function Reachable (M : Mapping) return Boolean is (M.State in Mapped | Stale);

    function Covers (R : Reserved_Range; First : Frame_Number; Pages : Page_Count)
      return Boolean is
      (First >= R.First and then First - R.First <= R.Pages - 1
       and then Pages <= R.Pages - (First - R.First));

    function Reserved_For
      (Table : Reserved_Table; Total : Reserved_Count; Unit : Unit_Number;
       Requester : Requester_ID; First : Frame_Number; Pages : Page_Count)
      return Boolean is
      (for some R in 1 .. Total =>
         Table (R).Unit = Unit and then Table (R).Requester = Requester
         and then Covers (Table (R), First, Pages));

    function Touches_Reserved
      (Table : Reserved_Table; Total : Reserved_Count; First : Frame_Number;
       Pages : Page_Count) return Boolean is
      (for some R in 1 .. Total =>
         Overlap (Table (R).First, Table (R).Pages, First, Pages));

    -- Domain isolation, in four parts. Every live mapping names a domain;
    -- a Granted run overlaps no other live run (of any domain or kind) and
    -- no reserved region; a Reserved_Region run is covered by a reserved
    -- region that names its domain's requester on its domain's unit.
    function Domains_Named (L : Ledger) return Boolean is
      (for all M in Mapping_Index =>
         (if Live (L.Mappings (M)) then L.Mappings (M).Domain <= L.Domain_Total));

    function Granted_Exclusive (L : Ledger) return Boolean is
      (for all A in Mapping_Index =>
         (for all B in Mapping_Index =>
            (if A /= B and then Live (L.Mappings (A)) and then Live (L.Mappings (B))
               and then (L.Mappings (A).Kind = Granted or else L.Mappings (B).Kind = Granted)
             then not Overlap (L.Mappings (A).First, L.Mappings (A).Pages,
                               L.Mappings (B).First, L.Mappings (B).Pages))));

    function Granted_Clear (L : Ledger) return Boolean is
      (for all M in Mapping_Index =>
         (if Live (L.Mappings (M)) and then L.Mappings (M).Kind = Granted then
            not Touches_Reserved (L.Reserved, L.Reserved_Total, L.Mappings (M).First, L.Mappings (M).Pages)));

    function Reserved_Named (L : Ledger) return Boolean is
      (for all M in Mapping_Index =>
         (if Live (L.Mappings (M)) and then L.Mappings (M).Kind = Reserved_Region
            and then L.Mappings (M).Domain <= L.Domain_Total
          then Reserved_For (L.Reserved, L.Reserved_Total, L.Domains (L.Mappings (M).Domain).Unit,
                             L.Domains (L.Mappings (M).Domain).Requester,
                             L.Mappings (M).First, L.Mappings (M).Pages)));

    function Stamps_Bounded (L : Ledger) return Boolean is
      (for all M in Mapping_Index =>
         (if Live (L.Mappings (M)) then
            L.Mappings (M).Stamp <= L.Domains (L.Mappings (M).Domain).Epoch));

    function Distinct_Domains (L : Ledger) return Boolean is
      (for all A in 1 .. L.Domain_Total =>
         (for all B in 1 .. L.Domain_Total =>
            (if A /= B then
               L.Domains (A).Unit /= L.Domains (B).Unit
               or else L.Domains (A).Requester /= L.Domains (B).Requester)));

    -- The ledger invariant: domain isolation, one domain per requester,
    -- and stamps never run ahead of their domain.
    function Isolated (L : Ledger) return Boolean is
      (Distinct_Domains (L) and then Domains_Named (L) and then Granted_Exclusive (L)
       and then Granted_Clear (L) and then Reserved_Named (L)
       and then Stamps_Bounded (L));

    -- No live mapping of any domain overlaps the run: its frames may be
    -- reused.
    function Reusable (L : Ledger; First : Frame_Number; Pages : Page_Count)
      return Boolean is
      (for all N in Mapping_Index =>
         (if Live (L.Mappings (N)) then
            not Overlap (First, Pages, L.Mappings (N).First, L.Mappings (N).Pages)));

    function Find_Domain (L : Ledger; Unit : Unit_Number; Requester : Requester_ID)
      return Domain_Count
    with Post =>
      (if Find_Domain'Result /= 0 then
         Find_Domain'Result <= L.Domain_Total
         and then L.Domains (Find_Domain'Result).Unit = Unit
         and then L.Domains (Find_Domain'Result).Requester = Requester
       else
         (for all D in 1 .. L.Domain_Total =>
            L.Domains (D).Unit /= Unit or else L.Domains (D).Requester /= Requester));

    ---------------------------------------------------------------------------
    -- Boot: reserved regions (RMRR scope pairs), before any grant.
    ---------------------------------------------------------------------------
    type Admit_Status is (Admitted, Capacity, Conflicts);

    procedure Admit_Reserved
      (L : in out Ledger; Unit : Unit_Number; Requester : Requester_ID;
       First : Frame_Number; Pages : Page_Count; Status : out Admit_Status)
    with
        Pre  => Isolated (L),
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Domains = L.Domains'Old
          and then L.Mappings = L.Mappings'Old
          and then (if Status = Admitted then
                      Reserved_For (L.Reserved, L.Reserved_Total, Unit, Requester, First, Pages)
                    else L = L'Old);

    ---------------------------------------------------------------------------
    -- Domains
    ---------------------------------------------------------------------------
    type Domain_Status is (Ready, Capacity);

    -- The domain of (Unit, Requester), created ownerless if it has none.
    procedure Create_Domain
      (L : in out Ledger; Unit : Unit_Number; Requester : Requester_ID;
       D : out Domain_Index; Created : out Boolean; Status : out Domain_Status)
    with
        Pre  => Isolated (L),
        Post => Isolated (L)
          and then L.Mappings = L.Mappings'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (if Status = Ready then
                      D <= L.Domain_Total
                      and then L.Domains (D).Unit = Unit
                      and then L.Domains (D).Requester = Requester
                      and then (if Created then
                                  L.Domain_Total = L.Domain_Total'Old + 1
                                  and then D = L.Domain_Total
                                  and then L.Domains (D).Owner = No_Owner
                                else L = L'Old)
                    else L = L'Old and then not Created);

    type Bind_Status is (Bound, Busy);

    -- Bind D to Owner unless another live owner holds it.
    procedure Bind_Owner
      (L : in out Ledger; D : Domain_Index; Owner : Owner_Key; Status : out Bind_Status)
    with
        Pre  => Isolated (L) and then D <= L.Domain_Total and then Owner /= No_Owner,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Mappings = L.Mappings'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (Status = Bound) =
                     (L.Domains'Old (D).Owner in No_Owner | Owner)
          and then (if Status = Bound then
                      L.Domains = (L.Domains'Old with delta
                                     D => (L.Domains'Old (D) with delta Owner => Owner))
                    else L = L'Old);

    -- Owner has ended: its domains keep their mappings, lose their owner.
    procedure Release_Owner (L : in out Ledger; Owner : Owner_Key)
    with
        Pre  => Isolated (L) and then Owner /= No_Owner,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Mappings = L.Mappings'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (for all D in Domain_Index =>
                      L.Domains (D) =
                        (if L.Domains'Old (D).Owner = Owner
                         then (L.Domains'Old (D) with delta Owner => No_Owner)
                         else L.Domains'Old (D)))
          and then (for all D in 1 .. L.Domain_Total => L.Domains (D).Owner /= Owner);

    ---------------------------------------------------------------------------
    -- Mapping
    ---------------------------------------------------------------------------
    type Map_Status is (Added, Not_Owner, Overlaps, Not_Reserved, Capacity);

    -- Frames ALLOC_DMA just gave Owner for D's device. Refused unless Owner
    -- holds D, and unless no live mapping in any domain and no reserved
    -- region overlaps the run.
    procedure Map_Grant
      (L : in out Ledger; D : Domain_Index; Owner : Owner_Key;
       First : Frame_Number; Pages : Page_Count; Writable : Boolean;
       M : out Mapping_Index; Status : out Map_Status)
    with
        Pre  => Isolated (L) and then D <= L.Domain_Total,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Domains = L.Domains'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (if Status = Added then
                      Owner /= No_Owner
                      and then L.Domains (D).Owner = Owner
                      and then not Live (L.Mappings'Old (M))
                      and then L.Mappings (M) =
                        (State => Mapped, Kind => Granted, Domain => D,
                         First => First, Pages => Pages, Writable => Writable,
                         Grantee => Owner, Stamp => 0)
                      and then (for all N in Mapping_Index =>
                                  (if N /= M then L.Mappings (N) = L.Mappings'Old (N)))
                    else L = L'Old)
          and then (if Status /= Added and then Status /= Capacity then
                      (Status = Not_Owner) =
                        (Owner = No_Owner or else L.Domains (D).Owner /= Owner));

    -- An RMRR run a reserved region names D's requester for (identity).
    procedure Map_Reserved
      (L : in out Ledger; D : Domain_Index; First : Frame_Number; Pages : Page_Count;
       M : out Mapping_Index; Status : out Map_Status)
    with
        Pre  => Isolated (L) and then D <= L.Domain_Total,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Domains = L.Domains'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (if Status = Added then
                      Reserved_For (L.Reserved, L.Reserved_Total, L.Domains (D).Unit, L.Domains (D).Requester,
                                    First, Pages)
                      and then not Live (L.Mappings'Old (M))
                      and then L.Mappings (M).State = Mapped
                      and then L.Mappings (M).Kind = Reserved_Region
                      and then L.Mappings (M).Domain = D
                      and then L.Mappings (M).First = First
                      and then L.Mappings (M).Pages = Pages
                      and then (for all N in Mapping_Index =>
                                  (if N /= M then L.Mappings (N) = L.Mappings'Old (N)))
                    else L = L'Old);

    -- The live Mapped Granted run of D that starts at First with Pages
    -- frames, or 0.
    function Find_Grant
      (L : Ledger; D : Domain_Index; First : Frame_Number; Pages : Page_Count)
      return Natural
    with Post =>
      (if Find_Grant'Result /= 0 then
         Find_Grant'Result in Mapping_Index
         and then L.Mappings (Find_Grant'Result).State = Mapped
         and then L.Mappings (Find_Grant'Result).Kind = Granted
         and then L.Mappings (Find_Grant'Result).Domain = D
         and then L.Mappings (Find_Grant'Result).First = First
         and then L.Mappings (Find_Grant'Result).Pages = Pages);

    ---------------------------------------------------------------------------
    -- Unmapping, in order
    ---------------------------------------------------------------------------

    -- The glue has cleared M's page-table entries: M goes Stale with the
    -- next stamp of its domain (returned).
    procedure Begin_Unmap (L : in out Ledger; M : Mapping_Index; Stamp : out Unsigned_64)
    with
        Pre  => Isolated (L) and then L.Mappings (M).State = Mapped
          and then L.Mappings (M).Kind = Granted
          and then L.Domains (L.Mappings (M).Domain).Epoch < Unsigned_64'Last,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then Stamp = L.Domains'Old (L.Mappings'Old (M).Domain).Epoch + 1
          and then L.Domains = (L.Domains'Old with delta
                                  L.Mappings'Old (M).Domain =>
                                    (L.Domains'Old (L.Mappings'Old (M).Domain)
                                       with delta Epoch => Stamp))
          and then L.Mappings = (L.Mappings'Old with delta
                                   M => (L.Mappings'Old (M) with delta
                                           State => Stale, Stamp => Stamp));

    -- The unit finished an IOTLB invalidation for D that it began after
    -- stamp Through was taken: every run of D that went Stale at or before
    -- Through is Flushed. (Assumption on the caller, see above.)
    procedure Complete_Flush (L : in out Ledger; D : Domain_Index; Through : Unsigned_64)
    with
        Pre  => Isolated (L) and then D <= L.Domain_Total
          and then Through <= L.Domains (D).Epoch,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Domains = L.Domains'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (for all N in Mapping_Index =>
                      L.Mappings (N) =
                        (if L.Mappings'Old (N).State = Stale
                           and then L.Mappings'Old (N).Domain = D
                           and then L.Mappings'Old (N).Stamp <= Through
                         then (L.Mappings'Old (N) with delta State => Flushed)
                         else L.Mappings'Old (N)));

    type Release_Status is (Released, Not_Flushed);

    -- Forget a Flushed Granted run: afterwards its frames are Reusable.
    procedure Release
      (L : in out Ledger; M : Mapping_Index; Status : out Release_Status)
    with
        Pre  => Isolated (L) and then L.Mappings (M).Kind = Granted,
        Post => Isolated (L)
          and then L.Domain_Total = L.Domain_Total'Old
          and then L.Domains = L.Domains'Old
          and then L.Reserved_Total = L.Reserved_Total'Old
          and then L.Reserved = L.Reserved'Old
          and then (Status = Released) = (L.Mappings'Old (M).State = Flushed)
          and then (if Status = Released then
                      not Live (L.Mappings (M))
                      and then (for all N in Mapping_Index =>
                                  (if N /= M then L.Mappings (N) = L.Mappings'Old (N)))
                      and then Reusable (L, L.Mappings'Old (M).First,
                                         L.Mappings'Old (M).Pages)
                    else L = L'Old);
end IOMMU_Domains;
