pragma Ada_2022;
with Interfaces; use Interfaces;

-------------------------------------------------------------------------------
-- Platform_Pool_Sets (ACPI-POOL-001)
--
-- The proved core of the platform-register pool: chipset and firmware MMIO
-- devmgr discovered independently (host bridge register blocks such as
-- MCHBAR, the PMC, the P2SB sideband window and its GPIO communities,
-- firmware-reserved spans), which the ACPI pair maps at addresses AML
-- computes at run time. In physical pages:
--
--   * State: the members and the claims, sized when devmgr declares the
--     pool (Members_Room, Claims_Room: derived from its discovery, each at
--     most a named ceiling). There are no fixed tables.
--       - Members: sorted, disjoint intervals, each of a typed Pool_Member_Kind
--         devmgr sets when it installs it: Device_BAR (a PCI function's
--         BAR: a driver bound to the device owns it whole) or
--         Chipset_Shared (a chipset block firmware keeps using after a
--         driver binds: MCHBAR, the PMC, the sideband window, a GPIO
--         community).
--       - Claims: device memory delegated to a process other than the
--         pool's owner (a bound driver's BARs and windows) that overlaps a
--         member, each naming its claimant (a process identity word).
--         Claims may overlap each other; they are withdrawn when their
--         claimant exits (Release_Claimant).
--   * Mapping_Table: one per mapping process (the holder), sized by what
--     it maps: the kernel grows it (Copy into a larger one) and charges it
--     to the holder's memory quota.
--
-- A mapping of pages Request by Holder is admitted iff Request lies wholly
-- inside one member and, when that member is a Device_BAR, no claim of
-- another process overlaps it (Decide). A Chipset_Shared member stays
-- admitted under a claim (the claim is SHARED: firmware keeps working, the
-- kernel logs the overlap). Consistent (S, T): no mapping of T conflicts,
-- that is, no Device_BAR mapping overlaps another process's claim.
-- Add_Mapping after an admitted decision keeps it; a claim followed by
-- Take_Conflicting until nothing is found restores it, and Take_Conflicting
-- only ever takes Device_BAR mappings (a shared claim never revokes:
-- Lemma_Shared_Claim). Capacity is a typed refusal (Members_Full,
-- Claims_Full, Table_Full), never a change of what is admitted.
--
-- The end-to-end flow (devmgr adds members before ACPI's _INI; ACPI maps
-- on demand; drivers' claims share chipset blocks and revoke only device
-- BARs) is described in Platform_Pool's spec.
--
-- Pure data and decisions: the caller (Platform_Pool) holds the lock,
-- allocates and grows the tables, unmaps the pages of a mapping it takes
-- and writes the log.
-------------------------------------------------------------------------------
package Platform_Pool_Sets with SPARK_Mode, Pure is

   -- Physical pages below the kernel's limit (Physical_Ranges.Address_Limit
   -- is 2 ** 47 bytes of 4 KiB pages).
   Page_Bytes : constant := 4096;
   Page_Limit : constant := 2 ** 35;
   subtype Page_Number is Unsigned_64 range 0 .. Page_Limit - 1;

   -- A process identity word (Process_Identities.To_Word); zero names none.
   subtype Identity is Unsigned_64;
   No_Identity : constant Identity := 0;

   -- Pages [First, Last], inclusive.
   type Interval is record
      First, Last : Page_Number := 0;
   end record;

   function Valid (I : Interval) return Boolean is (I.First <= I.Last);

   function Overlaps (A, B : Interval) return Boolean is
     (A.First <= B.Last and then B.First <= A.Last);

   -- Every page of A is a page of B.
   function Inside (A, B : Interval) return Boolean is
     (B.First <= A.First and then A.Last <= B.Last);

   ---------------------------------------------------------------------------
   -- Ceilings: the most a declaration or a growth may ask for (LIM-001).
   -- The sizes in use come from devmgr's discovery (members, claims) and
   -- from what a holder maps (mappings).
   ---------------------------------------------------------------------------
   Member_Ceiling  : constant := 2 ** 12;
   Claim_Ceiling   : constant := 2 ** 16;
   Mapping_Ceiling : constant := 2 ** 16;

   subtype Member_Count is Natural range 0 .. Member_Ceiling;
   subtype Member_Room is Member_Count range 1 .. Member_Ceiling;
   subtype Claim_Count is Natural range 0 .. Claim_Ceiling;
   subtype Claim_Room is Claim_Count range 1 .. Claim_Ceiling;
   subtype Mapping_Count is Natural range 0 .. Mapping_Ceiling;
   subtype Mapping_Room is Mapping_Count range 1 .. Mapping_Ceiling;

   -- What a member is, set by devmgr when it installs it.
   type Pool_Member_Kind is
     (Device_BAR,       -- a PCI function's BAR: a claim revokes and excludes
      Chipset_Shared);  -- a chipset block: a claim is shared, never revokes

   type Member is record
      Pages : Interval;
      Kind  : Pool_Member_Kind := Device_BAR;
   end record;
   type Member_Array is array (Member_Room range <>) of Member;

   type Claim is record
      Pages    : Interval;
      Claimant : Identity := No_Identity;
   end record;
   type Claim_Array is array (Claim_Room range <>) of Claim;

   -- Virtual_Page: the holder's page the first physical page is mapped at.
   -- Kind: the kind of the member it lies in, recorded when it is mapped.
   type Mapping is record
      Pages        : Interval;
      Holder       : Identity := No_Identity;
      Virtual_Page : Unsigned_64 := 0;
      Kind         : Pool_Member_Kind := Device_BAR;
   end record;
   type Mapping_Array is array (Mapping_Room range <>) of Mapping;

   type State (Members_Room : Member_Room; Claims_Room : Claim_Room) is record
      Member_Total : Member_Count := 0;
      Claim_Total  : Claim_Count := 0;
      Members      : Member_Array (1 .. Members_Room);
      Claims       : Claim_Array (1 .. Claims_Room);
   end record;

   type Mapping_Table (Room : Mapping_Room) is record
      Total : Mapping_Count := 0;
      Items : Mapping_Array (1 .. Room);
   end record;

   ---------------------------------------------------------------------------
   -- Invariants
   ---------------------------------------------------------------------------

   function Bounded (S : State) return Boolean is
     (S.Member_Total <= S.Members_Room and then S.Claim_Total <= S.Claims_Room);

   function Bounded (T : Mapping_Table) return Boolean is (T.Total <= T.Room);

   -- Members are valid, ascending and pairwise disjoint (each ends before
   -- the next begins).
   function Sorted_Disjoint (S : State) return Boolean is
     (S.Member_Total <= S.Members_Room
      and then (for all I in 1 .. S.Member_Total => Valid (S.Members (I).Pages))
      and then (for all I in 1 .. S.Member_Total - 1 =>
                  S.Members (I).Pages.Last < S.Members (I + 1).Pages.First));

   function Claims_Valid (S : State) return Boolean is
     (for all K in 1 .. S.Claim_Total => Valid (S.Claims (K).Pages))
   with Pre => Bounded (S);

   function Well_Formed (S : State) return Boolean is
     (Bounded (S) and then Sorted_Disjoint (S) and then Claims_Valid (S));

   function Table_Well_Formed (T : Mapping_Table) return Boolean is
     (Bounded (T) and then (for all K in 1 .. T.Total => Valid (T.Items (K).Pages)));

   -- Mapping M conflicts with the state: it lies in a Device_BAR member and
   -- a claim of a process other than its holder overlaps it. A mapping in
   -- a Chipset_Shared member never conflicts.
   function Conflicts (S : State; M : Mapping) return Boolean is
     (M.Kind = Device_BAR
      and then (for some K in 1 .. S.Claim_Total =>
                  S.Claims (K).Claimant /= M.Holder
                  and then Overlaps (S.Claims (K).Pages, M.Pages)))
   with Pre => Bounded (S);

   -- No mapping of T conflicts.
   function Consistent (S : State; T : Mapping_Table) return Boolean is
     (for all K in 1 .. T.Total => not Conflicts (S, T.Items (K)))
   with Pre => Bounded (S) and then Bounded (T);

   -- Every mapping of T lies inside a member of its recorded kind.
   function Kinds_Match (S : State; T : Mapping_Table) return Boolean is
     (for all K in 1 .. T.Total =>
        (for some I in 1 .. S.Member_Total =>
           Inside (T.Items (K).Pages, S.Members (I).Pages)
           and then S.Members (I).Kind = T.Items (K).Kind))
   with Pre => Bounded (S) and then Bounded (T);

   ---------------------------------------------------------------------------
   -- Members
   ---------------------------------------------------------------------------

   -- The member holding every page of Request, or 0.
   function Containing (S : State; Request : Interval) return Member_Count
   with Pre  => Sorted_Disjoint (S),
        Post => (if Containing'Result /= 0 then
                   Containing'Result <= S.Member_Total
                   and then Inside (Request, S.Members (Containing'Result).Pages)
                 else
                   (for all I in 1 .. S.Member_Total =>
                      not Inside (Request, S.Members (I).Pages)));

   -- The first member overlapping Request, or 0.
   function First_Overlap (S : State; Request : Interval) return Member_Count
   with Pre  => Sorted_Disjoint (S),
        Post => (if First_Overlap'Result /= 0 then
                   First_Overlap'Result <= S.Member_Total
                   and then Overlaps (Request, S.Members (First_Overlap'Result).Pages)
                 else
                   (for all I in 1 .. S.Member_Total =>
                      not Overlaps (Request, S.Members (I).Pages)));

   type Insert_Status is (Inserted, Invalid_Interval, Overlaps_Member, Members_Full);

   -- Add Item as a member. Refused (nothing changes) when Item's pages are
   -- not a valid interval, overlap a member, or the declared room is full.
   -- Inserted: the members are the old ones and Item, still sorted and
   -- disjoint.
   procedure Insert (S : in out State; Item : Member; Status : out Insert_Status)
   with Pre  => Well_Formed (S),
        Post => Well_Formed (S)
                and then S.Claims = S.Claims'Old
                and then S.Claim_Total = S.Claim_Total'Old
                and then (if Status = Inserted then
                            Valid (Item.Pages)
                            and then S.Member_Total = S.Member_Total'Old + 1
                            and then (for some I in 1 .. S.Member_Total =>
                                        S.Members (I) = Item)
                            and then (for all J in 1 .. S.Member_Total'Old =>
                                        (for some I in 1 .. S.Member_Total =>
                                           S.Members (I) = S.Members'Old (J)))
                            and then (for all I in 1 .. S.Member_Total =>
                                        S.Members (I) = Item
                                        or else (for some J in 1 .. S.Member_Total'Old =>
                                                   S.Members (I) = S.Members'Old (J)))
                          else S = S'Old)
                and then (Status = Invalid_Interval) = not Valid (Item.Pages)
                and then (if Status = Overlaps_Member then
                            First_Overlap (S, Item.Pages) /= 0)
                and then (if Status = Members_Full then
                            S.Member_Total = S.Members_Room)
                and then (if Valid (Item.Pages)
                             and then First_Overlap (S'Old, Item.Pages) = 0
                             and then S.Member_Total'Old < S.Members_Room
                          then Status = Inserted);

   ---------------------------------------------------------------------------
   -- Admission
   ---------------------------------------------------------------------------
   type Decision_Kind is (Admitted, Outside_Pool, Claimed, Invalid_Request);

   -- Member: the member holding Request (Admitted, Claimed). Claim: the
   -- first claim of another process overlapping it: the one that refuses
   -- it (Claimed, a Device_BAR member), or the one it shares the member
   -- with (Admitted in a Chipset_Shared member; 0 when none).
   type Decision is record
      Kind   : Decision_Kind := Invalid_Request;
      Member : Member_Count := 0;
      Claim  : Claim_Count := 0;
   end record;

   -- No claim of a process other than Holder overlaps Request.
   function Unclaimed (S : State; Holder : Identity; Request : Interval) return Boolean is
     (for all K in 1 .. S.Claim_Total =>
        S.Claims (K).Claimant = Holder
        or else not Overlaps (S.Claims (K).Pages, Request))
   with Pre => Bounded (S);

   -- Valid, wholly inside one member, and unclaimed by others unless that
   -- member is Chipset_Shared.
   function Admissible (S : State; Holder : Identity; Request : Interval) return Boolean is
     (Valid (Request)
      and then Containing (S, Request) /= 0
      and then (S.Members (Containing (S, Request)).Kind = Chipset_Shared
                or else Unclaimed (S, Holder, Request)))
   with Pre => Well_Formed (S);

   function Decide (S : State; Holder : Identity; Request : Interval) return Decision
   with Pre  => Well_Formed (S),
        Post => (Decide'Result.Kind = Admitted) = Admissible (S, Holder, Request)
                and then (if Decide'Result.Kind in Admitted | Claimed then
                            Decide'Result.Member = Containing (S, Request)
                            and then Decide'Result.Member in 1 .. S.Member_Total
                            and then Inside (Request, S.Members (Decide'Result.Member).Pages))
                and then (if Decide'Result.Kind = Claimed then
                            S.Members (Decide'Result.Member).Kind = Device_BAR)
                and then (if Decide'Result.Claim /= 0 then
                            Decide'Result.Kind in Admitted | Claimed
                            and then Decide'Result.Claim <= S.Claim_Total
                            and then S.Claims (Decide'Result.Claim).Claimant /= Holder
                            and then Overlaps (S.Claims (Decide'Result.Claim).Pages, Request))
                and then (if Decide'Result.Kind = Claimed then Decide'Result.Claim /= 0)
                and then (if Decide'Result.Kind = Admitted and then Decide'Result.Claim = 0 then
                            Unclaimed (S, Holder, Request))
                and then (if Decide'Result.Kind = Admitted and then Decide'Result.Claim /= 0 then
                            S.Members (Decide'Result.Member).Kind = Chipset_Shared)
                and then (if Decide'Result.Kind = Outside_Pool then
                            (for all I in 1 .. S.Member_Total =>
                               not Inside (Request, S.Members (I).Pages)))
                and then (Decide'Result.Kind = Invalid_Request) = not Valid (Request);

   ---------------------------------------------------------------------------
   -- Mappings (one table per holder)
   ---------------------------------------------------------------------------
   type Add_Status is (Added, Already_Recorded, Table_Full);

   -- Record M, admitted just now in the member Containing (S, M.Pages),
   -- whose kind it carries. The same mapping already recorded is not
   -- recorded twice. Table_Full: nothing changes; the caller grows the
   -- table (Copy) and adds again. Consistent and Kinds_Match are kept.
   procedure Add_Mapping
     (S : State; T : in out Mapping_Table; M : Mapping; Status : out Add_Status)
   with Pre  => Well_Formed (S) and then Table_Well_Formed (T)
                and then Admissible (S, M.Holder, M.Pages)
                and then M.Kind = S.Members (Containing (S, M.Pages)).Kind,
        Post => Table_Well_Formed (T)
                and then (if Consistent (S, T'Old) then Consistent (S, T))
                and then (if Kinds_Match (S, T'Old) then Kinds_Match (S, T))
                and then (if Status = Table_Full then
                            T = T'Old and then T.Total = T.Room)
                and then (if Status = Already_Recorded then T = T'Old)
                and then (if Status = Added then T.Total = T'Old.Total + 1)
                and then (if Status /= Table_Full then
                            (for some K in 1 .. T.Total => T.Items (K) = M));

   -- To holds From's mappings, in order (To is larger: the growth step).
   function Same_Items (From, To : Mapping_Table) return Boolean is
     (To.Total = From.Total
      and then (for all K in 1 .. From.Total => To.Items (K) = From.Items (K)))
   with Pre => Bounded (From) and then Bounded (To);

   procedure Copy (From : Mapping_Table; To : in out Mapping_Table)
   with Pre  => Table_Well_Formed (From) and then From.Total <= To.Room,
        Post => Table_Well_Formed (To) and then Same_Items (From, To);

   -- Take one recorded mapping that conflicts (Found), removing it; the
   -- caller unmaps its pages. It is always a Device_BAR mapping. Not
   -- Found: none conflicts (Consistent).
   procedure Take_Conflicting
     (S : State; T : in out Mapping_Table; Found : out Boolean; Taken : out Mapping)
   with Pre  => Well_Formed (S) and then Table_Well_Formed (T),
        Post => Table_Well_Formed (T)
                and then (if Found then
                            T.Total = T'Old.Total - 1
                            and then Conflicts (S, Taken)
                            and then Taken.Kind = Device_BAR
                            and then Valid (Taken.Pages)
                            and then (for all K in 1 .. T.Total =>
                                        (for some J in 1 .. T'Old.Total =>
                                           T.Items (K) = T'Old.Items (J)))
                          else T = T'Old and then Consistent (S, T));

   ---------------------------------------------------------------------------
   -- Claims
   ---------------------------------------------------------------------------
   type Claim_Status is
     (Recorded,         -- appended; the caller takes every conflicting mapping
      Duplicate,        -- the same pages for the same claimant: already held
      Outside_Members,  -- overlaps no member: it can never refuse a request
      Claims_Full);     -- the declared room is full: the caller refuses the
                        -- delegation (CAPACITY); nothing changes

   procedure Add_Claim
     (S : in out State; Pages : Interval; Claimant : Identity; Status : out Claim_Status)
   with Pre  => Well_Formed (S) and then Valid (Pages),
        Post => Well_Formed (S)
                and then S.Members = S.Members'Old
                and then S.Member_Total = S.Member_Total'Old
                and then (if Status = Recorded then
                            S.Claim_Total = S.Claim_Total'Old + 1
                            and then S.Claims (S.Claim_Total) = (Pages, Claimant)
                            and then (for all K in 1 .. S.Claim_Total'Old =>
                                        S.Claims (K) = S.Claims'Old (K))
                          else S = S'Old)
                and then (if Status = Duplicate then
                            (for some K in 1 .. S.Claim_Total =>
                               S.Claims (K) = (Pages, Claimant)))
                and then (if Status = Outside_Members then
                            (for all I in 1 .. S.Member_Total =>
                               not Overlaps (Pages, S.Members (I).Pages)))
                and then (if Status = Claims_Full then S.Claim_Total = S.Claims_Room)
                and then (if Status /= Duplicate and then First_Overlap (S, Pages) /= 0
                             and then S.Claim_Total'Old < S.Claims_Room
                          then Status = Recorded);

   -- Withdraw every claim of Claimant (it exited: its devices are unbound).
   procedure Release_Claimant (S : in out State; Claimant : Identity)
   with Pre  => Well_Formed (S),
        Post => Well_Formed (S)
                and then S.Members = S.Members'Old
                and then S.Member_Total = S.Member_Total'Old
                and then S.Claim_Total <= S.Claim_Total'Old
                and then (for all K in 1 .. S.Claim_Total =>
                            S.Claims (K).Claimant /= Claimant)
                and then (for all K in 1 .. S.Claim_Total =>
                            (for some J in 1 .. S.Claim_Total'Old =>
                               S.Claims (K) = S.Claims'Old (J)))
                and then (for all J in 1 .. S.Claim_Total'Old =>
                            (if S.Claims'Old (J).Claimant /= Claimant then
                               (for some K in 1 .. S.Claim_Total =>
                                  S.Claims (K) = S.Claims'Old (J))));

   ---------------------------------------------------------------------------
   -- Lemmas (ghost): what the operations above mean for every holder table.
   ---------------------------------------------------------------------------

   -- A claim that overlaps no member never overlaps an admissible request
   -- (so Add_Claim may leave it out: Outside_Members).
   procedure Lemma_Outside_Claim
     (S : State; Pages : Interval; Request : Interval)
   with Ghost, Global => null,
        Pre  => Well_Formed (S)
                and then (for all I in 1 .. S.Member_Total =>
                            not Overlaps (Pages, S.Members (I).Pages))
                and then Containing (S, Request) /= 0,
        Post => not Overlaps (Pages, Request);

   -- Fewer claims (each remaining one an old one) keep every table
   -- consistent.
   procedure Lemma_Fewer_Claims (Before, After : State; T : Mapping_Table)
   with Ghost, Global => null,
        Pre  => Bounded (Before) and then Bounded (After) and then Bounded (T)
                and then (for all K in 1 .. After.Claim_Total =>
                            (for some J in 1 .. Before.Claim_Total =>
                               After.Claims (K) = Before.Claims (J))),
        Post => (if Consistent (Before, T) then Consistent (After, T));

   -- A shared claim never revokes: a claim (appended by Add_Claim) that
   -- overlaps no Device_BAR member leaves every table whose mappings lie in
   -- members of their kind consistent.
   procedure Lemma_Shared_Claim
     (Before, After : State; T : Mapping_Table; Pages : Interval; Claimant : Identity)
   with Ghost, Global => null,
        Pre  => Well_Formed (Before) and then Well_Formed (After)
                and then Table_Well_Formed (T)
                and then After.Member_Total = Before.Member_Total
                and then (for all I in 1 .. Before.Member_Total =>
                            After.Members (I) = Before.Members (I))
                and then After.Claim_Total = Before.Claim_Total + 1
                and then (for all K in 1 .. Before.Claim_Total =>
                            After.Claims (K) = Before.Claims (K))
                and then After.Claims (After.Claim_Total) = (Pages, Claimant)
                and then (for all I in 1 .. Before.Member_Total =>
                            (if Before.Members (I).Kind = Device_BAR then
                               not Overlaps (Pages, Before.Members (I).Pages)))
                and then Kinds_Match (Before, T)
                and then Consistent (Before, T),
        Post => Consistent (After, T);

   -- A grown table (Copy) is as consistent as the one it replaces.
   procedure Lemma_Same_Items (S : State; From, To : Mapping_Table)
   with Ghost, Global => null,
        Pre  => Bounded (S) and then Bounded (From) and then Bounded (To)
                and then Same_Items (From, To),
        Post => (if Consistent (S, From) then Consistent (S, To))
                and then (if Kinds_Match (S, From) then Kinds_Match (S, To));

end Platform_Pool_Sets;
