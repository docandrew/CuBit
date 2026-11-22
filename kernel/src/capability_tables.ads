-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Per-process capability handle table (CAP-002 step 1,
-- docs/capability-handles.md, "Handles, not slots").
--
-- @description
-- A growable table of capabilities that only the kernel places entries in.
-- An entry is named by a Handle: its slot plus the slot's generation, one
-- word on the system-call ABI. Rules:
--
-- * Insert chooses the slot: the least recently freed one (a FIFO free
--   list), else the next never-used one. There is no insertion at a chosen
--   slot.
-- * Every use checks the generation. Closing or revoking an entry vacates
--   the slot; the next occupant gets the next generation, so a handle kept
--   after Close never reaches what was opened later (Stale). A slot whose
--   generation reached Generation_Limit is retired forever instead of
--   reused: a generation never wraps.
-- * A per-table limit (Handle_Limit: a count, or Kernel_Limit spelled out)
--   bounds the live entries; past it Insert reports Limit_Reached.
-- * Storage: a directory page of leaf pointers, then leaf pages of
--   Entries_Per_Leaf entries, all from Allocate_Page (the kernel charges each
--   page to the owner's memory quota). Pages are added on demand and kept
--   until Release_All: generations live in the leaves, and returning a leaf
--   early would forget them and let a stale handle resolve again.
--
-- Proved (gnatprove, see tests/capability-tables/README.md): absence of
-- run-time errors; Valid (the table invariant) is preserved by every
-- operation; Insert never overwrites a live entry and changes no other
-- entry; a handle resolves only with the live entry's own generation; every
-- slot's generation only grows, strictly before reuse; the live count equals
-- the number of live entries and never exceeds the limit; growth, close and
-- revoke preserve every other entry; pages charged equal pages held, and
-- Release_All returns every page (SPARK ownership: none is dropped).
--
-- Assumed (not proved): Allocate_Page/Release_Page honour their contract
-- (a fresh, page-aligned 4 KiB page, or OK = False); the caller serializes
-- every operation on one table (the kernel holds the owner's mailbox lock).
-- Regression-tested: LRF reuse order and free-list completeness.
--
-- Instantiate from a unit with `pragma Ada_2022` (the kernel defaults to
-- Ada 2012). Tests and proofs: tests/capability-tables/README.md.
-------------------------------------------------------------------------------
pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);

with Interfaces; use Interfaces;
with System;

generic
    -- What a slot holds (Capabilities.Capability in the kernel).
    type Element is private;
    -- Contents of a vacant slot: closed entries are scrubbed to this.
    Empty_Element : Element;
    -- Highest generation a slot may reach; a slot vacated at it is retired.
    Generation_Limit : Interfaces.Unsigned_32;
    -- Leaf pointers in the directory page: the table's structural capacity
    -- is Leaves_Per_Directory * Entries_Per_Leaf slots. At most
    -- Max_Directory_Leaves (one page of pointers).
    Leaves_Per_Directory : Positive;
    -- One 4 KiB page, charged to Owner; OK = False when refused (quota or
    -- memory). Page is page-aligned and not otherwise in use.
    with procedure Allocate_Page
      (Owner : Interfaces.Unsigned_64; Page : out System.Address;
       OK    : out Boolean);
    -- Return a page from Allocate_Page; refunds its charge.
    with procedure Release_Page (Page : System.Address);
package Capability_Tables with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Geometry
    ---------------------------------------------------------------------------
    Page_Bytes           : constant := 4096;
    Pointer_Bytes        : constant := 8;
    Max_Directory_Leaves : constant := Page_Bytes / Pointer_Bytes;
    Entries_Per_Leaf     : constant := 64;

    type Slot_Base is range 0 .. Max_Directory_Leaves * Entries_Per_Leaf;

    subtype Leaf_Number is Slot_Base range 0 .. Max_Directory_Leaves;
    -- Raises at instantiation if Leaves_Per_Directory is too large.
    Directory_Leaves : constant Leaf_Number := Leaf_Number (Leaves_Per_Directory);

    Max_Slots : constant Slot_Base := Directory_Leaves * Entries_Per_Leaf;

    subtype Slot_Count is Slot_Base range 0 .. Max_Slots;
    subtype Slot_Index is Slot_Count range 0 .. Max_Slots - 1;
    subtype Leaf_Count is Leaf_Number range 0 .. Directory_Leaves;
    subtype Leaf_Index is Leaf_Count range 0 .. Directory_Leaves - 1;
    subtype Leaf_Offset is Slot_Base range 0 .. Entries_Per_Leaf - 1;

    -- Pages a table holds: the directory plus its leaves.
    subtype Page_Count is Slot_Base range 0 .. Max_Directory_Leaves + 1;

    -- A free-list link: a slot, or No_Link.
    subtype Slot_Link is Slot_Count;
    No_Link : constant Slot_Link := Max_Slots;

    ---------------------------------------------------------------------------
    -- Generations
    ---------------------------------------------------------------------------
    subtype Generation is Interfaces.Unsigned_32;
    -- Never issued: a zero word never names an entry.
    No_Generation      : constant Generation := 0;
    Initial_Generation : constant Generation := 1;
    subtype Issued_Generation is
      Generation range Initial_Generation .. Generation'Last;
    -- Raises at instantiation if Generation_Limit is No_Generation.
    Last_Generation : constant Issued_Generation := Generation_Limit;

    ---------------------------------------------------------------------------
    -- Handle limit (like RLIMIT_NOFILE). Kernel_Limit means no limit of the
    -- program's own: only the kernel's, Max_Slots and the memory quota. It
    -- is a distinct value, not a large number.
    ---------------------------------------------------------------------------
    type Limit_Kind is (Bounded, Kernel_Limit);
    subtype Limit_Count is Slot_Base range 1 .. Slot_Base'Last;

    type Handle_Limit (Kind : Limit_Kind := Bounded) is record
        case Kind is
            when Bounded   => Most : Limit_Count := Limit_Count'Last;
            when Kernel_Limit => null;
        end case;
    end record;

    -- Default for every program class for now (user, 2026-10-12).
    Default_Handle_Count : constant Limit_Count := 4096;
    Default_Limit        : constant Handle_Limit :=
      (Kind => Bounded, Most => Default_Handle_Count);
    Kernel_Limit_Handles    : constant Handle_Limit := (Kind => Kernel_Limit);

    -- A bounded limit with Live entries is reached. (A Kernel_Limit table
    -- never reaches a limit of its own; at its capacity it is Table_Full.)
    function At_Limit (L : Handle_Limit; Live : Slot_Count) return Boolean is
      (L.Kind = Bounded and then Live >= L.Most);

    -- The most live entries a table with this limit may hold.
    function Bound (L : Handle_Limit) return Slot_Count is
      (case L.Kind is
         when Bounded   => Slot_Count (Slot_Base'Min (L.Most, Max_Slots)),
         when Kernel_Limit => Max_Slots);

    ---------------------------------------------------------------------------
    -- Handles: slot plus generation. No constructor from a slot number;
    -- From_Word only decodes a system-call argument (kernel-internal).
    ---------------------------------------------------------------------------
    type Handle is private;

    Slot_Field_Bits : constant := 32;

    function To_Word (H : Handle) return Unsigned_64;
    function From_Word (W : Unsigned_64) return Handle with
        Post => To_Word (From_Word'Result) = W;

    -- The handle's slot field is inside the structural capacity.
    function Names_Slot (H : Handle) return Boolean;
    function Slot_Of (H : Handle) return Slot_Index with
        Pre => Names_Slot (H);
    function Generation_Of (H : Handle) return Generation;

    ---------------------------------------------------------------------------
    -- Entries (visible for contracts; only this package builds a table)
    ---------------------------------------------------------------------------
    -- Free: vacant and on the free list. Retired: vacant at Last_Generation,
    -- never reused.
    type Slot_State is (Free, Live, Retired);
    -- Why the last occupant left; tells Stale from Revoked.
    type Vacancy_Cause is (Closed, Revoked);

    type Slot_Entry is record
        State : Slot_State    := Free;
        -- Generation of the current, or most recent, occupant.
        Gen   : Generation    := No_Generation;
        Cause : Vacancy_Cause := Closed;
        Next  : Slot_Link     := No_Link;
        Item  : Element       := Empty_Element;
    end record;

    -- A slot never used since its page was obtained.
    Unused_Entry : constant Slot_Entry :=
      (State => Free, Gen => No_Generation, Cause => Closed, Next => No_Link,
       Item => Empty_Element);

    ---------------------------------------------------------------------------
    -- Results. Callers must examine them; there is no Boolean to ignore.
    ---------------------------------------------------------------------------
    -- How a handle relates to the table:
    --   Current: names the live entry it was issued for.
    --   Invalid: never issued by this table (slot or generation unknown).
    --   Stale:   its entry was closed, or the slot has since been reused.
    --   Revoked: its entry was revoked and the slot not reused since.
    type Handle_Check is (Current, Invalid, Stale, Revoked);

    type Insert_Status is
      (Opened,         -- placed; the handle names it
       Limit_Reached,  -- the table's bounded Handle_Limit is reached
       Table_Full,     -- structural capacity reached (or every slot retired)
       Out_Of_Memory); -- Allocate_Page refused a page (quota or memory)

    type Insert_Result (Status : Insert_Status := Out_Of_Memory) is record
        case Status is
            when Opened => Opened_Handle : Handle;
            when others => null;
        end case;
    end record;

    type Lookup_Result (Check : Handle_Check := Invalid) is record
        case Check is
            when Current => Item : Element;
            when others  => null;
        end case;
    end record;

    -- Close and Revoke: the entry's former contents when Current.
    type Remove_Result (Check : Handle_Check := Invalid) is record
        case Check is
            when Current => Removed : Element;
            when others  => null;
        end case;
    end record;

    ---------------------------------------------------------------------------
    -- The table
    ---------------------------------------------------------------------------
    type Table is limited private with
        Default_Initial_Condition => Is_Empty (Table);

    -- Holds no page and no entry.
    function Is_Empty (T : Table) return Boolean;

    function Owner (T : Table) return Unsigned_64;
    function Limit (T : Table) return Handle_Limit;
    -- Slots 0 .. High_Water - 1 have been used; iteration bound.
    function High_Water (T : Table) return Slot_Count;
    function Live_Count (T : Table) return Slot_Count;
    function Pages_Held (T : Table) return Page_Count;

    -- The entry in slot S (Unused_Entry if its page is not held).
    function Entry_Of (T : Table; S : Slot_Index) return Slot_Entry;

    type Model_Array is array (Slot_Index) of Slot_Entry with Ghost;
    function Model (T : Table) return Model_Array with Ghost;

    -- Number of Live entries among M (0 .. N - 1).
    function Count_Live (M : Model_Array; N : Slot_Count) return Slot_Count
    with
        Ghost,
        Subprogram_Variant => (Decreases => N),
        Post => Count_Live'Result <= N;

    -- The table invariant (see the private part).
    function Valid (T : Table) return Boolean with Ghost;

    function Check (T : Table; H : Handle) return Handle_Check is
      (if not Names_Slot (H)
          or else Slot_Of (H) >= High_Water (T)
          or else Generation_Of (H) = No_Generation
          or else Generation_Of (H) > Entry_Of (T, Slot_Of (H)).Gen
       then Invalid
       elsif Generation_Of (H) < Entry_Of (T, Slot_Of (H)).Gen then Stale
       elsif Entry_Of (T, Slot_Of (H)).State = Live then Current
       elsif Entry_Of (T, Slot_Of (H)).Cause = Revoked then Revoked
       else Stale);

    -- Iteration over live entries (kernel scans): 0 .. High_Water - 1.
    function Is_Live_At (T : Table; S : Slot_Index) return Boolean is
      (S < High_Water (T) and then Entry_Of (T, S).State = Live);

    function Item_At (T : Table; S : Slot_Index) return Element is
      (Entry_Of (T, S).Item)
    with Pre => Is_Live_At (T, S);

    function Handle_At (T : Table; S : Slot_Index) return Handle with
        Pre  => Valid (T) and then Is_Live_At (T, S),
        Post => Names_Slot (Handle_At'Result)
                and then Slot_Of (Handle_At'Result) = S
                and then Generation_Of (Handle_At'Result) = Entry_Of (T, S).Gen
                and then Check (T, Handle_At'Result) = Current;

    ---------------------------------------------------------------------------
    -- Operations
    ---------------------------------------------------------------------------

    -- Make T an empty table with no owner and the default limit, whatever
    -- its fields held: for storage that is zeroed rather than default
    -- initialized (the kernel's process records). T must hold no page: it
    -- is never called on a table in use.
    procedure Clear (T : out Table) with
        Post => Is_Empty (T) and then Valid (T)
                and then Owner (T) = 0 and then Limit (T) = Default_Limit;

    -- Set the owner charged for pages and the handle limit of an empty table.
    procedure Configure
      (T : in out Table; New_Owner : Unsigned_64; New_Limit : Handle_Limit)
    with
        Pre  => Is_Empty (T),
        Post => Is_Empty (T) and then Valid (T)
                and then Owner (T) = New_Owner and then Limit (T) = New_Limit;

    -- Resolve a handle. Current only for the live entry's own generation.
    -- Inlined everywhere (the IPC path resolves a handle on every call; the
    -- kernel is built without cross-unit inlining).
    function Lookup (T : Table; H : Handle) return Lookup_Result with
        Inline_Always,
        Pre  => Valid (T),
        Post => Lookup'Result.Check = Check (T, H)
                and then
                (if Lookup'Result.Check = Current then
                    Names_Slot (H)
                    and then Entry_Of (T, Slot_Of (H)).State = Live
                    and then Entry_Of (T, Slot_Of (H)).Gen = Generation_Of (H)
                    and then Lookup'Result.Item = Entry_Of (T, Slot_Of (H)).Item);

    -- Place Item in a slot the table chooses and return its handle.
    procedure Insert (T : in out Table; Item : Element; Result : out Insert_Result)
    with
        Pre  => Valid (T) and then not Result'Constrained,
        Post => Insertion_Done (T, Item, Result,
                  Model (T)'Old, Live_Count (T)'Old, High_Water (T)'Old,
                  Pages_Held (T)'Old, Owner (T)'Old, Limit (T)'Old)
                and then Valid (T);

    -- Remove the entry a handle names (the process gave it up). Afterwards
    -- the handle is Stale; the slot is reused least-recently-freed first, or
    -- retired if its generation is at Last_Generation.
    procedure Close (T : in out Table; H : Handle; Result : out Remove_Result)
    with
        Pre  => Valid (T) and then not Result'Constrained,
        Post => Removal_Done (T, H, Result, Closed,
                  Model (T)'Old, Check (T, H)'Old, Live_Count (T)'Old,
                  High_Water (T)'Old, Pages_Held (T)'Old,
                  Owner (T)'Old, Limit (T)'Old)
                and then Valid (T);

    -- Remove the entry from outside (revocation). Afterwards the handle is
    -- Revoked until the slot is reused, then Stale.
    procedure Revoke (T : in out Table; H : Handle; Result : out Remove_Result)
    with
        Pre  => Valid (T) and then not Result'Constrained,
        Post => Removal_Done (T, H, Result, Revoked,
                  Model (T)'Old, Check (T, H)'Old, Live_Count (T)'Old,
                  High_Water (T)'Old, Pages_Held (T)'Old,
                  Owner (T)'Old, Limit (T)'Old)
                and then Valid (T);

    -- Release every page (process exit). Handles die with the process.
    procedure Release_All (T : in out Table) with
        Pre  => Valid (T),
        Post => Configuration_Kept (T, Owner (T)'Old, Limit (T)'Old)
                and then Is_Empty (T) and then Valid (T);

    ---------------------------------------------------------------------------
    -- Ghost relations used in the contracts above
    ---------------------------------------------------------------------------

    -- Nothing changed.
    function Unchanged (Before, After : Model_Array) return Boolean is
      (for all S in Slot_Index => After (S) = Before (S))
    with Ghost;

    -- Insert's effect: slot S was not live and now holds Item live under a
    -- generation greater than any it had; no other slot changed.
    function Opened_Into
      (Before, After : Model_Array; S : Slot_Index; Item : Element)
       return Boolean
    is
      (Before (S).State /= Live
       and then After (S).State = Live
       and then After (S).Item = Item
       and then After (S).Gen > Before (S).Gen
       and then (for all J in Slot_Index =>
                   (if J /= S then After (J) = Before (J))))
    with Ghost;

    -- Two entries agree except possibly their free-list link.
    function Same_But_Link (A, B : Slot_Entry) return Boolean is
      (A.State = B.State and then A.Gen = B.Gen and then A.Cause = B.Cause
       and then A.Item = B.Item)
    with Ghost;

    -- Close/Revoke's effect on a live slot S: vacated with its generation
    -- kept, the cause recorded and the item scrubbed; retired exactly when
    -- the generation is at Last_Generation; no other slot changed except a
    -- free-list link.
    function Vacated
      (Before, After : Model_Array; S : Slot_Index; Cause : Vacancy_Cause)
       return Boolean
    is
      (Before (S).State = Live
       and then After (S).State /= Live
       and then (After (S).State = Retired) = (Before (S).Gen = Last_Generation)
       and then After (S).Gen = Before (S).Gen
       and then After (S).Cause = Cause
       and then After (S).Item = Empty_Element
       and then (for all J in Slot_Index =>
                   (if J /= S then Same_But_Link (After (J), Before (J)))))
    with Ghost;

    function Removal_Done
      (T          : Table;
       H          : Handle;
       Result     : Remove_Result;
       Cause      : Vacancy_Cause;
       Old_Model  : Model_Array;
       Old_Check  : Handle_Check;
       Old_Live   : Slot_Count;
       Old_High   : Slot_Count;
       Old_Pages  : Page_Count;
       Old_Owner  : Unsigned_64;
       Old_Limit  : Handle_Limit) return Boolean
    is
      (Result.Check = Old_Check
       and then High_Water (T) = Old_High
       and then Pages_Held (T) = Old_Pages
       and then Owner (T) = Old_Owner and then Limit (T) = Old_Limit
       and then
       (if Old_Check = Current then
           Names_Slot (H)
           and then Vacated (Old_Model, Model (T), Slot_Of (H), Cause)
           and then Result.Removed = Old_Model (Slot_Of (H)).Item
           and then Old_Live > 0
           and then Live_Count (T) = Old_Live - 1
           and then Check (T, H) = (if Cause = Revoked then Revoked else Stale)
        else
           Unchanged (Old_Model, Model (T))
           and then Live_Count (T) = Old_Live))
    with Ghost;

    function Configuration_Kept
      (T : Table; Old_Owner : Unsigned_64; Old_Limit : Handle_Limit)
       return Boolean
    is (Owner (T) = Old_Owner and then Limit (T) = Old_Limit)
    with Ghost;

    function Insertion_Done
      (T          : Table;
       Item       : Element;
       Result     : Insert_Result;
       Old_Model  : Model_Array;
       Old_Live   : Slot_Count;
       Old_High   : Slot_Count;
       Old_Pages  : Page_Count;
       Old_Owner  : Unsigned_64;
       Old_Limit  : Handle_Limit) return Boolean
    is
      (Configuration_Kept (T, Old_Owner, Old_Limit)
       and then Live_Count (T) <= Bound (Limit (T))
       and then
       (case Result.Status is
          when Opened =>
            Names_Slot (Result.Opened_Handle)
            and then Opened_Into (Old_Model, Model (T),
                                  Slot_Of (Result.Opened_Handle), Item)
            and then Generation_Of (Result.Opened_Handle) =
                     Entry_Of (T, Slot_Of (Result.Opened_Handle)).Gen
            and then Check (T, Result.Opened_Handle) = Current
            and then Live_Count (T) = Old_Live + 1
            and then High_Water (T) >= Old_High
            and then Pages_Held (T) >= Old_Pages,
          when Limit_Reached =>
            At_Limit (Old_Limit, Old_Live)
            and then Unchanged (Old_Model, Model (T))
            and then Live_Count (T) = Old_Live
            and then High_Water (T) = Old_High
            and then Pages_Held (T) = Old_Pages,
          when Table_Full | Out_Of_Memory =>
            not At_Limit (Old_Limit, Old_Live)
            and then Unchanged (Old_Model, Model (T))
            and then Live_Count (T) = Old_Live
            and then High_Water (T) = Old_High
            and then Pages_Held (T) = Old_Pages))
    with Ghost;

private
    pragma Compile_Time_Error
      (Slot_Field_Bits /= 32, "handle word layout assumes a 32-bit slot field");

    Slot_Field_Mask : constant Unsigned_64 := 2 ** Slot_Field_Bits - 1;

    -- The slot field as an integer; only To_Word and From_Word convert
    -- between it and the word.
    type Slot_Field is range 0 .. 2 ** Slot_Field_Bits - 1;

    type Handle is record
        Slot : Slot_Field := 0;
        Gen  : Generation := No_Generation;
    end record;

    -- Word = generation << 32 | slot.
    function To_Word (H : Handle) return Unsigned_64 is
      (Shift_Left (Unsigned_64 (H.Gen), Slot_Field_Bits)
       or Unsigned_64 (Unsigned_32 (H.Slot)));

    function Names_Slot (H : Handle) return Boolean is
      (H.Slot < Slot_Field (Max_Slots));
    function Slot_Of (H : Handle) return Slot_Index is (Slot_Index (H.Slot));

    function Handle_For (S : Slot_Index; G : Generation) return Handle is
      ((Slot => Slot_Field (S), Gen => G))
    with Post => Names_Slot (Handle_For'Result)
                 and then Slot_Of (Handle_For'Result) = S
                 and then Generation_Of (Handle_For'Result) = G;
    function Generation_Of (H : Handle) return Generation is (H.Gen);

    ---------------------------------------------------------------------------
    -- Storage. Leaves and the directory are owning pointers (SPARK
    -- ownership: a page is never dropped or shared). Their memory comes
    -- from Allocate_Page through the adapter in the body.
    ---------------------------------------------------------------------------
    type Leaf is array (Leaf_Offset) of Slot_Entry;
    type Leaf_Access is access Leaf;
    type Directory is array (Leaf_Index) of Leaf_Access;
    type Directory_Access is access Directory;
    -- The page adapter converts page addresses to these pointers.
    pragma No_Strict_Aliasing (Leaf_Access);
    pragma No_Strict_Aliasing (Directory_Access);

    type Table is limited record
        Dir        : Directory_Access := null;
        Owner      : Unsigned_64      := 0;
        Limit      : Handle_Limit     := Default_Limit;
        High_Water : Slot_Count       := 0;
        Leaves     : Leaf_Count       := 0;
        Live       : Slot_Count       := 0;
        Free_Head  : Slot_Link        := No_Link;
        Free_Tail  : Slot_Link        := No_Link;
        -- Pages obtained from Allocate_Page and not yet released.
        Charged    : Page_Count       := 0;
    end record;

    function Leaf_Of (S : Slot_Index) return Leaf_Index is
      (Leaf_Index (S / Entries_Per_Leaf));
    function Offset_Of (S : Slot_Index) return Leaf_Offset is
      (S mod Entries_Per_Leaf);

    function Is_Empty (T : Table) return Boolean is
      (T.Dir = null and then T.High_Water = 0 and then T.Leaves = 0
       and then T.Live = 0 and then T.Charged = 0
       and then T.Free_Head = No_Link and then T.Free_Tail = No_Link);

    function Owner (T : Table) return Unsigned_64 is (T.Owner);
    function Limit (T : Table) return Handle_Limit is (T.Limit);
    function High_Water (T : Table) return Slot_Count is (T.High_Water);
    function Live_Count (T : Table) return Slot_Count is (T.Live);
    function Pages_Held (T : Table) return Page_Count is (T.Charged);

    function Count_Live (M : Model_Array; N : Slot_Count) return Slot_Count is
      (if N = 0 then 0
       else Count_Live (M, N - 1) + (if M (N - 1).State = Live then 1 else 0));

    ---------------------------------------------------------------------------
    -- The invariant, in parts.
    ---------------------------------------------------------------------------

    -- Leaves 0 .. Leaves - 1 are held, no others; the directory is held
    -- exactly when a leaf is; slots in use fit the leaves; Charged counts
    -- every page held.
    function Storage_Shape (T : Table) return Boolean is
      ((T.Dir = null) = (T.Leaves = 0)
       and then (if T.Dir /= null then
                   (for all L in Leaf_Index =>
                      (T.Dir (L) /= null) = (L < T.Leaves)))
       and then T.High_Water <= T.Leaves * Entries_Per_Leaf
       and then T.Charged = (if T.Dir = null then 0 else T.Leaves + 1))
    with Ghost;

    -- And no leaf is held before it is needed: the last one is in use.
    function Storage_Valid (T : Table) return Boolean is
      (Storage_Shape (T)
       and then (T.Leaves = 0
                 or else T.High_Water > (T.Leaves - 1) * Entries_Per_Leaf))
    with Ghost;

    -- Per-slot rules: used slots have been issued a generation; a free slot
    -- can still advance; a retired one cannot; slots past the high-water
    -- mark are untouched.
    function Entries_Valid (M : Model_Array; High : Slot_Count) return Boolean is
      (for all S in Slot_Index =>
         (if S < High then
             M (S).Gen >= Initial_Generation
             and then M (S).Gen <= Last_Generation
             and then (if M (S).State = Free then M (S).Gen < Last_Generation)
             and then (if M (S).State = Retired then
                          M (S).Gen = Last_Generation)
          else M (S) = Unused_Entry))
    with Ghost;

    -- A slot on the free list.
    function Listed (M : Model_Array; High : Slot_Count; S : Slot_Link)
       return Boolean
    is (S < High and then M (S).State = Free)
    with Ghost;

    -- The free list is closed under Next, its head has no predecessor and
    -- Next is injective, so taking the head never strands a link to a slot
    -- that becomes live. (Completeness and FIFO order are regression-tested,
    -- not proved: they do not affect safety.)
    function Free_List_Valid
      (M : Model_Array; High : Slot_Count; Head, Tail : Slot_Link)
       return Boolean
    is
      ((Head = No_Link) = (Tail = No_Link)
       and then (Head = No_Link or else Listed (M, High, Head))
       and then (Tail = No_Link
                 or else (Listed (M, High, Tail)
                          and then M (Tail).Next = No_Link))
       and then (for all S in Slot_Index =>
                   (if Listed (M, High, S) then
                       (M (S).Next = No_Link
                        or else (Listed (M, High, M (S).Next)
                                 and then M (S).Next /= Head))))
       and then (for all S in Slot_Index =>
                   (if Listed (M, High, S) and then M (S).Next /= No_Link then
                       (for all R in Slot_Index =>
                          (if R /= S and then Listed (M, High, R) then
                              M (S).Next /= M (R).Next)))))
    with Ghost;

    function Valid (T : Table) return Boolean is
      (Storage_Valid (T)
       and then Entries_Valid (Model (T), T.High_Water)
       and then Free_List_Valid
                  (Model (T), T.High_Water, T.Free_Head, T.Free_Tail)
       and then T.Live = Count_Live (Model (T), T.High_Water)
       and then T.Live <= Bound (T.Limit));
end Capability_Tables;
