with Interfaces; use Interfaces;
with Intel_GPU_Session_Limits;
package Intel_GPU_Render_Sessions with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- The driver's render-session table for one GPU service incarnation:
   -- slot allocation, session identities (tags) and the session lifecycle,
   -- including recycling a slot after its session has released everything.
   -- Only trusted admission code reserves/finalizes sessions. A request
   -- handler may Resolve using kernel-returned sender and authorityTag, never
   -- a tag/session supplied in message words. No kernel minting occurs here.
   --
   -- Slots and GuC context IDs: slot I's session uses GuC context
   -- Context_ID (I) and no other session does while I is in use, so slot
   -- recycling is GuC context-ID recycling. A slot becomes free (reusable)
   -- only through Recycle, which requires a retired session that holds
   -- nothing: in particular its GuC context hold ends only when the GuC
   -- acknowledged deregistration (Acknowledge_Deregistration), never on a
   -- request or a timeout. Tags never repeat: a recycled slot's next session
   -- gets a fresh tag, so stale requests and replies cannot reach it.
   --
   -- Storage: Grown slots are usable; the table grows (Grow) when no slot is
   -- free, up to Limit, the GuC context-ID space. Every field's default is
   -- all-zero bits, so zero-filled memory is an empty table (the driver keeps
   -- the table in committed-on-demand storage, Intel_GPU_Slot_Storage).
   Tag_Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   Tag_Last : constant Unsigned_64 := 16#4750_FFFF_FFFF_FFFF#;
   Limit : constant := Intel_GPU_Session_Limits.Session_Limit;
   subtype Slot_Count is Natural range 0 .. Limit;
   -- Zero: no slot.
   subtype Slot_Index is Slot_Count;
   subtype Slot is Slot_Index range 1 .. Limit;
   -- A tag is Tag_Base + Issue * Slot_Span + slot, Issue counting every
   -- reservation from one: unique, increasing, and its slot is O(1) to find.
   Slot_Span : constant := 2 ** 16;
   pragma Compile_Time_Error (Limit >= Slot_Span, "slot does not fit a tag");
   Issue_Limit : constant := (Tag_Last - Tag_Base) / Slot_Span;
   subtype Issue_Count is Unsigned_64 range 0 .. Issue_Limit;
   -- What a session holds that must be given back before its slot is reused.
   -- GuC_Context: the GuC may know its context (a registration may have
   -- reached it). Context_Image: its context image has a GGTT claim.
   type Holding is (GuC_Context, Context_Image);
   type Holdings is array (Holding) of Boolean;
   No_Holdings : constant Holdings := [others => False];
   -- Everything else a retired session must have given up, observed by the
   -- driver when it asks to recycle the slot.
   type Release_Facts is record
      -- No allocation is charged to the session any more: every buffer,
      -- table and context backing it had was confirmed retired.
      Backing_Released : Boolean := False;
      -- No CPU grant of its memory is outstanding.
      Grants_Returned : Boolean := False;
      -- The queue service holds nothing for the slot.
      Queue_Forgotten : Boolean := False;
      -- No driver operation in progress names the slot or the session.
      Unreferenced : Boolean := False;
   end record;
   function All_Released (Facts : Release_Facts) return Boolean is
     (Facts.Backing_Released and Facts.Grants_Returned and
      Facts.Queue_Forgotten and Facts.Unreferenced);
   type Registry is limited private
     with Default_Initial_Condition =>
       Grown (Registry) = 0 and not Failed (Registry) and not Exhausted (Registry);
   function Grown (Object : Registry) return Slot_Count;
   -- Proof model of the grown slots: what each holds and which session.
   type Slot_View is record
      Tag : Unsigned_64 := 0;          -- zero: free
      Retired : Boolean := False;
      Held : Holdings := No_Holdings;
   end record;
   type View is array (Slot range <>) of Slot_View;
   function Model (Object : Registry) return View
     with Ghost, Post => Model'Result'First = 1 and Model'Result'Last = Grown (Object);
   -- Every slot but Index is as it was.
   function Others_Unchanged (Before, After : View; Index : Slot_Index) return Boolean is
     (Before'First = After'First and then Before'Last = After'Last and then
      (for all I in Before'Range => (if I /= Index then After (I) = Before (I))))
     with Ghost;
   -- No slot's holdings changed.
   function Holdings_Unchanged (Before, After : View) return Boolean is
     (Before'First = After'First and then Before'Last = After'Last and then
      (for all I in Before'Range => After (I).Held = Before (I).Held))
     with Ghost;
   function Failed (Object : Registry) return Boolean;
   -- Every tag was issued: no further session can be admitted.
   function Exhausted (Object : Registry) return Boolean;
   -- Slot I's session's GuC context ID.
   function Context_ID (Index : Slot) return Unsigned_32 is
     (Unsigned_32 (Intel_GPU_Session_Limits.First_Context_ID +
                   Intel_GPU_Session_Limits.Boot_Contexts + Index - 1))
     with Post => Context_ID'Result < Intel_GPU_Session_Limits.Invalid_Context_ID;
   -- The slot a tag's layout names (zero for no slot), whether or not that
   -- tag is in the table.
   function Tag_Slot (Tag : Unsigned_64) return Slot_Index is
     (if Tag <= Tag_Base or else Tag > Tag_Last or else
         (Tag - Tag_Base) mod Slot_Span not in 1 .. Limit
      then 0 else Slot_Index ((Tag - Tag_Base) mod Slot_Span));
   --  Issued-record location only, NEVER authorization. Reserved and retired
   --  records remain locatable, including after quarantine, for exact cleanup
   --  bookkeeping, until the slot is recycled. Zero means no current record.
   --  Consumers must still Resolve using the kernel sender/stamp for work.
   function Storage_Index (Object : Registry; Tag : Unsigned_64) return Slot_Index
     with Post => (if Storage_Index'Result /= 0 then
       Tag > Tag_Base and Tag <= Tag_Last and
       Storage_Index'Result <= Grown (Object) and
       Storage_Index'Result = Tag_Slot (Tag));
   -- Reverse metadata lookup. Never fabricate an identity for a free slot.
   function Issued_Tag (Object : Registry; Index : Slot_Index) return Unsigned_64
     with Post => (if Issued_Tag'Result /= 0 then
       Index /= 0 and Storage_Index (Object, Issued_Tag'Result) = Index);
   -- The process identity the session was reserved for (zero when free).
   function Sender_Of (Object : Registry; Index : Slot_Index) return Unsigned_64
     with Post => (if Sender_Of'Result /= 0 then Issued_Tag (Object, Index) /= 0);
   -- This registry issued Tag's issue number. True also for tags of
   -- recycled slots: a request stamped with one (the kernel stamps the badge
   -- the broker minted for the session) is a stale request of a finished
   -- session, never of the slot's current one.
   function Was_Issued (Object : Registry; Tag : Unsigned_64) return Boolean
     with Post => (if Was_Issued'Result then Tag_Slot (Tag) /= 0);
   -- A free slot, or zero when every grown slot is in use (the caller may
   -- Grow and reserve again), the sender is zero, the table is quarantined
   -- or tags are exhausted. A free slot is never passed over.
   procedure Reserve
     (Object : in out Registry; Sender : Unsigned_64; Tag : out Unsigned_64)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       (if Tag = 0 then
          Failed (Object) or else Sender = 0 or else Exhausted (Object) or else
          (for all I in 1 .. Grown (Object) => Issued_Tag (Object, I) /= 0)
        else
          Sender /= 0 and Tag > Tag_Base and Tag <= Tag_Last and
          Storage_Index (Object, Tag) /= 0 and
          Sender_Of (Object, Storage_Index (Object, Tag)) = Sender and
          Issued_Tag (Object, Storage_Index (Object, Tag)) = Tag and
          Was_Issued (Object, Tag) and
          not Retired (Object, Storage_Index (Object, Tag)) and
          Holds_Nothing (Object, Storage_Index (Object, Tag)) and
          -- Only a free slot is taken; no other slot changes.
          Model (Object)'Old (Storage_Index (Object, Tag)).Tag = 0 and
          Others_Unchanged (Model (Object)'Old, Model (Object), Storage_Index (Object, Tag)))
       and then (if Tag = 0 then Model (Object) = Model (Object)'Old);
   -- Reserved tags do not resolve. After trusted grant completion, finalize
   -- exactly once; failed/uncertain grants retire the tag.
   procedure Finalize
     (Object : in out Registry; Sender, Tag : Unsigned_64;
      Granted : Boolean; Accepted : out Boolean)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       Holdings_Unchanged (Model (Object)'Old, Model (Object)) and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Storage_Index (Object, Tag));
   function Resolve
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post => (Resolve'Result = 0 or else
       (Sender /= 0 and Resolve'Result = Stamped_Tag and
        Stamped_Tag > Tag_Base and Stamped_Tag <= Tag_Last and
        Storage_Index (Object, Stamped_Tag) /= 0 and
        not Retired (Object, Storage_Index (Object, Stamped_Tag))));
   -- Closing revokes work authorization, not the retained cleanup identity.
   procedure Close (Object : in out Registry; Sender, Tag : Unsigned_64)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       Storage_Index (Object, Tag) = Storage_Index (Object, Tag)'Old and then
       Resolve (Object, Sender, Tag) = 0 and then
       Holdings_Unchanged (Model (Object)'Old, Model (Object)) and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Storage_Index (Object, Tag));
   -- Read-only cleanup observation only. Must NEVER substitute for Resolve
   -- in allocation, mapping, binding or submission authorization. Inputs are
   -- kernel envelope values, not application words. Quarantine fails closed.
   function Resolve_Retired
     (Object : Registry; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post => (Resolve_Retired'Result = 0 or else
       (Sender /= 0 and Resolve_Retired'Result = Stamped_Tag and
        Stamped_Tag > Tag_Base and Stamped_Tag <= Tag_Last and
        Resolve (Object, Sender, Stamped_Tag) = 0));
   -- Admission closed (finalized as failed, or closed), not yet recycled.
   function Retired (Object : Registry; Index : Slot_Index) return Boolean
     with Post => (if Retired'Result then Issued_Tag (Object, Index) /= 0);
   -- Sender alone never resolves an old session. PID reuse with a newly
   -- minted tag cannot recover earlier names. Nothing is recycled while
   -- quarantined.
   procedure Quarantine (Object : in out Registry)
     with Post => Failed (Object) and Grown (Object) = Grown (Object)'Old and
       Model (Object) = Model (Object)'Old;
   -- Back slots up to To. The caller has made the storage for every slot
   -- up to To usable (committed) first.
   procedure Grow (Object : in out Registry; To : Slot_Count)
     with Pre => To >= Grown (Object),
          Post => Grown (Object) = To and then
            Failed (Object) = Failed (Object)'Old and then
            -- Existing slots keep everything; new ones are free.
            (for all I in 1 .. Grown (Object)'Old =>
               Model (Object) (I) = Model (Object)'Old (I)) and then
            (for all I in Grown (Object)'Old + 1 .. To => Model (Object) (I).Tag = 0);
   function Holds (Object : Registry; Index : Slot_Index; What : Holding) return Boolean
     with Post => (if Holds'Result then Issued_Tag (Object, Index) /= 0);
   function Holds_Nothing (Object : Registry; Index : Slot_Index) return Boolean is
     (for all What in Holding => not Holds (Object, Index, What));
   -- The session in slot Index acquired What. No effect on a free slot.
   procedure Hold (Object : in out Registry; Index : Slot_Index; What : Holding)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       Issued_Tag (Object, Index) = Issued_Tag (Object, Index)'Old and then
       (if Issued_Tag (Object, Index) /= 0 then Holds (Object, Index, What)) and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Index);
   -- The session gave back its context image's GGTT claim.
   procedure Release_Image (Object : in out Registry; Index : Slot_Index)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       Issued_Tag (Object, Index) = Issued_Tag (Object, Index)'Old and then
       not Holds (Object, Index, Context_Image) and then
       Holds (Object, Index, GuC_Context) = Holds (Object, Index, GuC_Context)'Old and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Index);
   -- The GuC acknowledged deregistering the session's context (G2H
   -- DEREGISTER_DONE delivered to it). The only way a GuC_Context hold ends.
   procedure Acknowledge_Deregistration (Object : in out Registry; Index : Slot_Index)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       Issued_Tag (Object, Index) = Issued_Tag (Object, Index)'Old and then
       not Holds (Object, Index, GuC_Context) and then
       Holds (Object, Index, Context_Image) = Holds (Object, Index, Context_Image)'Old and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Index);
   -- Free slot Index for the next session: only a retired session that holds
   -- nothing and has released everything else, never while quarantined.
   procedure Recycle
     (Object : in out Registry; Index : Slot_Index; Facts : Release_Facts;
      Accepted : out Boolean)
     with Post => Grown (Object) = Grown (Object)'Old and then
       Failed (Object) = Failed (Object)'Old and then
       (if Accepted then
          not Failed (Object) and then All_Released (Facts) and then
          Retired (Object, Index)'Old and then Holds_Nothing (Object, Index)'Old and then
          Issued_Tag (Object, Index) = 0 and then
          Storage_Index (Object, Issued_Tag (Object, Index)'Old) = 0
        else Model (Object) = Model (Object)'Old) and then
       Others_Unchanged (Model (Object)'Old, Model (Object), Index);
private
   -- Free must stay the first literal: zero bits are a free slot.
   type Phase is (Free, Reserved, Active, Closed);
   type Entry_State is record
      Sender : Unsigned_64 := 0;
      Tag : Unsigned_64 := 0;
      State : Phase := Free;
      Held : Holdings := No_Holdings;
   end record;
   type Entries is array (Slot) of Entry_State;
   function Issue_Of (Tag : Unsigned_64) return Unsigned_64 is
     ((Tag - Tag_Base) / Slot_Span)
     with Pre => Tag > Tag_Base;
   function Entry_Valid (Item : Entry_State; Index : Slot; Issued : Issue_Count)
     return Boolean is
     (if Item.State = Free then
        Item.Tag = 0 and Item.Sender = 0 and Item.Held = No_Holdings
      else
        Item.Sender /= 0 and then Item.Tag > Tag_Base and then Item.Tag <= Tag_Last and then
        Tag_Slot (Item.Tag) = Index and then Issue_Of (Item.Tag) in 1 .. Issued);
   -- Items is last: the driver commits a table's storage as a prefix.
   type Registry is limited record
      Grown : Slot_Count := 0;
      Issued : Issue_Count := 0;
      Failed : Boolean := False;
      Items : Entries;
   end record
     with Type_Invariant =>
       (for all I in 1 .. Registry.Grown =>
          Entry_Valid (Registry.Items (I), I, Registry.Issued));
   function Grown (Object : Registry) return Slot_Count is (Object.Grown);
   function Failed (Object : Registry) return Boolean is (Object.Failed);
   function Exhausted (Object : Registry) return Boolean is (Object.Issued = Issue_Limit);
end Intel_GPU_Render_Sessions;
