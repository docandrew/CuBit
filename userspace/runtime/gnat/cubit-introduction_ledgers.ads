pragma Ada_2022;
with Interfaces; use Interfaces;

--  A launcher's kept launch decisions for late introduction
--  (CuBit.Late_Introduction, ABI-002; CAP-003's run-time binding path):
--  procmgr's, and devmgr's for the children it starts itself.
--
--  At launch, an optional service entry whose only lack is a running
--  provider is Absent in the program's startup table, and the launcher keeps
--  the decision it reached for it, after every check a granted entry
--  passes (declared in the manifest, approved, a startup-only role approved
--  by the plan): the holder (the launched process), the entry's index in
--  its startup table, the provider role, the rights and the badge. When a
--  provider of that role starts, procmgr introduces each holder waiting for
--  one, minting exactly the kept decision; each decision is used once.
--
--  The ledger is the only source of introductions, and Keep is the only way
--  a decision enters it: what is proved here is that an introduction is
--  always one kept decision of that very holder and index (Find, Next_For),
--  that a decision is used at most once (Remove), and that a holder's
--  decisions go with it when it exits (Forget), so a later process never
--  inherits them. That Keep is called only after the launch checks passed
--  is the launchers' call sites (procmgr's Spawn_Attempt Mint_Service,
--  devmgr's Devmgr_Launches.Mint), shown by the headless
--  late-introduction test, not proved.
--
--  Bounded by a named ceiling (Maximum_Decisions): a decision that does not
--  fit is not kept, and the program simply runs without that service, as
--  it would have before late introduction (procmgr logs it).
package CuBit.Introduction_Ledgers with SPARK_Mode is

   subtype Process_Word is Unsigned_64;
   No_Holder : constant Process_Word := 0;

   Maximum_Decisions : constant := 256;
   subtype Slot_Count is Natural range 0 .. Maximum_Decisions;
   subtype Slot is Positive range 1 .. Maximum_Decisions;

   --  How the holder is introduced: answering its kept Await call
   --  (Saved_Reply), or, for devmgr, a call procmgr makes down to it
   --  offering the endpoint (CuBit.Service_Exchange.Available).
   type Delivery is (Saved_Reply, Offer);

   type Decision is record
      Holder : Process_Word := No_Holder;
      --  Its startup-table index (Saved_Reply), or devmgr's request number.
      Index  : Unsigned_64 := 0;
      --  The provider's role (procmgr's Provider_Role_Of).
      Role   : Unsigned_64 := 0;
      --  The manifest rights procmgr kept (Manifest_Rights), as a mask.
      Rights : Unsigned_64 := 0;
      --  The badge minted with it; 0: an untagged copy.
      Tag    : Unsigned_64 := 0;
      Way    : Delivery := Saved_Reply;
      --  Saved_Reply: the holder's Await call is kept, waiting.
      Waiting : Boolean := False;
   end record;

   function Used (D : Decision) return Boolean is (D.Holder /= No_Holder);

   type Ledger is array (Slot) of Decision;
   Empty : constant Ledger := [others => (others => <>)];

   --  Keep D in a free slot. Not kept (Into = 0, L unchanged) when L is full
   --  or already holds a decision for D's holder and index.
   procedure Keep (L : in out Ledger; D : Decision; Into : out Slot_Count)
   with Pre  => Used (D),
        Post => (if Into = 0 then L = L'Old
                 else not Used (L'Old (Into)) and then L (Into) = D
                      and then (for all S in Slot => (if S /= Into then L (S) = L'Old (S))));

   --  The decision kept for Holder's entry Index, or 0.
   function Find (L : Ledger; Holder : Process_Word; Index : Unsigned_64) return Slot_Count
   with Post => (if Find'Result = 0 then
                   (for all S in Slot =>
                      not (Used (L (S)) and then L (S).Holder = Holder and then L (S).Index = Index))
                 else Used (L (Find'Result)) and then L (Find'Result).Holder = Holder
                      and then L (Find'Result).Index = Index);

   --  The first decision at or after From for a provider of Role that can
   --  be delivered now (an Offer, or a Saved_Reply whose call is waiting),
   --  or 0.
   function Next_For (L : Ledger; Role : Unsigned_64; From : Slot) return Slot_Count
   with Post => (if Next_For'Result /= 0 then
                   Next_For'Result >= From and then Used (L (Next_For'Result))
                   and then L (Next_For'Result).Role = Role
                   and then (L (Next_For'Result).Way = Offer or else L (Next_For'Result).Waiting));

   --  The holder's call is now kept, waiting (Saved_Reply only).
   procedure Set_Waiting (L : in out Ledger; At_Slot : Slot)
   with Pre  => Used (L (At_Slot)) and then L (At_Slot).Way = Saved_Reply,
        Post => L (At_Slot) = (L'Old (At_Slot) with delta Waiting => True)
                and then (for all S in Slot => (if S /= At_Slot then L (S) = L'Old (S)));

   --  The decision was used (or dropped): its slot is free.
   procedure Remove (L : in out Ledger; At_Slot : Slot)
   with Post => not Used (L (At_Slot))
                and then (for all S in Slot => (if S /= At_Slot then L (S) = L'Old (S)));

   --  Holder exited: none of its decisions remain; every other is as it was.
   procedure Forget (L : in out Ledger; Holder : Process_Word)
   with Pre  => Holder /= No_Holder,
        Post => (for all S in Slot =>
                   (if Used (L'Old (S)) and then L'Old (S).Holder = Holder
                    then not Used (L (S)) else L (S) = L'Old (S)));

   function Count (L : Ledger) return Slot_Count
   with Post => Count'Result <= Maximum_Decisions;

end CuBit.Introduction_Ledgers;
