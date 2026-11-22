pragma Ada_2022;
with Devmgr_PCI_Claims;
-- PCI readers/owners use the same allocator, growth reporting and lifetime
-- discipline as this family's port/memory ledgers. Serialized owner only.
generic
package Devmgr_Claim_Ledgers.PCI is
   package C renames Devmgr_PCI_Claims;
   package Q renames C.Q;
   package P renames C.P;
   use type Q.Record_ID;
   type Ledger is limited private;
   type Prepare_Status is (Prepared, Replay_Ready, Rejected);
   type Preparation is record
      Status : Prepare_Status := Rejected;
      Why : Q.Disposition := Q.Unavailable;
      Index : Natural := 0;
      ID : Q.Record_ID := Q.No_Record;
   end record;
   -- Backing is supplied only by the broker's independently validated root
   -- lookup. Valid shape here is not MCFG admission or kernel inspection.
   procedure Prepare
     (L : in out Ledger; Reader : P.Process_ID; Stamp : Q.Word;
      Request : Q.Request; Backing : C.Window; Result : out Preparation;
      Report : out Capacity_Report);
   procedure Complete
     (L : in out Ledger; Index : Positive; Expected : Q.Record_ID;
      Fact : C.Delivery; Accepted : out Boolean);
   function Count (L : Ledger) return Natural;
   function Claim_At (L : Ledger; Index : Positive) return C.Claim
     with Pre => Index <= Count (L);
   function Reader_Claims (L : Ledger; Key : Q.Bus_Key) return Natural;
   type Owner_Outcome is (Bound, Conflict, Invalid, Capacity_Refused, Closed);
   procedure Bind_Owner
     (L : in out Ledger; Key : C.Function_Key;
      Expected, Owner : P.Process_ID; Result : out Owner_Outcome;
      Report : out Capacity_Report);
   function Owner_Of (L : Ledger; Key : C.Function_Key) return P.Process_ID;
   -- Only a trusted confirmed-exit path may call this. Image5 has no such
   -- notification for procmgr's backend child; it must not infer an exit from
   -- a timeout, a missing role registration or an unknown reply.
   procedure Retire
     (L : in out Ledger; Life : P.Process_ID; Readers_Removed, Owners_Cleared : out Natural);
   procedure Close (L : in out Ledger);
private
   package Reader_Store is new Devmgr_Growable (C.Claim, Allocate, Release);
   package Owner_Store is new Devmgr_Growable (C.Function_Owner, Allocate, Release);
   type Ledger is limited record
      Rows : Reader_Store.Store;
      Owners : Owner_Store.Store;
      Next_ID : Q.Record_ID := 1;
      Closed : Boolean := False;
   end record;
end Devmgr_Claim_Ledgers.PCI;
