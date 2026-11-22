------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The connection table: 4-tuples to connection slots, with handles that
--  cannot outlive their connection (docs/netstack-redesign.md, "Scale and
--  denial of service").
--
--  Slots come from a free stack (constant time). Lookup hashes the
--  4-tuple with SipHash under a boot-time secret into one of
--  Bucket_Count buckets of Bucket_Size entries and scans that bucket; a
--  remote peer cannot choose tuples that collide without the secret. A
--  full bucket refuses the connection, as a dropped SYN would.
--
--  Proved (tests/net-tcp):
--  - Find returns a slot exactly when an open connection has that
--    4-tuple, and that connection's slot;
--  - Insert refuses a 4-tuple that is already open;
--  - a handle is valid only while its connection is open: closing bumps
--    the slot's generation, so an old handle never reaches a reused slot
--    (until the generation wraps, after 2**32 reuses of one slot);
--  - Count is the number of open connections, and nothing else changes
--    when one opens or closes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SipHash;
with TCP_Isn; use type TCP_Isn.Endpoints;

generic
   Max_Connections : Positive;
   Bucket_Count    : Positive;
   Bucket_Size     : Positive;
package Connection_Table with SPARK_Mode is

   subtype Connection_Count is Natural range 0 .. Max_Connections;
   --  A connection's slot, or none.
   subtype Maybe_Slot is Connection_Count;
   No_Slot : constant Maybe_Slot := 0;
   subtype Slot is Maybe_Slot range 1 .. Max_Connections;
   subtype Endpoints is TCP_Isn.Endpoints;

   type Handle is record
      Index      : Slot := 1;
      Generation : Unsigned_32 := 0;
   end record;

   type Table is private;

   function Valid (T : Table) return Boolean with Ghost;
   function Open (T : Table; S : Slot) return Boolean;
   function Key (T : Table; S : Slot) return Endpoints with Pre => Open (T, S);
   function Generation (T : Table; S : Slot) return Unsigned_32;
   function Count (T : Table) return Connection_Count;

   function Current (T : Table; H : Handle) return Boolean is
     (Open (T, H.Index) and then Generation (T, H.Index) = H.Generation);

   --  Nothing changes but slot S.
   function Same_Except (A, B : Table; S : Slot) return Boolean is
     (for all X in Slot =>
        (if X /= S then
           Open (A, X) = Open (B, X) and then Generation (A, X) = Generation (B, X) and then
           (if Open (A, X) then Key (A, X) = Key (B, X))));

   procedure Initialize (T : out Table; Secret : SipHash.Key) with
     Post => Valid (T) and then Count (T) = 0 and then
             (for all S in Slot => not Open (T, S));

   --  The open connection with this 4-tuple, or No_Slot.
   function Find (T : Table; E : Endpoints) return Maybe_Slot with
     Pre  => Valid (T),
     Post => (if Find'Result = No_Slot then (for all S in Slot => (if Open (T, S) then Key (T, S) /= E))
              else Find'Result in Slot and then Open (T, Find'Result) and then
                   Key (T, Find'Result) = E);

   type Insert_Status is (Inserted, Exists, Table_Full, Bucket_Full);

   procedure Insert (T : in out Table; E : Endpoints; H : out Handle; Status : out Insert_Status)
   with
     Pre  => Valid (T),
     Post => Valid (T) and then
             (Status = Exists) = (for some S in Slot => Open (T'Old, S) and then Key (T'Old, S) = E) and then
             (if Status = Inserted then
                not Open (T'Old, H.Index) and then Current (T, H) and then
                H.Generation = Generation (T'Old, H.Index) and then
                Key (T, H.Index) = E and then Count (T) = Count (T'Old) + 1 and then
                Same_Except (T, T'Old, H.Index)
              else T = T'Old);

   procedure Remove (T : in out Table; H : Handle) with
     Pre  => Valid (T) and then Current (T, H),
     Post => Valid (T) and then not Open (T, H.Index) and then not Current (T, H) and then
             --  The slot's next occupant gets a newer generation, so H
             --  never becomes current again (until the counter wraps).
             Generation (T, H.Index) = H.Generation + 1 and then
             Count (T) = Count (T'Old) - 1 and then Same_Except (T, T'Old, H.Index);

private
   subtype Bucket_Id is Natural range 0 .. Bucket_Count - 1;
   --  A place within a bucket, or none.
   subtype Maybe_Position is Natural range 0 .. Bucket_Size;
   subtype Position is Maybe_Position range 1 .. Bucket_Size;
   type Entry_Array is array (Position) of Maybe_Slot;
   type Bucket_Array is array (Bucket_Id) of Entry_Array;

   type Key_Array is array (Slot) of Endpoints;
   type Flag_Array is array (Slot) of Boolean;
   type Gen_Array is array (Slot) of Unsigned_32;
   type Home_Array is array (Slot) of Bucket_Id;
   type Place_Array is array (Slot) of Maybe_Position;
   type Spot_Array is array (Slot) of Connection_Count;   --  0: open, not on the stack
   type Stack_Array is array (Slot) of Slot;

   type Table is record
      Secret  : SipHash.Key;
      Keys    : Key_Array;
      Used    : Flag_Array;
      Gen     : Gen_Array;
      Home    : Home_Array;    --  an open slot's bucket
      Place   : Place_Array;   --  and its position there
      Buckets : Bucket_Array;
      Stack   : Stack_Array;   --  free slots: Stack (1 .. Top)
      Spot    : Spot_Array;    --  a free slot's place in Stack
      Top     : Connection_Count;
      N       : Connection_Count;
   end record;

   function Bucket_Of (Secret : SipHash.Key; E : Endpoints) return Bucket_Id is
     (Bucket_Id (SipHash.Hash (Secret, TCP_Isn.Serialize (E)) mod Unsigned_64 (Bucket_Count)));

   --  Free slots among 1 .. K.
   function Free_Slots (U : Flag_Array; K : Connection_Count) return Connection_Count is
     (if K = 0 then 0 else Free_Slots (U, K - 1) + (if U (K) then 0 else 1))
   with Ghost, Pre => K <= Max_Connections, Post => Free_Slots'Result <= K,
        Subprogram_Variant => (Decreases => K);

   function Valid (T : Table) return Boolean is
     (--  Free stack and free slots correspond.
      (for all K in 1 .. T.Top => not T.Used (T.Stack (K)) and then T.Spot (T.Stack (K)) = K) and then
      (for all S in Slot =>
         (if T.Used (S) then T.Spot (S) = 0
          else T.Spot (S) in 1 .. T.Top and then T.Stack (T.Spot (S)) = S)) and then
      T.N + T.Top = Max_Connections and then
      --  Open slots are in their bucket, where they hash.
      (for all S in Slot =>
         (if T.Used (S) then
            T.Home (S) = Bucket_Of (T.Secret, T.Keys (S)) and then
            T.Place (S) in Position and then
            T.Buckets (T.Home (S)) (T.Place (S)) = S)) and then
      --  Bucket entries are open slots that know where they are.
      (for all B in Bucket_Id =>
         (for all P in Position =>
            (if T.Buckets (B) (P) /= No_Slot then
               T.Buckets (B) (P) in Slot and then T.Used (T.Buckets (B) (P)) and then
               T.Home (T.Buckets (B) (P)) = B and then T.Place (T.Buckets (B) (P)) = P))) and then
      T.Top = Free_Slots (T.Used, Max_Connections));

   function Open (T : Table; S : Slot) return Boolean is (T.Used (S));
   function Key (T : Table; S : Slot) return Endpoints is (T.Keys (S));
   function Generation (T : Table; S : Slot) return Unsigned_32 is (T.Gen (S));
   function Count (T : Table) return Connection_Count is (T.N);
end Connection_Table;
