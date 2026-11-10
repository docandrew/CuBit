pragma Ada_2022;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Heap_Classes;
with Heap_Extents;
with Heap_Slab_Instance;

package body CuAlloc with
  SPARK_Mode,
  Refined_State => (Heap_State => (Ready, Directory, Capacity, Count, Map, Map_Leaves, Registry, Heaps,
                                   Abandoned, Fallback_Heap, Lock_Word, Fallback_Word, Lock_Held,
                                   Fallback_Held, Thread_Slot.Heap))
is
   package S renames Heap_Slab_Instance;
   package E renames Heap_Extents;
   package HC renames Heap_Classes;
   use type S.Release_Status;

   SMALL_LIMIT : constant := 4_096;
   --  Larger, or aligned beyond a page: a reservation of its own.
   HUGE_THRESHOLD : constant := 1_048_576;
   --  Small and medium arenas hold this much payload; the proved cores
   --  address exactly this much.
   ARENA_PAYLOAD : constant := 16 * 1_048_576;
   pragma Compile_Time_Error (S.Arena_Bytes /= ARENA_PAYLOAD, "slab arena geometry");
   pragma Compile_Time_Error (E.Arena_Bytes /= ARENA_PAYLOAD, "extent arena geometry");
   pragma Compile_Time_Error (E.Page_Bytes /= Page_Bytes, "extent page geometry");
   --  Each small or medium arena is one reservation of this span, aligned
   --  to it: the arena holding an address is the address >> ARENA_SHIFT.
   ARENA_SHIFT : constant := 25;
   ARENA_SPAN : constant := 2 ** ARENA_SHIFT;
   ARENA_PAGES : constant := ARENA_SPAN / Page_Bytes;
   --  The cores' metadata, at the arena's base (checked against the cores'
   --  sizes when the heap starts); the payload follows.
   --  Page 0 is the arena's header: its identity word (the map entry that
   --  names it), for sized frees. The core state follows it.
   ARENA_HEADER : constant := Page_Bytes;
   SLAB_META : constant := 270_336;
   EXTENT_META : constant := 36_864;
   pragma Compile_Time_Error (SLAB_META mod Page_Bytes /= 0 or else EXTENT_META mod Page_Bytes /= 0,
                              "metadata is pages");
   pragma Compile_Time_Error (SLAB_META + ARENA_PAYLOAD > ARENA_SPAN, "slab arena fits its span");
   --  Backing is added in steps of at least this much (fewer provider calls).
   COMMIT_STEP : constant := 1_048_576;
   --  What a plain request (and realloc) is aligned to.
   STANDARD_ALIGNMENT : constant := 16;

   ------------------------------------------------------------------------
   --  Shared regions: the huge directory, the radix map, the registry.
   ------------------------------------------------------------------------
   --  The largest reservation this layer asks for: the largest request
   --  rounded to pages, and alignment slack.
   MAXIMUM_TOTAL : constant := Maximum_Request + Maximum_Alignment;

   --  A reservation and its backed prefix.
   type Region is record
      Base, Total, Committed : Unsigned_64 := 0;
   end record;
   function Region_Valid (R : Region) return Boolean is
     (R.Total > 0 and then R.Base > 0 and then R.Base <= Unsigned_64'Last - R.Total
      and then R.Base mod Page_Bytes = 0 and then R.Total mod Page_Bytes = 0
      and then R.Committed <= R.Total and then R.Committed mod Page_Bytes = 0
      and then R.Total <= MAXIMUM_TOTAL)
     with Ghost;

   --  The radix map covers the low 2^47 bytes: a top level of leaf
   --  addresses (0: none) for each 2^36 bytes, leaves of 32-bit entries for
   --  each 32 MiB slot (0: no arena). Both live in one reservation, the top
   --  level first, leaves appended as they are needed.
   ADDRESS_LIMIT : constant := 2 ** 47;
   MAP_SLOTS : constant := ADDRESS_LIMIT / ARENA_SPAN;
   LEAF_BITS : constant := 11;
   LEAF_ENTRIES : constant := 2 ** LEAF_BITS;
   LEAF_BYTES : constant := LEAF_ENTRIES * 4;
   MAP_TOP : constant := MAP_SLOTS / LEAF_ENTRIES;
   MAP_TOP_BYTES : constant := MAP_TOP * 8;
   MAP_BYTES : constant := MAP_TOP_BYTES + MAP_TOP * LEAF_BYTES;
   subtype Leaf_Number is Natural range 0 .. MAP_TOP;
   subtype Map_Slot is Unsigned_64 range 0 .. MAP_SLOTS - 1;

   --  Heaps: numbered from 1, each a reservation of its own; the registry
   --  holds each one's base at Registry.Base + 8 * Id.
   MAXIMUM_HEAPS : constant := 65_535;
   REGISTRY_BYTES : constant := (MAXIMUM_HEAPS + 1) * 8;
   subtype Heap_Number is Natural range 0 .. MAXIMUM_HEAPS;
   subtype Heap_Id is Positive range 1 .. MAXIMUM_HEAPS;

   --  Map entry: heap (low 16 bits), the arena's place in its table (13
   --  bits from ENTRY_SHIFT) and, in ENTRY_MEDIUM, its kind (set: medium),
   --  so a free knows a small block's arena from the entry alone.
   ENTRY_SHIFT : constant := 16;
   ENTRY_MEDIUM : constant := 2 ** 29;

   ------------------------------------------------------------------------
   --  Huge blocks: the sorted directory.
   ------------------------------------------------------------------------
   MAXIMUM_BLOCKS : constant := 65_536;
   type Block_Record is record
      Base : Unsigned_64 := 0;         --  the reservation
      Total : Unsigned_64 := 0;        --  its bytes
      Payload : Unsigned_64 := 0;      --  the block
      Committed : Unsigned_64 := 0;    --  bytes backed, from Base
      Bytes : Unsigned_64 := 0;        --  the block's usable bytes (0: freed, release refused)
   end record;
   RECORD_BYTES : constant := 40;
   for Block_Record use record
      Base at 0 range 0 .. 63;
      Total at 8 range 0 .. 63;
      Payload at 16 range 0 .. 63;
      Committed at 24 range 0 .. 63;
      Bytes at 32 range 0 .. 63;
   end record;
   for Block_Record'Size use RECORD_BYTES * 8;
   DIRECTORY_TOTAL : constant := MAXIMUM_BLOCKS * RECORD_BYTES;
   pragma Compile_Time_Error (DIRECTORY_TOTAL mod Page_Bytes /= 0, "directory is pages");

   subtype Block_Number is Natural range 0 .. MAXIMUM_BLOCKS;
   subtype Block_Position is Positive range 1 .. MAXIMUM_BLOCKS;
   subtype Search_Position is Positive range 1 .. MAXIMUM_BLOCKS + 1;
   No_Block : constant Block_Number := 0;
   type Block_Table is array (Block_Position range <>) of Block_Record;

   function Well_Formed (R : Block_Record) return Boolean is
     (R.Total > 0 and then R.Total <= MAXIMUM_TOTAL and then R.Base > 0
      and then R.Base <= Unsigned_64'Last - R.Total
      and then R.Base mod Page_Bytes = 0 and then R.Total mod Page_Bytes = 0
      and then R.Committed <= R.Total and then R.Committed mod Page_Bytes = 0
      and then R.Payload >= R.Base and then R.Payload - R.Base <= R.Total
      and then R.Bytes <= R.Total - (R.Payload - R.Base))
     with Ghost;

   function Ends (R : Block_Record) return Unsigned_64 is (R.Base + R.Total)
     with Pre => R.Base <= Unsigned_64'Last - R.Total;

   function Contains (R : Block_Record; Address : Unsigned_64) return Boolean is
     (Address >= R.Base and then Address - R.Base < R.Total);

   function Directory_Valid (Table : Block_Table; Last : Block_Number) return Boolean is
     (Table'First = 1 and then Last <= Table'Last
      and then (for all P in 1 .. Last => Well_Formed (Table (P)))
      and then (for all P in 1 .. Last =>
                  (for all Q in P + 1 .. Last => Ends (Table (P)) <= Table (Q).Base)))
     with Ghost;

   ------------------------------------------------------------------------
   --  A heap: its shared words, its private part, its arena table.
   ------------------------------------------------------------------------
   --  Shared words, each on its own cache line: the heap's state (changed
   --  only under the directory lock) and its inbox (atomics only).
   STATE_OFFSET : constant := 0;
   INBOX_OFFSET : constant := 64;
   HEAP_UNUSED : constant := 0;
   HEAP_HELD : constant := 1;
   HEAP_ABANDONED : constant := 2;
   --  The private part, then the arena table, backed as arenas are added.
   PRIVATE_OFFSET : constant := Page_Bytes;
   TABLE_OFFSET : constant := PRIVATE_OFFSET + 4 * Page_Bytes;
   MAXIMUM_HEAP_ARENAS : constant := 4_096;
   ARENA_RECORD_BYTES : constant := 8;
   HEAP_BYTES : constant := TABLE_OFFSET + MAXIMUM_HEAP_ARENAS * ARENA_RECORD_BYTES;
   --  Backed when a heap is made: its private part and 512 arena records.
   HEAP_INITIAL : constant := TABLE_OFFSET + Page_Bytes;
   RECORDS_PER_PAGE : constant := Page_Bytes / ARENA_RECORD_BYTES;
   pragma Compile_Time_Error (2 ** (ENTRY_SHIFT + 13) > ENTRY_MEDIUM or else MAXIMUM_HEAP_ARENAS >= 2 ** 13,
                              "map entries hold a heap and an arena number");
   --  The fallback heap's holder token (FS bases are aligned, never 1).
   FALLBACK_TOKEN : constant := 1;

   subtype Arena_Number is Natural range 0 .. MAXIMUM_HEAP_ARENAS;
   subtype Arena_Index is Positive range 1 .. MAXIMUM_HEAP_ARENAS;
   type Arena_Kind is (Unused, Small, Medium) with Size => 8;
   --  A small or medium arena: its slot (base / ARENA_SPAN), the pages
   --  backed from its base, its kind.
   type Arena_Record is record
      Slot : Unsigned_32 := 0;
      Pages : Unsigned_16 := 0;
      Kind : Arena_Kind := Unused;
   end record;
   for Arena_Record use record
      Slot at 0 range 0 .. 31;
      Pages at 4 range 0 .. 15;
      Kind at 6 range 0 .. 7;
   end record;
   for Arena_Record'Size use ARENA_RECORD_BYTES * 8;
   type Arena_Table is array (Arena_Index) of Arena_Record;

   --  A set of a heap's arenas (by table number): a word per 64, and a
   --  summary bit per non-empty word.
   WORDS_PER_SET : constant := MAXIMUM_HEAP_ARENAS / 64;
   subtype Word_Number is Natural range 0 .. WORDS_PER_SET - 1;
   type Set_Words is array (Word_Number) of Unsigned_64;
   type Arena_Set is record
      Summary : Unsigned_64;
      Words : Set_Words;
   end record;

   --  Medium requests by pages, and their buckets: bucket B holds arenas
   --  that may still have a free run of 2 ** B pages.
   MEDIUM_PAGE_LIMIT : constant := HUGE_THRESHOLD / Page_Bytes;
   subtype Medium_Pages is Positive range 1 .. MEDIUM_PAGE_LIMIT;
   subtype Medium_Bucket is Natural range 0 .. 8;
   pragma Compile_Time_Error (2 ** Medium_Bucket'Last /= MEDIUM_PAGE_LIMIT, "buckets cover medium");

   --  Batches of remote frees: a node is a small block of NODE_BYTES,
   --  [next node][count | capacity << 32][entries ...].
   NODE_BYTES : constant := 512;
   NODE_CAPACITY : constant := (NODE_BYTES - 16) / 8;
   subtype Node_Count is Natural range 0 .. NODE_CAPACITY;
   subtype Node_Index is Positive range 1 .. NODE_CAPACITY;
   OUTGOING_SLOTS : constant := 8;
   subtype Outgoing_Slot is Natural range 0 .. OUTGOING_SLOTS - 1;
   type Batch is record
      Destination : Heap_Number := 0;   --  0: an empty slot
      Node : Unsigned_64 := 0;
      Count : Node_Count := 0;
   end record;
   type Batches is array (Outgoing_Slot) of Batch;

   --  Where each small class allocates next: an arena (its base; 0: none)
   --  and one of its slabs (the slow path finds the arena's table number
   --  from its identity word). A cached slab is
   --  backed through its end (the slow path backs it before caching it, and
   --  a slab that was full held blocks to its end; nothing is decommitted),
   --  so the fast path takes a slot from it with no commit check.
   type Slab_Cache is record
      Arena : Unsigned_64 := 0;
      Page : S.Slab_Id := S.Slab_Id'First;
   end record;
   type Class_Caches is array (HC.Size_Class) of Slab_Cache;
   type Class_Sets is array (HC.Size_Class) of Arena_Set;
   type Bucket_Sets is array (Medium_Bucket) of Arena_Set;
   --  The fast paths read Holder, Id, Arenas and one Cache entry: first.
   type Heap_Private is record
      Holder : Unsigned_64;          --  the holding thread's token (0: none)
      Id : Heap_Number;
      Arenas : Arena_Number;         --  table records in use: 1 .. Arenas
      Cache : Class_Caches;          --  where each small class allocates next
      Capacity : Arena_Number;       --  table records backed
      Committed : Unsigned_64;       --  bytes of the heap's reservation backed
      Current_Medium : Arena_Number;
      Small_Sets : Class_Sets;       --  arenas that may serve each class
      Medium_Sets : Bucket_Sets;     --  arenas that may serve each bucket
      Outgoing : Batches;
   end record;

   function Batches_Valid (B : Batches) return Boolean is
     (for all O in Outgoing_Slot =>
        B (O).Count < NODE_CAPACITY
        and then (if B (O).Destination = 0 then B (O).Count = 0 else B (O).Node /= 0))
     with Ghost;

   function Cache_Valid (C : Slab_Cache) return Boolean is
     (C.Arena mod ARENA_SPAN = 0 and then C.Arena < ADDRESS_LIMIT)
     with Ghost;

   function Private_Valid (P : Heap_Private) return Boolean is
     (P.Id /= 0 and then P.Arenas <= P.Capacity
      and then P.Committed <= HEAP_BYTES and then P.Committed mod Page_Bytes = 0
      and then TABLE_OFFSET + Unsigned_64 (P.Capacity) * ARENA_RECORD_BYTES <= P.Committed
      and then (for all K in HC.Size_Class => Cache_Valid (P.Cache (K)))
      and then P.Current_Medium <= P.Arenas
      and then Batches_Valid (P.Outgoing))
     with Ghost;

   function Heap_Base_Valid (Base : Unsigned_64) return Boolean is
     (Base > 0 and then Base mod Page_Bytes = 0 and then Base <= Unsigned_64'Last - HEAP_BYTES)
     with Ghost;

   ------------------------------------------------------------------------
   --  State
   ------------------------------------------------------------------------
   Ready : Boolean := False;
   --  The huge directory's own reservation, records backed, records used.
   Directory : Region := (others => 0);
   Capacity : Block_Number := 0;
   Count : Block_Number := 0;
   Map : Region := (others => 0);
   Map_Leaves : Leaf_Number := 0;
   Registry : Region := (others => 0);
   Heaps : Heap_Number := 0;
   --  Heaps left by their threads, waiting for adoption.
   Abandoned : Heap_Number := 0;
   Fallback_Heap : Unsigned_64 := 0;
   --  0: free, 1: taken.
   Lock_Word : aliased Unsigned_8 := 0;
   Fallback_Word : aliased Unsigned_8 := 0;
   --  The locking discipline (ghost): this caller holds the lock.
   Lock_Held : Boolean := False with Ghost;
   Fallback_Held : Boolean := False with Ghost;
   --  The calling thread's heap (0: none): a word in the static TLS block,
   --  reached only through Get_Thread_Heap and Set_Thread_Heap.
   package Thread_Slot with SPARK_Mode => Off is
      Heap : Unsigned_64 := 0 with Thread_Local_Storage;
   end Thread_Slot;

   function State_Valid return Boolean is
     (Capacity <= MAXIMUM_BLOCKS and then Count <= Capacity and then Abandoned <= Heaps
      and then (if Ready then
                  Region_Valid (Directory) and then Directory.Total = DIRECTORY_TOTAL
                  and then Unsigned_64 (Capacity) * RECORD_BYTES <= Directory.Committed
                  and then Region_Valid (Map) and then Map.Total = MAP_BYTES
                  and then MAP_TOP_BYTES + Unsigned_64 (Map_Leaves) * LEAF_BYTES <= Map.Committed
                  and then Region_Valid (Registry) and then Registry.Total = REGISTRY_BYTES
                  and then (Unsigned_64 (Heaps) + 1) * 8 <= Registry.Committed
                  and then (if Fallback_Heap /= 0 then Heap_Base_Valid (Fallback_Heap))
                else Count = 0 and then Capacity = 0 and then Map_Leaves = 0 and then Heaps = 0
                  and then Fallback_Heap = 0))
     with Ghost;

   --  The calling thread may change the heap P: it is P's recorded holder
   --  (its token, checked at each entry), and for the fallback heap it
   --  holds the fallback lock.
   function Holds (P : Heap_Private; Me : Unsigned_64) return Boolean is
     (Me /= 0 and then P.Holder = Me and then (if Me = FALLBACK_TOKEN then Fallback_Held))
     with Ghost;

   ------------------------------------------------------------------------
   --  The provider and the memory boundary. These bodies are outside SPARK:
   --  they call the generic's actuals, convert addresses, touch payload or
   --  node memory, or are atomic steps.
   ------------------------------------------------------------------------
   procedure Provider_Reserve (Bytes, Alignment : Unsigned_64; Base : out Unsigned_64) with
     Global => null, Pre => Bytes > 0 and then Bytes mod Page_Bytes = 0;
   --  The exact-prefix contract: Offset is what is backed already.
   procedure Provider_Commit (Base, Offset, Bytes : Unsigned_64; Success : out Boolean) with
     Global => null,
     Pre => Bytes > 0 and then Bytes <= Maximum_Commit and then Bytes mod Page_Bytes = 0
       and then Offset mod Page_Bytes = 0;
   procedure Provider_Release (Base, Bytes : Unsigned_64; Success : out Boolean) with
     Global => null;
   function To_Address (Value : Unsigned_64) return System.Address with
     Global => null, Post => To_Integer (To_Address'Result) = Integer_Address (Value);
   --  Write Bytes zero bytes at Item; copy Bytes bytes from Source to Target.
   procedure Zero (Item, Bytes : Unsigned_64) with Global => null;
   procedure Copy (Target, Source, Bytes : Unsigned_64) with Global => null;
   --  Shared words (the map, the registry, heaps' inboxes): acquire loads,
   --  release stores; Take_All exchanges the word with 0 (acquire and
   --  release); Push_Node links Node in front of the list at Inbox (writes
   --  Node's next word, then a release compare-and-swap, retried).
   procedure Load_64 (Address : Unsigned_64; Value : out Unsigned_64) with Global => null, Inline;
   procedure Load_32 (Address : Unsigned_64; Value : out Unsigned_32) with Global => null, Inline;
   procedure Store_64 (Address, Value : Unsigned_64) with Global => null;
   procedure Store_32 (Address : Unsigned_64; Value : Unsigned_32) with Global => null;
   procedure Take_All (Address : Unsigned_64; Value : out Unsigned_64) with Global => null;
   procedure Push_Node (Inbox, Node : Unsigned_64) with Global => null;
   --  Node words (a node is a block this protocol owns while it travels).
   procedure Node_Read (Node : Unsigned_64; Next : out Unsigned_64; Used, Room : out Node_Count) with
     Global => null;
   procedure Node_Entry (Node : Unsigned_64; K : Node_Index; Item : out Unsigned_64) with Global => null;
   procedure Node_Put (Node : Unsigned_64; K : Node_Index; Item : Unsigned_64) with Global => null;
   procedure Node_Seal (Node : Unsigned_64; Used, Room : Node_Count) with Global => null;
   --  The calling thread: its token (its FS base: unique among live
   --  threads; 0 when it has no thread pointer), and its heap slot (a
   --  thread-local word; only valid with a token).
   procedure Get_Token (Me : out Unsigned_64) with Global => null, Inline;
   procedure Get_Thread_Heap (Base : out Unsigned_64) with Global => null, Inline;
   procedure Set_Thread_Heap (Base : Unsigned_64) with Global => null;
   --  The lock words' atomic steps. Try_*: Taken when it was free and is now ours.
   procedure Try_Take (Taken : out Boolean) with Inline, Global => (In_Out => Lock_Word);
   procedure Give_Back with Inline, Global => (In_Out => Lock_Word);
   procedure Try_Take_Fallback (Taken : out Boolean) with Inline, Global => (In_Out => Fallback_Word);
   procedure Give_Back_Fallback with Inline, Global => (In_Out => Fallback_Word);
   procedure Pause with
     Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pause", Global => null,
     Always_Terminates;

   procedure Provider_Reserve (Bytes, Alignment : Unsigned_64; Base : out Unsigned_64) with
     SPARK_Mode => Off
   is
   begin
      Base := Reserve (Bytes, Alignment);
   end Provider_Reserve;

   procedure Provider_Commit (Base, Offset, Bytes : Unsigned_64; Success : out Boolean) with
     SPARK_Mode => Off
   is
   begin
      Success := Commit (Base, Offset, Bytes);
   end Provider_Commit;

   procedure Provider_Release (Base, Bytes : Unsigned_64; Success : out Boolean) with
     SPARK_Mode => Off
   is
   begin
      Success := Release (Base, Bytes);
   end Provider_Release;

   function To_Address (Value : Unsigned_64) return System.Address with SPARK_Mode => Off is
   begin
      return System.Storage_Elements.To_Address (Integer_Address (Value));
   end To_Address;

   procedure Zero (Item, Bytes : Unsigned_64) with SPARK_Mode => Off is
      Target : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Item);
   begin
      Target := [others => 0];
   end Zero;

   procedure Copy (Target, Source, Bytes : Unsigned_64) with SPARK_Mode => Off is
      From : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Source);
      To : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Target);
   begin
      To := From;
   end Copy;

   --  GCC's atomics; the memory-order numbers are GCC's.
   RELAXED : constant := 0;
   ACQUIRE : constant := 2;
   RELEASE_ORDER : constant := 3;
   ACQUIRE_RELEASE : constant := 4;
   function Atomic_Load_64 (Ptr : System.Address; Model : Integer) return Unsigned_64 with
     Import, Convention => Intrinsic, External_Name => "__atomic_load_8", SPARK_Mode => Off;
   function Atomic_Load_32 (Ptr : System.Address; Model : Integer) return Unsigned_32 with
     Import, Convention => Intrinsic, External_Name => "__atomic_load_4", SPARK_Mode => Off;
   procedure Atomic_Store_64 (Ptr : System.Address; Value : Unsigned_64; Model : Integer) with
     Import, Convention => Intrinsic, External_Name => "__atomic_store_8", SPARK_Mode => Off;
   procedure Atomic_Store_32 (Ptr : System.Address; Value : Unsigned_32; Model : Integer) with
     Import, Convention => Intrinsic, External_Name => "__atomic_store_4", SPARK_Mode => Off;
   function Atomic_Exchange_64 (Ptr : System.Address; Value : Unsigned_64; Model : Integer)
     return Unsigned_64 with
     Import, Convention => Intrinsic, External_Name => "__atomic_exchange_8", SPARK_Mode => Off;
   function Atomic_Swap_64 (Ptr : System.Address; Expected, Desired : Unsigned_64) return Boolean with
     Import, Convention => Intrinsic, External_Name => "__sync_bool_compare_and_swap_8",
     SPARK_Mode => Off;
   function Exchange_8 (Ptr : System.Address; Value : Unsigned_8; Model : Integer) return Unsigned_8 with
     Import, Convention => Intrinsic, External_Name => "__atomic_exchange_1", SPARK_Mode => Off;
   procedure Store_8 (Ptr : System.Address; Value : Unsigned_8; Model : Integer) with
     Import, Convention => Intrinsic, External_Name => "__atomic_store_1", SPARK_Mode => Off;

   procedure Load_64 (Address : Unsigned_64; Value : out Unsigned_64) with SPARK_Mode => Off is
   begin
      Value := Atomic_Load_64 (To_Address (Address), ACQUIRE);
   end Load_64;

   procedure Load_32 (Address : Unsigned_64; Value : out Unsigned_32) with SPARK_Mode => Off is
   begin
      Value := Atomic_Load_32 (To_Address (Address), ACQUIRE);
   end Load_32;

   procedure Store_64 (Address, Value : Unsigned_64) with SPARK_Mode => Off is
   begin
      Atomic_Store_64 (To_Address (Address), Value, RELEASE_ORDER);
   end Store_64;

   procedure Store_32 (Address : Unsigned_64; Value : Unsigned_32) with SPARK_Mode => Off is
   begin
      Atomic_Store_32 (To_Address (Address), Value, RELEASE_ORDER);
   end Store_32;

   procedure Take_All (Address : Unsigned_64; Value : out Unsigned_64) with SPARK_Mode => Off is
   begin
      Value := Atomic_Exchange_64 (To_Address (Address), 0, ACQUIRE_RELEASE);
   end Take_All;

   procedure Push_Node (Inbox, Node : Unsigned_64) with SPARK_Mode => Off is
      Next : Unsigned_64 with Import, Volatile, Address => To_Address (Node);
      Head : Unsigned_64;
   begin
      loop
         Head := Atomic_Load_64 (To_Address (Inbox), RELAXED);
         Next := Head;
         --  A full barrier (lock cmpxchg): Node's words before the swap.
         exit when Atomic_Swap_64 (To_Address (Inbox), Head, Node);
         Pause;
      end loop;
   end Push_Node;

   type Node_Words is array (0 .. NODE_CAPACITY + 1) of Unsigned_64;

   procedure Node_Read (Node : Unsigned_64; Next : out Unsigned_64; Used, Room : out Node_Count) with
     SPARK_Mode => Off
   is
      Words : Node_Words with Import, Address => To_Address (Node);
      Header : constant Unsigned_64 := Words (1);
      Stated_Room : constant Unsigned_64 := Shift_Right (Header, 32);
      Stated_Used : constant Unsigned_64 := Header and 16#FFFF_FFFF#;
   begin
      Next := Words (0);
      Room := (if Stated_Room > NODE_CAPACITY then 0 else Node_Count (Stated_Room));
      Used := (if Stated_Used > Unsigned_64 (Room) then Room else Node_Count (Stated_Used));
   end Node_Read;

   procedure Node_Entry (Node : Unsigned_64; K : Node_Index; Item : out Unsigned_64) with
     SPARK_Mode => Off
   is
      Words : Node_Words with Import, Address => To_Address (Node);
   begin
      Item := Words (K + 1);
   end Node_Entry;

   procedure Node_Put (Node : Unsigned_64; K : Node_Index; Item : Unsigned_64) with SPARK_Mode => Off is
      Words : Node_Words with Import, Address => To_Address (Node);
   begin
      Words (K + 1) := Item;
   end Node_Put;

   procedure Node_Seal (Node : Unsigned_64; Used, Room : Node_Count) with SPARK_Mode => Off is
      Words : Node_Words with Import, Address => To_Address (Node);
   begin
      Words (1) := Unsigned_64 (Used) or Shift_Left (Unsigned_64 (Room), 32);
   end Node_Seal;

   procedure Get_Token (Me : out Unsigned_64) with SPARK_Mode => Off is
   begin
      System.Machine_Code.Asm
        ("rdfsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Me), Volatile => True);
   end Get_Token;

   procedure Get_Thread_Heap (Base : out Unsigned_64) with SPARK_Mode => Off is
   begin
      Base := Thread_Slot.Heap;
   end Get_Thread_Heap;

   procedure Set_Thread_Heap (Base : Unsigned_64) with SPARK_Mode => Off is
   begin
      Thread_Slot.Heap := Base;
   end Set_Thread_Heap;

   procedure Try_Take (Taken : out Boolean) with SPARK_Mode => Off is
   begin
      Taken := Exchange_8 (Lock_Word'Address, 1, ACQUIRE) = 0;
   end Try_Take;

   procedure Give_Back with SPARK_Mode => Off is
   begin
      Store_8 (Lock_Word'Address, 0, RELEASE_ORDER);
   end Give_Back;

   procedure Try_Take_Fallback (Taken : out Boolean) with SPARK_Mode => Off is
   begin
      Taken := Exchange_8 (Fallback_Word'Address, 1, ACQUIRE) = 0;
   end Try_Take_Fallback;

   procedure Give_Back_Fallback with SPARK_Mode => Off is
   begin
      Store_8 (Fallback_Word'Address, 0, RELEASE_ORDER);
   end Give_Back_Fallback;

   ------------------------------------------------------------------------
   --  The locks
   ------------------------------------------------------------------------
   procedure Lock with
     Global => (In_Out => (Lock_Word, Lock_Held)), Pre => not Lock_Held, Post => Lock_Held
   is
      Taken : Boolean;
   begin
      loop
         Try_Take (Taken);
         exit when Taken;
         Pause;
      end loop;
      Lock_Held := True;
   end Lock;

   procedure Unlock with
     Global => (In_Out => (Lock_Word, Lock_Held)), Pre => Lock_Held, Post => not Lock_Held
   is
   begin
      Lock_Held := False;
      Give_Back;
   end Unlock;

   procedure Lock_Fallback with
     Global => (In_Out => (Fallback_Word, Fallback_Held)), Pre => not Fallback_Held,
     Post => Fallback_Held
   is
      Taken : Boolean;
   begin
      loop
         Try_Take_Fallback (Taken);
         exit when Taken;
         Pause;
      end loop;
      Fallback_Held := True;
   end Lock_Fallback;

   procedure Unlock_Fallback with
     Global => (In_Out => (Fallback_Word, Fallback_Held)), Pre => Fallback_Held,
     Post => not Fallback_Held
   is
   begin
      Fallback_Held := False;
      Give_Back_Fallback;
   end Unlock_Fallback;

   ------------------------------------------------------------------------
   --  Arithmetic and backing
   ------------------------------------------------------------------------
   function Round_Up_Pages (Value : Unsigned_64) return Unsigned_64 is
     ((Value + (Page_Bytes - 1)) / Page_Bytes * Page_Bytes)
     with Pre => Value <= Unsigned_64'Last - (Page_Bytes - 1),
     Post => Round_Up_Pages'Result >= Value and then Round_Up_Pages'Result - Value < Page_Bytes
       and then Round_Up_Pages'Result mod Page_Bytes = 0;

   function Round_Up_Steps (Value : Unsigned_64) return Unsigned_64 is
     ((Value + (COMMIT_STEP - 1)) / COMMIT_STEP * COMMIT_STEP)
     with Pre => Value <= Unsigned_64'Last - (COMMIT_STEP - 1),
     Post => Round_Up_Steps'Result >= Value and then Round_Up_Steps'Result - Value < COMMIT_STEP;

   --  The address of a page-aligned overlay. The mask changes nothing (Value
   --  is a page multiple: proved at each use) but tells the compiler the
   --  alignment, so it emits no run-time alignment check for the overlay.
   function Page_Address (Value : Unsigned_64) return System.Address is
     (To_Address (Value and not (Page_Bytes - 1)))
     with Pre => Value mod Page_Bytes = 0,
     Post => To_Integer (Page_Address'Result) = Integer_Address (Value);

   --  Back the reservation at Base (Total bytes, Committed backed) up to
   --  Upto bytes, in steps of at least COMMIT_STEP, never past its end and
   --  never more than Maximum_Commit at once. Each step asks for exactly
   --  the next prefix.
   procedure Ensure (Base, Total : Unsigned_64; Committed : in out Unsigned_64; Upto : Unsigned_64;
                     Success : out Boolean) with
     Global => null,
     Pre => Total mod Page_Bytes = 0 and then Total <= MAXIMUM_TOTAL and then Committed <= Total
       and then Committed mod Page_Bytes = 0,
     Post => Committed <= Total and then Committed mod Page_Bytes = 0
       and then Committed >= Committed'Old and then (if Success then Committed >= Upto)
   is
      Wanted, Step : Unsigned_64;
      Done : Boolean;
   begin
      Success := True;
      while Committed < Upto loop
         pragma Loop_Invariant (Committed <= Total and then Committed mod Page_Bytes = 0
                                and then Committed >= Committed'Loop_Entry and then Success);
         pragma Loop_Variant (Increases => Committed);
         Wanted := Upto - Committed;
         Step := Total - Committed;
         if Wanted < Step then
            Step := Unsigned_64'Min (Step, Round_Up_Steps (Wanted));
         end if;
         Step := Unsigned_64'Min (Step, Maximum_Commit);
         --  A Maximum_Commit that is not a page multiple ends growth here.
         if Step = 0 or else Step mod Page_Bytes /= 0 then
            Success := False;
            return;
         end if;
         Provider_Commit (Base, Committed, Step, Done);
         if not Done then
            Success := False;
            return;
         end if;
         Committed := Committed + Step;
      end loop;
   end Ensure;

   ------------------------------------------------------------------------
   --  Huge blocks: binary search over the sorted directory
   ------------------------------------------------------------------------
   --  The first position whose block starts above Address.
   function Upper_Bound (Table : Block_Table; Last : Block_Number; Address : Unsigned_64)
     return Search_Position
   with
     Pre => Directory_Valid (Table, Last),
     Post => Upper_Bound'Result <= Last + 1
       and then (for all Q in 1 .. Upper_Bound'Result - 1 => Table (Q).Base <= Address)
       and then (for all Q in Upper_Bound'Result .. Last => Table (Q).Base > Address)
   is
      Low : Search_Position := 1;
      High : Search_Position := Last + 1;
      Middle : Block_Position;
   begin
      while Low < High loop
         pragma Loop_Invariant (Low <= High and then High <= Last + 1);
         pragma Loop_Invariant (for all Q in 1 .. Low - 1 => Table (Q).Base <= Address);
         pragma Loop_Invariant (for all Q in High .. Last => Table (Q).Base > Address);
         pragma Loop_Variant (Decreases => High - Low);
         Middle := Low + (High - Low) / 2;
         if Table (Middle).Base <= Address then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      return Low;
   end Upper_Bound;

   --  The position of the block whose reservation holds Address, or
   --  No_Block when none holds it.
   function Block_Of (Table : Block_Table; Address : Unsigned_64) return Block_Number with
     Pre => Directory_Valid (Table, Count),
     Post => (if Block_Of'Result = No_Block then
                (for all Q in 1 .. Count => not Contains (Table (Q), Address))
              else Block_Of'Result <= Count and then Contains (Table (Block_Of'Result), Address))
   is
      Position : constant Search_Position := Upper_Bound (Table, Count, Address);
   begin
      if Position = 1 or else not Contains (Table (Position - 1), Address) then
         return No_Block;
      end if;
      return Position - 1;
   end Block_Of;

   --  Insert R at its sorted place: Position, or No_Block (nothing changed)
   --  when there is no room or R would overlap a neighbour.
   procedure Insert (Table : in out Block_Table; R : Block_Record; Position : out Block_Number) with
     Pre => Directory_Valid (Table, Count) and then Well_Formed (R),
     Post => Directory_Valid (Table, Count)
       and then (if Position = No_Block then Count = Count'Old and then Table = Table'Old
                 else Count = Count'Old + 1 and then Position <= Count and then Table (Position) = R)
   is
      Place : Search_Position;
   begin
      if Count >= Table'Last then
         Position := No_Block;
         return;
      end if;
      Place := Upper_Bound (Table, Count, R.Base);
      if (Place > 1 and then Ends (Table (Place - 1)) > R.Base)
        or else (Place <= Count and then Ends (R) > Table (Place).Base)
      then
         Position := No_Block;
         return;
      end if;
      declare
         Old : constant Block_Table := Table with Ghost;
      begin
         Table (Place + 1 .. Count + 1) := Table (Place .. Count);
         Table (Place) := R;
         pragma Assert (Well_Formed (Table (Place)));
         pragma Assert (for all Q in 1 .. Place - 1 => Table (Q) = Old (Q));
         pragma Assert (for all Q in Place + 1 .. Count + 1 => Table (Q) = Old (Q - 1));
         pragma Assert (for all Q in 1 .. Place - 1 => Well_Formed (Old (Q)));
         pragma Assert (for all Q in 1 .. Place - 1 => Well_Formed (Table (Q)));
         pragma Assert (for all Q in Place + 1 .. Count + 1 => Well_Formed (Old (Q - 1)));
         pragma Assert (for all Q in Place + 1 .. Count + 1 => Well_Formed (Table (Q)));
         pragma Assert (for all Q in 1 .. Count + 1 => Well_Formed (Table (Q)));
         pragma Assert (for all Q in Place + 1 .. Count + 1 => Ends (R) <= Table (Q).Base);
         pragma Assert (for all P in 1 .. Place - 1 => Ends (Table (P)) <= R.Base);
      end;
      Count := Count + 1;
      Position := Place;
   end Insert;

   --  Remove the record at Position; records after it move down.
   procedure Remove (Table : in out Block_Table; Position : Block_Position) with
     Pre => Directory_Valid (Table, Count) and then Position <= Count,
     Post => Directory_Valid (Table, Count) and then Count = Count'Old - 1
   is
      Old : constant Block_Table := Table with Ghost;
   begin
      Table (Position .. Count - 1) := Table (Position + 1 .. Count);
      pragma Assert (for all Q in 1 .. Position - 1 => Table (Q) = Old (Q));
      pragma Assert (for all Q in Position .. Count - 1 => Table (Q) = Old (Q + 1));
      Count := Count - 1;
   end Remove;

   ------------------------------------------------------------------------
   --  Starting
   ------------------------------------------------------------------------
   --  Reserve one shared region and back its first Initial bytes.
   procedure Make_Region (Total, Initial : Unsigned_64; R : out Region; Success : out Boolean) with
     Global => null,
     Pre => Total > 0 and then Total mod Page_Bytes = 0 and then Total <= MAXIMUM_TOTAL and then Initial <= Total,
     Post => (if Success then Region_Valid (R) and then R.Total = Total and then R.Committed >= Initial)
   is
      Base : Unsigned_64;
      Done : Boolean;
   begin
      R := (others => 0);
      Success := False;
      Provider_Reserve (Total, Page_Bytes, Base);
      if Base = 0 then
         return;
      elsif Base > Unsigned_64'Last - Total or else Base mod Page_Bytes /= 0 then
         Provider_Release (Base, Total, Done);
         return;
      end if;
      R := (Base => Base, Total => Total, Committed => 0);
      Ensure (R.Base, R.Total, R.Committed, Initial, Success);
      if not Success then
         Provider_Release (Base, Total, Done);
         R := (others => 0);
      end if;
   end Make_Region;

   procedure Start (Success : out Boolean) with
     Pre => State_Valid and then Lock_Held,
     Post => State_Valid and then Success = Ready
   is
      Done : Boolean;
      Held : Region;
   begin
      Success := Ready;
      if Ready then
         return;
      end if;
      if S.State'Size > (SLAB_META - ARENA_HEADER) * System.Storage_Unit
        or else E.State'Size > (EXTENT_META - ARENA_HEADER) * System.Storage_Unit
        or else Heap_Private'Size > (TABLE_OFFSET - PRIVATE_OFFSET) * System.Storage_Unit
      then
         return;
      end if;
      Make_Region (DIRECTORY_TOTAL, 0, Directory, Done);
      if not Done then
         return;
      end if;
      Make_Region (MAP_BYTES, MAP_TOP_BYTES, Map, Done);
      if not Done then
         Provider_Release (Directory.Base, Directory.Total, Done);
         Directory := (others => 0);
         return;
      end if;
      Make_Region (REGISTRY_BYTES, 8, Held, Done);
      if not Done then
         Provider_Release (Directory.Base, Directory.Total, Done);
         Provider_Release (Map.Base, Map.Total, Done);
         Directory := (others => 0);
         Map := (others => 0);
         return;
      end if;
      Registry := Held;
      Ready := True;
      Success := True;
   end Start;

   --  Back room for one more directory record when it is full; Success
   --  when there is room.
   procedure Make_Room (Success : out Boolean) with
     Pre => Ready and then State_Valid and then Lock_Held,
     Post => State_Valid and then (if Success then Count < Capacity)
   is
      Done : Boolean;
      Committed : Unsigned_64 := Directory.Committed;
   begin
      if Count < Capacity then
         Success := True;
         return;
      elsif Count = MAXIMUM_BLOCKS then
         Success := False;
         return;
      end if;
      Ensure (Directory.Base, Directory.Total, Committed, Unsigned_64 (Count + 1) * RECORD_BYTES, Done);
      pragma Assert (Committed >= Directory.Committed);
      pragma Assert (Unsigned_64 (Capacity) * RECORD_BYTES <= Committed);
      Directory.Committed := Committed;
      pragma Assert (Region_Valid (Directory));
      if Done then
         --  Room for exactly one more; the next Make_Room finds the backing
         --  already there (Ensure returns at once) until a step ends.
         Capacity := Count + 1;
         pragma Assert (Unsigned_64 (Capacity) * RECORD_BYTES <= Directory.Committed);
      end if;
      Success := Count < Capacity;
   end Make_Room;

   ------------------------------------------------------------------------
   --  The radix map and the registry
   ------------------------------------------------------------------------
   --  The map entry for Address's slot (0: no small or medium arena).
   --  Lock-free: entries change only under the lock, one word at a time.
   procedure Map_Lookup (Address : Unsigned_64; Value : out Unsigned_32) with
     Inline, Pre => State_Valid and then Ready, Post => (if Value /= 0 then Address < ADDRESS_LIMIT)
   is
      Leaf : Unsigned_64;
   begin
      Value := 0;
      if Address >= ADDRESS_LIMIT then
         return;
      end if;
      Load_64 (Map.Base + Shift_Right (Address, ARENA_SHIFT + LEAF_BITS) * 8, Leaf);
      if Leaf = 0 or else Leaf > Unsigned_64'Last - LEAF_BYTES then
         return;
      end if;
      Load_32 (Leaf + (Shift_Right (Address, ARENA_SHIFT) and (LEAF_ENTRIES - 1)) * 4, Value);
   end Map_Lookup;

   --  Name Value at Slot; a leaf is added (backed) when the slot has none.
   procedure Map_Set (Slot : Map_Slot; Value : Unsigned_32; Success : out Boolean) with
     Pre => State_Valid and then Ready and then Lock_Held,
     Post => State_Valid and then Ready
   is
      Top : constant Unsigned_64 := Map.Base + Shift_Right (Slot, LEAF_BITS) * 8;
      Leaf : Unsigned_64;
      Done : Boolean;
   begin
      Success := False;
      Load_64 (Top, Leaf);
      if Leaf = 0 then
         if Map_Leaves = MAP_TOP then
            return;
         end if;
         Ensure (Map.Base, Map.Total, Map.Committed,
                 MAP_TOP_BYTES + Unsigned_64 (Map_Leaves + 1) * LEAF_BYTES, Done);
         if not Done then
            return;
         end if;
         Leaf := Map.Base + MAP_TOP_BYTES + Unsigned_64 (Map_Leaves) * LEAF_BYTES;
         Map_Leaves := Map_Leaves + 1;
         --  Fresh backing reads zero: an empty leaf, published.
         Store_64 (Top, Leaf);
      elsif Leaf > Unsigned_64'Last - LEAF_BYTES then
         return;
      end if;
      Store_32 (Leaf + (Slot and (LEAF_ENTRIES - 1)) * 4, Value);
      Success := True;
   end Map_Set;

   --  The base of heap Id (0: no such heap). Lock-free.
   procedure Heap_Base_Of (Id : Heap_Number; Base : out Unsigned_64) with
     Pre => State_Valid and then Ready
   is
   begin
      Base := 0;
      if Id /= 0 and then Id <= Heaps then
         Load_64 (Registry.Base + Unsigned_64 (Id) * 8, Base);
      end if;
   end Heap_Base_Of;

   ------------------------------------------------------------------------
   --  Arena sets
   ------------------------------------------------------------------------
   --  The lowest set bit's number (Value /= 0), by halving.
   function Lowest_Bit (Value : Unsigned_64) return Natural with
     Post => Lowest_Bit'Result < 64
   is
      V : Unsigned_64 := Value;
      N : Natural := 0;
   begin
      if (V and 16#FFFF_FFFF#) = 0 then N := N + 32; V := Shift_Right (V, 32); end if;
      if (V and 16#FFFF#) = 0 then N := N + 16; V := Shift_Right (V, 16); end if;
      if (V and 16#FF#) = 0 then N := N + 8; V := Shift_Right (V, 8); end if;
      if (V and 16#F#) = 0 then N := N + 4; V := Shift_Right (V, 4); end if;
      if (V and 16#3#) = 0 then N := N + 2; V := Shift_Right (V, 2); end if;
      if (V and 16#1#) = 0 then N := N + 1; end if;
      return N;
   end Lowest_Bit;

   procedure Include (Set : in out Arena_Set; I : Arena_Index) with Inline_Always is
      W : constant Word_Number := (I - 1) / 64;
      Bit : constant Unsigned_64 := Shift_Left (1, (I - 1) mod 64);
   begin
      if (Set.Words (W) and Bit) = 0 then
         Set.Words (W) := Set.Words (W) or Bit;
         Set.Summary := Set.Summary or Shift_Left (1, W);
      end if;
   end Include;

   procedure Exclude (Set : in out Arena_Set; I : Arena_Index) is
      W : constant Word_Number := (I - 1) / 64;
   begin
      Set.Words (W) := Set.Words (W) and not Shift_Left (1, (I - 1) mod 64);
      if Set.Words (W) = 0 then
         Set.Summary := Set.Summary and not Shift_Left (1, W);
      end if;
   end Exclude;

   --  I in every class's set, or every bucket's.
   procedure Include_Every (Sets : in out Class_Sets; I : Arena_Index) is
   begin
      for K in HC.Size_Class loop
         Include (Sets (K), I);
      end loop;
   end Include_Every;

   procedure Include_Every (Sets : in out Bucket_Sets; I : Arena_Index) is
   begin
      for B in Medium_Bucket loop
         Include (Sets (B), I);
      end loop;
   end Include_Every;

   --  Some member, or 0 when there is none (a summary bit over an empty
   --  word is cleared on the way).
   procedure First (Set : in out Arena_Set; I : out Arena_Number) is
      W : Word_Number;
   begin
      I := 0;
      for Attempt in Word_Number loop
         exit when Set.Summary = 0;
         W := Lowest_Bit (Set.Summary);
         if Set.Words (W) /= 0 then
            I := W * 64 + Lowest_Bit (Set.Words (W)) + 1;
            return;
         end if;
         Set.Summary := Set.Summary and not Shift_Left (1, W);
      end loop;
   end First;

   --  Medium requests of Pages look in bucket Bucket_Of (Pages): the
   --  smallest B with 2 ** B >= Pages.
   function Bucket_Of (Pages : Medium_Pages) return Medium_Bucket is
     (if Pages <= 1 then 0 elsif Pages <= 2 then 1 elsif Pages <= 4 then 2 elsif Pages <= 8 then 3
      elsif Pages <= 16 then 4 elsif Pages <= 32 then 5 elsif Pages <= 64 then 6
      elsif Pages <= 128 then 7 else 8);

   ------------------------------------------------------------------------
   --  Arena metadata: each arena's proved core (Heap_Slabs or Heap_Extents)
   --  lives at its base. These overlay it and call the core; the core's
   --  Valid precondition is the assumption here, the one place each kind of
   --  arena memory is trusted (only the arena's heap's holder calls them,
   --  except the read-only Slab_Usable / Extent_Usable).
   ------------------------------------------------------------------------
   procedure Slab_Initialize (Base : Unsigned_64) with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57 is
      Heap : S.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      S.Initialize (Heap);
   end Slab_Initialize;

   procedure Slab_Allocate (Base : Unsigned_64; Size : HC.Request_Size; Value : out S.Allocation)
     with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57, Inline_Always
   is
      Heap : S.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      pragma Assume (S.Valid (Heap), "a small arena's metadata is as its holder last left it");
      S.Allocate (Heap, Size, Value);
   end Slab_Allocate;

   --  The fast path's allocation: a slot of Class from slab Page's cache.
   procedure Slab_Take (Base : Unsigned_64; Page : S.Slab_Id; Class : HC.Size_Class;
                        Position : out S.Offset; Success : out Boolean)
     with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57, Inline_Always,
     Post => (if Success then S.Page_Of (Position) = Page
              and then Unsigned_64 (Position) <= ARENA_PAYLOAD - Unsigned_64 (HC.Stride (Class)))
   is
      Heap : S.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      pragma Assume (S.Valid (Heap), "a small arena's metadata is as its holder last left it");
      S.Allocate_At (Heap, Page, Class, Position, Success);
   end Slab_Take;

   --  Released: Position was live and is now free; Class: its slab's class;
   --  Was_Full: the slab had no free slot before; Emptied: it is now empty
   --  (it may serve any class).
   procedure Slab_Release
     (Base : Unsigned_64; Position : S.Offset; Released, Was_Full, Emptied : out Boolean;
      Class : out HC.Size_Class)
     with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57, Inline_Always
   is
      Heap : S.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
      Status : S.Release_Status;
   begin
      pragma Assume (S.Valid (Heap), "a small arena's metadata is as its holder last left it");
      S.Release_Reporting (Heap, Position, Status, Class, Was_Full, Emptied);
      Released := Status = S.Released;
   end Slab_Release;

   --  The bytes of the live small block at Position, or 0.
   function Slab_Usable (Base : Unsigned_64; Position : S.Offset) return Unsigned_64 with
     Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57, Post => Slab_Usable'Result <= SMALL_LIMIT
   is
      Heap : S.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      if S.Live (Heap, Position) then
         return Unsigned_64 (HC.Stride (S.Class_Of (Heap, S.Page_Of (Position))));
      end if;
      return 0;
   end Slab_Usable;

   procedure Extent_Initialize (Base : Unsigned_64) with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57 is
      Heap : E.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      E.Initialize (Heap);
   end Extent_Initialize;

   procedure Extent_Allocate (Base : Unsigned_64; Pages : E.Run_Length; First : out E.Page_Reference)
     with Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57,
     Post => (if First /= E.No_Page then E.Fits (First, Pages))
   is
      Heap : E.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      pragma Assume (E.Valid (Heap), "a medium arena's metadata is as its holder last left it");
      E.Allocate (Heap, Pages, 1, First);
   end Extent_Allocate;

   procedure Extent_Release (Base : Unsigned_64; First : E.Page_Id; Released : out Boolean) with
     No_Inline,
     Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57
   is
      Heap : E.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      pragma Assume (E.Valid (Heap), "a medium arena's metadata is as its holder last left it");
      E.Release (Heap, First, Released);
   end Extent_Release;

   --  The bytes of the page run starting at First, or 0.
   function Extent_Usable (Base : Unsigned_64; First : E.Page_Id) return Unsigned_64 with
     Global => null, Pre => Base mod Page_Bytes = 0 and then Base < 2 ** 57, Post => Extent_Usable'Result <= ARENA_PAYLOAD
   is
      Heap : E.State with Import, Alignment => 8, Address => Page_Address (Base + ARENA_HEADER);
   begin
      return Unsigned_64 (E.Length (Heap, First)) * Page_Bytes;
   end Extent_Usable;

   ------------------------------------------------------------------------
   --  A heap's arenas
   ------------------------------------------------------------------------
   --  R names an arena the map can cover; its base.
   function Arena_Usable (R : Arena_Record) return Boolean is
     (R.Kind /= Unused and then R.Slot /= 0 and then Unsigned_64 (R.Slot) < MAP_SLOTS
      and then Unsigned_64 (R.Pages) <= ARENA_PAGES);
   function Base_Of (R : Arena_Record) return Unsigned_64 is (Unsigned_64 (R.Slot) * ARENA_SPAN)
     with Post => Base_Of'Result mod Page_Bytes = 0 and then Base_Of'Result < 2 ** 57
       and then Base_Of'Result mod ARENA_SPAN = 0
       and then (if Arena_Usable (R) then Base_Of'Result < ADDRESS_LIMIT and then Base_Of'Result /= 0);
   function Meta_Of (Kind : Arena_Kind) return Unsigned_64 is
     (if Kind = Small then SLAB_META else EXTENT_META)
     with Post => Meta_Of'Result in EXTENT_META .. SLAB_META;

   --  Back arena R from its base up to Upto (at most its span).
   procedure Ensure_Arena (R : in out Arena_Record; Upto : Unsigned_64; Success : out Boolean) with
     No_Inline,
     Pre => Arena_Usable (R) and then Upto <= ARENA_SPAN,
     Post => Arena_Usable (R) and then R.Kind = R.Kind'Old and then R.Slot = R.Slot'Old
       and then (if Success then Unsigned_64 (R.Pages) * Page_Bytes >= Upto)
   is
      Committed : Unsigned_64 := Unsigned_64 (R.Pages) * Page_Bytes;
   begin
      Ensure (Base_Of (R), ARENA_SPAN, Committed, Upto, Success);
      R.Pages := Unsigned_16 (Committed / Page_Bytes);
   end Ensure_Arena;

   --  A block for Size from small arena I, its backing in place; or
   --  No_Address (the arena's slabs unchanged) when it has none.
   --  Try_Small's rare tail: back the arena up to Ends for the block at
   --  Position, or give the block back.
   procedure Grow_Small (R : in out Arena_Record; Position : S.Offset; Ends : Unsigned_64; Done : out Boolean)
   with
     No_Inline,
     Pre => Arena_Usable (R) and then Ends <= ARENA_SPAN,
     Post => R.Kind = R.Kind'Old and then R.Slot = R.Slot'Old and then Arena_Usable (R)
   is
      Released, Was_Full, Emptied : Boolean;
      Class : HC.Size_Class;
   begin
      Ensure_Arena (R, Ends, Done);
      if not Done then
         Slab_Release (Base_Of (R), Position, Released, Was_Full, Emptied, Class);
      end if;
   end Grow_Small;

   --  A block for Size from small arena I, its backing in place; or
   --  No_Address (the arena's slabs unchanged) when it has none. The whole
   --  slab that holds it is backed (so it may be cached: Slab_Cache), Page
   --  is that slab.
   procedure Try_Small (Table : in out Arena_Table; I : Arena_Index; Size : HC.Request_Size;
                        Item : out Unsigned_64; Page : out S.Slab_Id) with
     Post => (for all J in Arena_Index => Table (J).Kind = Table'Old (J).Kind)
       and then (if Item /= No_Address then Arena_Usable (Table (I)) and then Table (I).Kind = Small)
   is
      R : constant Arena_Record := Table (I);
      Value : S.Allocation;
      Offset, Ends : Unsigned_64;
      Done : Boolean;
   begin
      Item := No_Address;
      Page := S.Slab_Id'First;
      if R.Kind /= Small or else not Arena_Usable (R) then
         return;
      end if;
      Slab_Allocate (Base_Of (R), Size, Value);
      if not Value.Value.Success then
         return;
      end if;
      Page := S.Page_Of (Value.Value.Position);
      Offset := SLAB_META + Unsigned_64 (Value.Value.Position);
      Ends := SLAB_META + Unsigned_64 (Page) * S.Slab_Bytes;
      if Ends > Unsigned_64 (R.Pages) * Page_Bytes then
         Grow_Small (Table (I), Value.Value.Position, Ends, Done);
         if not Done then
            return;
         end if;
      end if;
      Item := Base_Of (R) + Offset;
   end Try_Small;

   procedure Try_Medium (R : in out Arena_Record; Pages : Medium_Pages; Item : out Unsigned_64) with
     Post => R.Kind = R.Kind'Old and then R.Slot = R.Slot'Old
   is
      First : E.Page_Reference;
      Offset, Ends : Unsigned_64;
      Done, Released : Boolean;
   begin
      Item := No_Address;
      if R.Kind /= Medium or else not Arena_Usable (R) then
         return;
      end if;
      Extent_Allocate (Base_Of (R), Pages, First);
      if First = E.No_Page then
         return;
      end if;
      Offset := EXTENT_META + Unsigned_64 (First - 1) * Page_Bytes;
      Ends := Offset + Unsigned_64 (Pages) * Page_Bytes;
      if Ends > Unsigned_64 (R.Pages) * Page_Bytes then
         Ensure_Arena (R, Ends, Done);
         if not Done then
            Extent_Release (Base_Of (R), First, Released);
            return;
         end if;
      end if;
      Item := Base_Of (R) + Offset;
   end Try_Medium;

   --  The bytes of the live block Item in arena R (0: none).
   function Usable_In_Arena (R : Arena_Record; Item : Unsigned_64) return Unsigned_64 with
     Post => Usable_In_Arena'Result <= ARENA_PAYLOAD
   is
      Base, Offset, Meta : Unsigned_64;
   begin
      if not Arena_Usable (R) or else Shift_Right (Item, ARENA_SHIFT) /= Unsigned_64 (R.Slot) then
         return 0;
      end if;
      Base := Base_Of (R);
      Meta := Meta_Of (R.Kind);
      Offset := Item - Base;
      if Offset < Meta or else Offset - Meta >= ARENA_PAYLOAD then
         return 0;
      end if;
      Offset := Offset - Meta;
      if R.Kind = Small then
         return Slab_Usable (Base, S.Offset (Offset));
      elsif Offset mod Page_Bytes = 0 then
         return Extent_Usable (Base, E.Page_Id (Offset / Page_Bytes + 1));
      end if;
      return 0;
   end Usable_In_Arena;

   --  A block of Class was released in arena I, into a slab that was full
   --  before (Note_Room) or is now empty (Note_Empty): the arena may serve
   --  the class again (any class, when empty). Only these transitions are
   --  noted; a release into a slab with room changes nothing here (the
   --  arena's bit is set already, or was cleared only when none of its
   --  slabs had room). I outside the table: nothing is noted. Note_Room is
   --  inline (a slab at its fill line toggles it on every free: a bit test
   --  when it is set already); Note_Empty, which touches every class, not.
   procedure Note_Room (P : in out Heap_Private; I : Natural; Class : HC.Size_Class) with
     Inline_Always,
     Pre => Private_Valid (P),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      Capacity : constant Arena_Number := P.Capacity with Ghost;
      Committed : constant Unsigned_64 := P.Committed with Ghost;
   begin
      if I /= 0 and then I <= P.Arenas then
         Include (P.Small_Sets (Class), I);
      end if;
      pragma Assert (P.Capacity = Capacity and then P.Committed = Committed);
   end Note_Room;

   procedure Note_Empty (P : in out Heap_Private; I : Natural) with
     No_Inline,
     Pre => Private_Valid (P),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      Capacity : constant Arena_Number := P.Capacity with Ghost;
      Committed : constant Unsigned_64 := P.Committed with Ghost;
   begin
      if I /= 0 and then I <= P.Arenas then
         Include_Every (P.Small_Sets, I);
      end if;
      pragma Assert (P.Capacity = Capacity and then P.Committed = Committed);
   end Note_Empty;

   procedure Note_Medium_Free (P : in out Heap_Private; I : Arena_Index) with
     Inline_Always,
     Pre => Private_Valid (P) and then I <= P.Arenas,
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      Capacity : constant Arena_Number := P.Capacity with Ghost;
      Committed : constant Unsigned_64 := P.Committed with Ghost;
   begin
      if P.Current_Medium /= I then
         P.Current_Medium := I;
      end if;
      Include_Every (P.Medium_Sets, I);
      pragma Assert (P.Capacity = Capacity and then P.Committed = Committed);
   end Note_Medium_Free;

   --  Release Item, in this heap's arena I, and note the capacity it gives
   --  back. Anything that is not a live block there is ignored.
   procedure Release_Local (P : in out Heap_Private; Table : Arena_Table; Me : Unsigned_64;
                            I : Arena_Index; Item : Unsigned_64) with
     Inline_Always,
     Pre => Private_Valid (P) and then Holds (P, Me) and then I <= P.Arenas,
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      R : constant Arena_Record := Table (I);
      Base, Offset : Unsigned_64;
      Released, Was_Full, Emptied : Boolean;
      Class : HC.Size_Class;
   begin
      if not Arena_Usable (R) or else Shift_Right (Item, ARENA_SHIFT) /= Unsigned_64 (R.Slot) then
         return;
      end if;
      Base := Base_Of (R);
      Offset := Item - Base;
      if R.Kind = Small then
         if Offset < SLAB_META or else Offset - SLAB_META >= ARENA_PAYLOAD then
            return;
         end if;
         Slab_Release (Base, S.Offset (Offset - SLAB_META), Released, Was_Full, Emptied, Class);
         if Released then
            P.Cache (Class) := (Arena => Base, Page => S.Page_Of (S.Offset (Offset - SLAB_META)));
         end if;
         if Was_Full then
            Note_Room (P, I, Class);
         end if;
         if Emptied then
            Note_Empty (P, I);
         end if;
      else
         if Offset < EXTENT_META or else Offset - EXTENT_META >= ARENA_PAYLOAD
           or else (Offset - EXTENT_META) mod Page_Bytes /= 0
         then
            return;
         end if;
         Extent_Release (Base, E.Page_Id ((Offset - EXTENT_META) / Page_Bytes + 1), Released);
         if Released then
            Note_Medium_Free (P, I);
         end if;
      end if;
   end Release_Local;

   --  Map Item to its arena in this heap (I = 0 when it is not this heap's).
   procedure Own_Arena (P : Heap_Private; Item : Unsigned_64; I : out Arena_Number) with
     Pre => State_Valid and then Ready and then Private_Valid (P),
     Post => I <= P.Arenas
   is
      Value : Unsigned_32;
      Index : Natural;
   begin
      I := 0;
      Map_Lookup (Item, Value);
      Index := Natural (Shift_Right (Value, ENTRY_SHIFT) and 16#1FFF#);
      if Natural (Value and 16#FFFF#) = P.Id and then Index /= 0 and then Index <= P.Arenas then
         I := Index;
      end if;
   end Own_Arena;

   --  Room in the table for one more arena (backing another page of
   --  records when the backed ones are all used).
   procedure Table_Room (P : in out Heap_Private; Base : Unsigned_64; Success : out Boolean) with
     Pre => Private_Valid (P) and then Heap_Base_Valid (Base),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old and then (if Success then P.Arenas < P.Capacity)
   is
      Committed : Unsigned_64 := P.Committed;
      Wanted : Unsigned_64;
   begin
      Success := P.Arenas < P.Capacity;
      if Success or else P.Arenas = MAXIMUM_HEAP_ARENAS then
         return;
      end if;
      Wanted := TABLE_OFFSET + Unsigned_64 (P.Arenas + 1) * ARENA_RECORD_BYTES;
      Ensure (Base, HEAP_BYTES, Committed, Wanted, Success);
      P.Committed := Committed;
      if Success then
         --  Room for exactly one more; the next call finds the backing
         --  there already (Ensure returns at once) until a step ends.
         P.Capacity := P.Arenas + 1;
      end if;
   end Table_Room;

   --  A new arena of Kind in this heap: table record I, mapped, its
   --  metadata backed and initialized, in every set of its kind. I = 0
   --  when the provider refuses or a limit is reached.
   procedure Create_Arena (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64;
                           Kind : Arena_Kind; I : out Arena_Number) with
     Pre => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then Holds (P, Me) and then Heap_Base_Valid (Base) and then Kind /= Unused,
     Post => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old and then I <= P.Arenas
       and then (if I /= 0 then Table (I).Kind = Kind)
   is
      Room, Done, Mapped : Boolean;
      Arena : Unsigned_64;
      Identity : Unsigned_32;
      R : Arena_Record;
   begin
      I := 0;
      Table_Room (P, Base, Room);
      if not Room then
         return;
      end if;
      Provider_Reserve (ARENA_SPAN, ARENA_SPAN, Arena);
      if Arena = 0 then
         return;
      elsif Arena mod ARENA_SPAN /= 0 or else Arena >= ADDRESS_LIMIT then
         Provider_Release (Arena, ARENA_SPAN, Done);
         return;
      end if;
      R := (Slot => Unsigned_32 (Arena / ARENA_SPAN), Pages => 0, Kind => Kind);
      Ensure_Arena (R, Meta_Of (Kind), Done);
      if not Done then
         Provider_Release (Arena, ARENA_SPAN, Done);
         return;
      end if;
      if Kind = Small then
         Slab_Initialize (Arena);
      else
         Extent_Initialize (Arena);
      end if;
      --  The record and the identity word first: a thread that sees the map
      --  entry, or holds a block of the arena, sees them.
      Table (P.Arenas + 1) := R;
      Identity := Unsigned_32 (P.Id) or Shift_Left (Unsigned_32 (P.Arenas + 1), ENTRY_SHIFT)
        or (if Kind = Medium then ENTRY_MEDIUM else 0);
      Store_32 (Arena, Identity);
      Lock;
      Map_Set (Unsigned_64 (R.Slot), Identity, Mapped);
      Unlock;
      if not Mapped then
         Table (P.Arenas + 1) := (others => <>);
         Provider_Release (Arena, ARENA_SPAN, Done);
         return;
      end if;
      declare
         Capacity_Before : constant Arena_Number := P.Capacity with Ghost;
         Committed_Before : constant Unsigned_64 := P.Committed with Ghost;
      begin
         if Kind = Small then
            Include_Every (P.Small_Sets, P.Arenas + 1);
         else
            Include_Every (P.Medium_Sets, P.Arenas + 1);
         end if;
         pragma Assert (P.Capacity = Capacity_Before and then P.Committed = Committed_Before);
         pragma Assert (TABLE_OFFSET + Unsigned_64 (P.Capacity) * ARENA_RECORD_BYTES <= P.Committed);
      end;
      P.Arenas := P.Arenas + 1;
      I := P.Arenas;
   end Create_Arena;

   ------------------------------------------------------------------------
   --  Remote frees: batches, inboxes, draining
   ------------------------------------------------------------------------
   --  Seal Node with Used entries and push it to Destination's inbox. A
   --  destination always exists (its number came from the map); a batch
   --  for none is dropped (leaked), never written anywhere.
   procedure Push_Batch (Destination : Heap_Number; Node : Unsigned_64; Used : Node_Count) with
     Pre => State_Valid and then Ready
   is
      Base : Unsigned_64;
   begin
      Node_Seal (Node, Used, NODE_CAPACITY);
      Heap_Base_Of (Destination, Base);
      if Base /= 0 and then Base <= Unsigned_64'Last - INBOX_OFFSET then
         Push_Node (Base + INBOX_OFFSET, Node);
      end if;
   end Push_Batch;

   --  Publish slot O's batch to its destination's inbox; the slot empties.
   procedure Publish (P : in out Heap_Private; O : Outgoing_Slot) with
     Pre => State_Valid and then Ready and then Private_Valid (P),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old and then P.Outgoing (O).Destination = 0
   is
   begin
      if P.Outgoing (O).Destination /= 0 then
         Push_Batch (P.Outgoing (O).Destination, P.Outgoing (O).Node, P.Outgoing (O).Count);
      end if;
      P.Outgoing (O) := (Destination => 0, Node => 0, Count => 0);
   end Publish;

   procedure Publish_All (P : in out Heap_Private) with
     Pre => State_Valid and then Ready and then Private_Valid (P),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
   begin
      for O in Outgoing_Slot loop
         pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                and then P.Id = P.Id'Loop_Entry and then P.Arenas = P.Arenas'Loop_Entry);
         if P.Outgoing (O).Destination /= 0 then
            Publish (P, O);
         end if;
      end loop;
   end Publish_All;

   --  Add Item (a block of heap Owner) to the batch for Owner, Node
   --  starting it when there is none (Node: a node block, 0 when there is
   --  none to start with). Added: Item is in a batch now (the batch may
   --  have been published at once, full); Used: Node was taken. Item must
   --  not be added again once Added: a second add is a second free.
   procedure Batch_Add (P : in out Heap_Private; Owner : Heap_Id; Item, Node : Unsigned_64;
                        Added, Used : out Boolean) with
     Pre => State_Valid and then Ready and then Private_Valid (P),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      O : constant Outgoing_Slot := Owner mod OUTGOING_SLOTS;
   begin
      Added := False;
      Used := False;
      if P.Outgoing (O).Destination /= Owner then
         if Node = 0 then
            return;
         end if;
         Publish (P, O);
         P.Outgoing (O) := (Destination => Owner, Node => Node, Count => 0);
         Used := True;
      end if;
      Node_Put (P.Outgoing (O).Node, P.Outgoing (O).Count + 1, Item);
      Added := True;
      if P.Outgoing (O).Count + 1 = NODE_CAPACITY then
         --  Full: it goes now.
         Push_Batch (P.Outgoing (O).Destination, P.Outgoing (O).Node, NODE_CAPACITY);
         P.Outgoing (O) := (Destination => 0, Node => 0, Count => 0);
      else
         P.Outgoing (O).Count := P.Outgoing (O).Count + 1;
      end if;
   end Batch_Add;

   --  A node has been drained: give it back to its owner. Ours: release
   --  it. Another heap's: it carries our next batch to that heap (or rides
   --  in the current one). No allocation.
   procedure Return_Node (P : in out Heap_Private; Table : in out Arena_Table; Me, Node : Unsigned_64;
                          Room : Node_Count) with
     Pre => State_Valid and then Ready and then Private_Valid (P) and then Holds (P, Me) and then Node /= 0,
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      Value : Unsigned_32;
      Owner : Heap_Number;
      I : Arena_Number;
      Added, Used : Boolean;
      O : Outgoing_Slot;
      Home : Unsigned_64;
   begin
      Map_Lookup (Node, Value);
      Owner := Heap_Number (Value and 16#FFFF#);
      if Owner = 0 or else Owner > Heaps then
         return;
      elsif Owner = P.Id then
         Own_Arena (P, Node, I);
         if I /= 0 then
            Release_Local (P, Table, Me, I, Node);
         end if;
         return;
      end if;
      O := Owner mod OUTGOING_SLOTS;
      if P.Outgoing (O).Destination = Owner or else Room /= NODE_CAPACITY then
         --  It rides in the batch (or starts one, as a node of full size).
         Batch_Add (P, Owner, Node, (if Room = NODE_CAPACITY then Node else 0), Added, Used);
         if not Added then
            --  A short node and no batch to ride in: it goes home alone,
            --  as a node of no entries.
            Node_Seal (Node, 0, 0);
            Heap_Base_Of (Owner, Home);
            if Home /= 0 and then Home <= Unsigned_64'Last - INBOX_OFFSET then
               Push_Node (Home + INBOX_OFFSET, Node);
            end if;
         end if;
      else
         Publish (P, O);
         P.Outgoing (O) := (Destination => Owner, Node => Node, Count => 0);
      end if;
   end Return_Node;

   --  Take the whole inbox and release what it holds.
   procedure Drain (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64) with
     Pre => State_Valid and then Ready and then Private_Valid (P) and then Holds (P, Me)
       and then Heap_Base_Valid (Base),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      List, Node, Item : Unsigned_64;
      Used, Room : Node_Count;
      I : Arena_Number;
   begin
      Take_All (Base + INBOX_OFFSET, List);
      while List /= 0 loop
         pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                and then P.Id = P.Id'Loop_Entry and then P.Arenas = P.Arenas'Loop_Entry);
         Node := List;
         Node_Read (Node, List, Used, Room);
         for K in 1 .. Used loop
            pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                   and then P.Id = P.Id'Loop_Entry
                                   and then P.Arenas = P.Arenas'Loop_Entry);
            Node_Entry (Node, K, Item);
            Own_Arena (P, Item, I);
            if I /= 0 then
               Release_Local (P, Table, Me, I, Item);
            end if;
         end loop;
         Return_Node (P, Table, Me, Node, Room);
      end loop;
   end Drain;

   --  Pending remote frees in this heap's inbox.
   procedure Inbox_Pending (Base : Unsigned_64; Pending : out Boolean) with
     Pre => Heap_Base_Valid (Base)
   is
      Head : Unsigned_64;
   begin
      Load_64 (Base + INBOX_OFFSET, Head);
      Pending := Head /= 0;
   end Inbox_Pending;

   --  On a miss: drain the inbox, publish the batches.
   procedure Tend (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then Private_Valid (P) and then Holds (P, Me)
       and then Heap_Base_Valid (Base),
     Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
       and then P.Arenas = P.Arenas'Old
   is
      Pending : Boolean;
   begin
      Inbox_Pending (Base, Pending);
      if Pending then
         Drain (P, Table, Me, Base);
      end if;
      Publish_All (P);
   end Tend;

   ------------------------------------------------------------------------
   --  Allocation in a heap
   ------------------------------------------------------------------------
   procedure Allocate_Small (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64;
                             Size : HC.Request_Size; Item : out Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then Holds (P, Me) and then Heap_Base_Valid (Base),
     Post => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
   is
      Class : constant HC.Size_Class := HC.Class_For (Size);
      I : Arena_Number := 0;
      Page : S.Slab_Id;
      Pending : Boolean;

      --  Item came from slab Page of arena I: the class allocates there next.
      procedure Remember (I : Arena_Index) with
        Pre => Private_Valid (P) and then I <= P.Arenas and then Arena_Usable (Table (I)),
        Post => Private_Valid (P) and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
          and then P.Arenas = P.Arenas'Old
      is
      begin
         P.Cache (Class) := (Arena => Base_Of (Table (I)), Page => Page);
      end Remember;
   begin
      Item := No_Address;
      if P.Cache (Class).Arena /= 0 then
         Own_Arena (P, P.Cache (Class).Arena + SLAB_META, I);
      end if;
      if I /= 0 then
         --  The cached slab's cache is empty or it is full: the core
         --  refills, or finds another slab in the same arena.
         Try_Small (Table, I, Size, Item, Page);
         if Item /= No_Address then
            Remember (I);
            return;
         end if;
         Exclude (P.Small_Sets (Class), I);
      end if;
      --  A miss: first tend (what other threads freed back to us, our own
      --  batches out); then the arenas that may serve this class; then a
      --  new arena.
      Tend (P, Table, Me, Base);
      for Attempt in 1 .. 2 loop
         pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                and then P.Id = P.Id'Loop_Entry);
         for Probe in Arena_Index loop
            pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                   and then P.Id = P.Id'Loop_Entry);
            First (P.Small_Sets (Class), I);
            exit when I = 0;
            if I <= P.Arenas then
               Try_Small (Table, I, Size, Item, Page);
               if Item /= No_Address then
                  Remember (I);
                  return;
               end if;
            end if;
            Exclude (P.Small_Sets (Class), I);
         end loop;
         exit when Attempt = 2;
         Inbox_Pending (Base, Pending);
         exit when not Pending;
         Drain (P, Table, Me, Base);
      end loop;
      Create_Arena (P, Table, Me, Base, Small, I);
      Item := No_Address;
      if I /= 0 then
         Try_Small (Table, I, Size, Item, Page);
         if Item /= No_Address then
            Remember (I);
         end if;
      end if;
   end Allocate_Small;

   procedure Allocate_Medium (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64;
                              Bytes : Unsigned_64; Item : out Unsigned_64) with
     Pre => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then Holds (P, Me) and then Heap_Base_Valid (Base) and then Bytes in 1 .. HUGE_THRESHOLD,
     Post => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
   is
      Pages : constant Medium_Pages := Medium_Pages ((Bytes + (Page_Bytes - 1)) / Page_Bytes);
      Bucket : constant Medium_Bucket := Bucket_Of (Pages);
      I : Arena_Number := P.Current_Medium;
      Pending : Boolean;

      --  Arena I cannot serve Pages, so none of the buckets from this one up.
      procedure Note_Short (I : Arena_Index) with
        Post => P.Holder = P.Holder'Old and then P.Id = P.Id'Old and then P.Arenas = P.Arenas'Old
          and then P.Capacity = P.Capacity'Old and then P.Committed = P.Committed'Old
          and then P.Cache = P.Cache'Old and then P.Current_Medium = P.Current_Medium'Old
          and then P.Outgoing = P.Outgoing'Old
      is
      begin
         for B in Bucket .. Medium_Bucket'Last loop
            pragma Loop_Invariant (P.Holder = P.Holder'Loop_Entry and then P.Id = P.Id'Loop_Entry
                                   and then P.Arenas = P.Arenas'Loop_Entry
                                   and then P.Capacity = P.Capacity'Loop_Entry
                                   and then P.Committed = P.Committed'Loop_Entry
                                   and then P.Cache = P.Cache'Loop_Entry
                                   and then P.Current_Medium = P.Current_Medium'Loop_Entry
                                   and then P.Outgoing = P.Outgoing'Loop_Entry);
            Exclude (P.Medium_Sets (B), I);
         end loop;
      end Note_Short;
   begin
      if I /= 0 then
         Try_Medium (Table (I), Pages, Item);
         if Item /= No_Address then
            return;
         end if;
         Note_Short (I);
      end if;
      Tend (P, Table, Me, Base);
      for Attempt in 1 .. 2 loop
         pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                and then P.Id = P.Id'Loop_Entry);
         for Probe in Arena_Index loop
            pragma Loop_Invariant (Private_Valid (P) and then P.Holder = P.Holder'Loop_Entry
                                   and then P.Id = P.Id'Loop_Entry);
            First (P.Medium_Sets (Bucket), I);
            exit when I = 0;
            if I <= P.Arenas then
               Try_Medium (Table (I), Pages, Item);
               if Item /= No_Address then
                  P.Current_Medium := I;
                  return;
               end if;
               Note_Short (I);
            else
               Exclude (P.Medium_Sets (Bucket), I);
            end if;
         end loop;
         exit when Attempt = 2;
         Inbox_Pending (Base, Pending);
         exit when not Pending;
         Drain (P, Table, Me, Base);
      end loop;
      Create_Arena (P, Table, Me, Base, Medium, I);
      Item := No_Address;
      if I /= 0 then
         P.Current_Medium := I;
         Try_Medium (Table (I), Pages, Item);
      end if;
   end Allocate_Medium;

   --  A free of Item, mapped to Value (/= 0), in the calling thread's heap.
   procedure Free_In (P : in out Heap_Private; Table : in out Arena_Table; Me, Base : Unsigned_64;
                      Value : Unsigned_32; Item : Unsigned_64) with
     Pre => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then Holds (P, Me) and then Heap_Base_Valid (Base),
     Post => State_Valid and then Ready and then not Lock_Held and then Private_Valid (P)
       and then P.Holder = P.Holder'Old and then P.Id = P.Id'Old
   is
      Owner : constant Heap_Number := Heap_Number (Value and 16#FFFF#);
      Index : constant Natural := Natural (Shift_Right (Value, ENTRY_SHIFT) and 16#1FFF#);
      Added, Used, Pending : Boolean;
      Node : Unsigned_64;
      I : Arena_Number;
   begin
      if Owner = P.Id then
         if Index /= 0 and then Index <= P.Arenas then
            Release_Local (P, Table, Me, Index, Item);
         end if;
         return;
      elsif Owner = 0 or else Owner > Heaps then
         return;
      end if;
      --  A thread that frees others' blocks is likely sent its own back:
      --  take them now (one load of the inbox word), so they are reused
      --  before its arenas grow.
      Inbox_Pending (Base, Pending);
      if Pending then
         Drain (P, Table, Me, Base);
      end if;
      --  Another heap's block: into its batch. A new batch needs a node,
      --  from this heap; without one, the block itself travels alone (its
      --  first 16 bytes become a node of no entries).
      Batch_Add (P, Owner, Item, 0, Added, Used);
      if Added then
         return;
      end if;
      Allocate_Small (P, Table, Me, Base, NODE_BYTES, Node);
      if Node /= No_Address then
         Node_Seal (Node, 0, NODE_CAPACITY);
         Batch_Add (P, Owner, Item, Node, Added, Used);
         if not Used then
            --  A drain while the node was made started a batch for Owner
            --  (with a returned node); this one goes back.
            Own_Arena (P, Node, I);
            if I /= 0 then
               Release_Local (P, Table, Me, I, Node);
            end if;
         end if;
      else
         Node_Seal (Item, 0, 0);
         Heap_Base_Of (Owner, Node);
         if Node /= 0 and then Node <= Unsigned_64'Last - INBOX_OFFSET then
            Push_Node (Node + INBOX_OFFSET, Item);
         end if;
      end if;
   end Free_In;

   ------------------------------------------------------------------------
   --  Heaps: making, adopting, abandoning; each operation's entry
   ------------------------------------------------------------------------
   procedure Read_State (Base : Unsigned_64; Value : out Unsigned_64) with
     Pre => Lock_Held and then Heap_Base_Valid (Base)
   is
   begin
      Load_64 (Base + STATE_OFFSET, Value);
   end Read_State;

   procedure Write_State (Base, Value : Unsigned_64) with
     Pre => Lock_Held and then Heap_Base_Valid (Base)
   is
   begin
      Store_64 (Base + STATE_OFFSET, Value);
   end Write_State;

   --  Under the lock: a new heap held by Me (Base 0 when refused).
   procedure Create_Heap (Me : Unsigned_64; Base : out Unsigned_64) with
     Pre => State_Valid and then Ready and then Lock_Held and then Me /= 0,
     Post => State_Valid and then Ready and then Lock_Held
       and then (if Base /= 0 then Heap_Base_Valid (Base))
   is
      Committed : Unsigned_64 := 0;
      Done : Boolean;
      Id : Heap_Id;
   begin
      Base := 0;
      if Heaps = MAXIMUM_HEAPS then
         return;
      end if;
      Id := Heaps + 1;
      Ensure (Registry.Base, Registry.Total, Registry.Committed, (Unsigned_64 (Id) + 1) * 8, Done);
      if not Done then
         return;
      end if;
      declare
         Fresh : Unsigned_64;
      begin
         Provider_Reserve (HEAP_BYTES, Page_Bytes, Fresh);
         if Fresh = 0 then
            return;
         elsif Fresh mod Page_Bytes /= 0 or else Fresh > Unsigned_64'Last - HEAP_BYTES then
            Provider_Release (Fresh, HEAP_BYTES, Done);
            return;
         end if;
         Ensure (Fresh, HEAP_BYTES, Committed, HEAP_INITIAL, Done);
         if not Done then
            Provider_Release (Fresh, HEAP_BYTES, Done);
            return;
         end if;
         declare
            P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Fresh + PRIVATE_OFFSET);
            Empty : constant Arena_Set := (Summary => 0, Words => [others => 0]);
         begin
            P := (Id => Id, Holder => Me, Arenas => 0,
                  Capacity => Arena_Number ((Committed - TABLE_OFFSET) / ARENA_RECORD_BYTES),
                  Committed => Committed, Cache => [others => (0, S.Slab_Id'First)],
                  Current_Medium => 0, Small_Sets => [others => Empty],
                  Medium_Sets => [others => Empty], Outgoing => [others => (0, 0, 0)]);
         end;
         Write_State (Fresh, HEAP_HELD);
         Store_64 (Registry.Base + Unsigned_64 (Id) * 8, Fresh);
         Heaps := Id;
         Base := Fresh;
      end;
   end Create_Heap;

   --  Under the lock: an abandoned heap, now held by Me (Base 0: none).
   procedure Adopt_Heap (Me : Unsigned_64; Base : out Unsigned_64) with
     Pre => State_Valid and then Ready and then Lock_Held and then Me /= 0,
     Post => State_Valid and then Ready and then Lock_Held
       and then (if Base /= 0 then Heap_Base_Valid (Base))
   is
      Candidate, State : Unsigned_64;
   begin
      Base := 0;
      if Abandoned = 0 then
         return;
      end if;
      for Id in 1 .. Heaps loop
         pragma Loop_Invariant (State_Valid and then Ready and then Lock_Held);
         Load_64 (Registry.Base + Unsigned_64 (Id) * 8, Candidate);
         if Candidate /= 0 and then Candidate mod Page_Bytes = 0
           and then Candidate <= Unsigned_64'Last - HEAP_BYTES and then Candidate /= Fallback_Heap
         then
            Read_State (Candidate, State);
            if State = HEAP_ABANDONED then
               declare
                  P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Candidate + PRIVATE_OFFSET);
               begin
                  P.Holder := Me;
               end;
               Write_State (Candidate, HEAP_HELD);
               Abandoned := Abandoned - 1;
               Base := Candidate;
               return;
            end if;
         end if;
      end loop;
   end Adopt_Heap;

   --  The calling thread's heap, taking one (adopted or new) when it has
   --  none: Base 0 when refused. Me /= 0.
   procedure Attach (Me : Unsigned_64; Base : out Unsigned_64) with
     Pre => State_Valid and then not Lock_Held and then Me /= 0,
     Post => State_Valid and then not Lock_Held and then (if Base /= 0 then Ready and then Heap_Base_Valid (Base))
   is
      Started : Boolean;
   begin
      Get_Thread_Heap (Base);
      if Base /= 0 then
         if not Ready or else Base mod Page_Bytes /= 0 or else Base > Unsigned_64'Last - HEAP_BYTES then
            Base := 0;
         end if;
         return;
      end if;
      Lock;
      Start (Started);
      if Started then
         Adopt_Heap (Me, Base);
         if Base = 0 then
            Create_Heap (Me, Base);
         end if;
      end if;
      Unlock;
      if Base /= 0 then
         Set_Thread_Heap (Base);
         Attached;
      end if;
   end Attach;

   --  The fallback heap, for threads without a thread pointer, made on
   --  first use. Base 0 when refused.
   procedure Fallback (Base : out Unsigned_64) with
     Pre => State_Valid and then not Lock_Held and then Fallback_Held,
     Post => State_Valid and then not Lock_Held and then Fallback_Held
       and then (if Base /= 0 then Ready and then Heap_Base_Valid (Base))
   is
      Started : Boolean;
   begin
      if Fallback_Heap = 0 then
         Lock;
         Start (Started);
         if Started then
            Create_Heap (FALLBACK_TOKEN, Base);
            Fallback_Heap := Base;
         end if;
         Unlock;
      end if;
      Base := (if Ready then Fallback_Heap else 0);
   end Fallback;

   --  An operation's start: the caller's token Me and heap Base (0: none
   --  could be had), the fallback lock taken when it is the fallback heap.
   procedure Enter (Me, Base : out Unsigned_64) with
     Pre => State_Valid and then not Lock_Held and then not Fallback_Held,
     Post => State_Valid and then not Lock_Held and then Fallback_Held = (Me = FALLBACK_TOKEN)
       and then (if Base /= 0 then Ready and then Heap_Base_Valid (Base) and then Me /= 0)
   is
   begin
      Get_Token (Me);
      if Me /= 0 and then Me /= FALLBACK_TOKEN then
         Attach (Me, Base);
      else
         Me := FALLBACK_TOKEN;
         Lock_Fallback;
         Fallback (Base);
      end if;
   end Enter;

   procedure Leave (Me : Unsigned_64) with
     Pre => Fallback_Held = (Me = FALLBACK_TOKEN), Post => not Fallback_Held
   is
   begin
      if Me = FALLBACK_TOKEN then
         Unlock_Fallback;
      end if;
   end Leave;

   ------------------------------------------------------------------------
   --  The operations in the caller's heap: overlay its private part and
   --  table, check the caller holds it, work.
   ------------------------------------------------------------------------
   --  What a request is: refused, small (Size), medium, huge.
   type Request_Kind is (Refused, Small_Request, Medium_Request, Huge_Request);
   function Kind_Of (Wanted, Align : Unsigned_64) return Request_Kind is
     (if Align > Maximum_Alignment or else (Align and (Align - 1)) /= 0 or else Wanted > Maximum_Request
        or else Wanted = 0 or else Align = 0 then Refused
      elsif Wanted <= SMALL_LIMIT and then Align <= SMALL_LIMIT then Small_Request
      elsif Wanted <= HUGE_THRESHOLD and then Align <= Page_Bytes then Medium_Request
      else Huge_Request);

   procedure Allocate_Here (Base, Me : Unsigned_64; Kind : Request_Kind; Wanted, Align : Unsigned_64;
                            Item : out Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then not Lock_Held and then Heap_Base_Valid (Base)
       and then Me /= 0 and then (if Me = FALLBACK_TOKEN then Fallback_Held)
       and then Kind in Small_Request | Medium_Request and then Kind = Kind_Of (Wanted, Align),
     Post => State_Valid and then Ready and then not Lock_Held
   is
      P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
      Table : Arena_Table with Import, Alignment => 8, Address => Page_Address (Base + TABLE_OFFSET);
   begin
      pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
      Item := No_Address;
      if P.Holder /= Me then
         return;
      end if;
      if Kind = Small_Request then
         Allocate_Small (P, Table, Me, Base, HC.Request_Size (Unsigned_64'Max (Wanted, Align)), Item);
      else
         Allocate_Medium (P, Table, Me, Base, Wanted, Item);
      end if;
   end Allocate_Here;

   procedure Free_Here (Base, Me : Unsigned_64; Value : Unsigned_32; Item : Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then not Lock_Held and then Heap_Base_Valid (Base)
       and then Me /= 0 and then (if Me = FALLBACK_TOKEN then Fallback_Held),
     Post => State_Valid and then Ready and then not Lock_Held
   is
      P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
      Table : Arena_Table with Import, Alignment => 8, Address => Page_Address (Base + TABLE_OFFSET);
   begin
      pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
      if P.Holder = Me then
         Free_In (P, Table, Me, Base, Value, Item);
         --  The fallback heap may not run again soon: its batches go now.
         if Me = FALLBACK_TOKEN then
            Publish_All (P);
         end if;
      end if;
   end Free_Here;

   procedure Exit_Here (Base, Me : Unsigned_64; Left : out Boolean) with
     Pre => State_Valid and then Ready and then not Lock_Held and then Heap_Base_Valid (Base)
       and then Me /= 0 and then Me /= FALLBACK_TOKEN,
     Post => State_Valid and then Ready and then not Lock_Held
   is
      P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
      Table : Arena_Table with Import, Alignment => 8, Address => Page_Address (Base + TABLE_OFFSET);
   begin
      pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
      Left := P.Holder = Me;
      if Left then
         Drain (P, Table, Me, Base);
         Publish_All (P);
         P.Holder := 0;
      end if;
   end Exit_Here;

   --  The block's arena record, read from its heap's table (another
   --  thread's heap included: the words read are fixed while the block is
   --  live). Found False when the map names no usable arena.
   procedure Find_Arena (Value : Unsigned_32; R : out Arena_Record; Found : out Boolean) with
     Pre => State_Valid and then Ready
   is
      Owner : constant Heap_Number := Heap_Number (Value and 16#FFFF#);
      Index : constant Natural := Natural (Shift_Right (Value, ENTRY_SHIFT) and 16#1FFF#);
      Base : Unsigned_64;
   begin
      R := (others => <>);
      Found := False;
      Heap_Base_Of (Owner, Base);
      if Base = 0 or else Base mod Page_Bytes /= 0 or else Base > Unsigned_64'Last - HEAP_BYTES
        or else Index = 0 or else Index > MAXIMUM_HEAP_ARENAS
      then
         return;
      end if;
      declare
         P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
         Table : Arena_Table with Import, Alignment => 8, Address => Page_Address (Base + TABLE_OFFSET);
      begin
         pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
         if Index <= P.Arenas then
            R := Table (Index);
            Found := True;
         end if;
      end;
   end Find_Arena;

   ------------------------------------------------------------------------
   --  The fast paths: a small request, or a free of a block of the
   --  caller's own heap, served from the arena its heap names, with no
   --  call out of line. Hit False: take the full path (which repeats what
   --  was tried here, nothing having changed).
   ------------------------------------------------------------------------
   procedure Allocate_Fast (Size : HC.Request_Size; Item : out Unsigned_64; Hit : out Boolean) with
     Inline_Always,
     Pre => State_Valid and then Unlocked, Post => State_Valid and then Unlocked
   is
      Me, Base : Unsigned_64;
   begin
      Item := No_Address;
      Hit := False;
      Get_Token (Me);
      if Me <= FALLBACK_TOKEN then
         return;
      end if;
      Get_Thread_Heap (Base);
      if Base = 0 or else Base mod Page_Bytes /= 0 then
         return;
      end if;
      declare
         P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
         Class : constant HC.Size_Class := HC.Class_For (Size);
         Arena : Unsigned_64;
         Position : S.Offset;
      begin
         pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
         Arena := P.Cache (Class).Arena;
         if P.Holder /= Me or else Arena = 0 then
            return;
         end if;
         Slab_Take (Arena, P.Cache (Class).Page, Class, Position, Hit);
         if Hit then
            Item := Arena + SLAB_META + Unsigned_64 (Position);
         end if;
      end;
   end Allocate_Fast;

   --  A free of a small block of the caller's own heap: the map entry Value
   --  names the heap and says small, so the block's arena is its address
   --  rounded down to the span; no table record is read. Hit False: take
   --  the full path.
   procedure Free_Fast (Item : Unsigned_64; Value : Unsigned_32; Hit : out Boolean) with
     Inline_Always,
     Pre => State_Valid and then Ready and then Unlocked and then Item < ADDRESS_LIMIT,
     Post => State_Valid and then Unlocked
   is
      Me, Base : Unsigned_64;
      Arena : constant Unsigned_64 := Item - Item mod ARENA_SPAN;
      --  Below the payload, the subtraction wraps past the payload's size.
      Offset : constant Unsigned_64 := Item mod ARENA_SPAN - SLAB_META;
   begin
      Hit := False;
      Get_Token (Me);
      if Me <= FALLBACK_TOKEN or else Offset >= ARENA_PAYLOAD then
         return;
      end if;
      Get_Thread_Heap (Base);
      if Base = 0 or else Base mod Page_Bytes /= 0 then
         return;
      end if;
      declare
         P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
         Position : constant S.Offset := S.Offset (Offset);
         Released, Was_Full, Emptied : Boolean;
         Class : HC.Size_Class;
      begin
         pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
         if P.Holder /= Me or else Natural (Value and 16#FFFF#) /= P.Id then
            return;
         end if;
         Slab_Release (Arena, Position, Released, Was_Full, Emptied, Class);
         if Released then
            --  The class allocates next where it last freed: that slot is
            --  on top of the slab's cache (LIFO reuse, warm in cache). The
            --  slab held a block, so it is backed to its end (Try_Small).
            P.Cache (Class).Arena := Arena;
            P.Cache (Class).Page := S.Page_Of (Position);
            if Was_Full then
               Note_Room (P, Natural (Shift_Right (Value, ENTRY_SHIFT) and 16#1FFF#), Class);
            end if;
            if Emptied then
               Note_Empty (P, Natural (Shift_Right (Value, ENTRY_SHIFT) and 16#1FFF#));
            end if;
         end if;
         Hit := True;
      end;
   end Free_Fast;

   ------------------------------------------------------------------------
   --  Huge blocks, under the lock: overlay the directory records.
   ------------------------------------------------------------------------
   procedure Allocate_Huge_Over (Last : Block_Number; Base, Bytes, Alignment : Unsigned_64;
                                 Item : out Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then Lock_Held and then Last = Capacity
       and then Base = Directory.Base and then Bytes in 1 .. Maximum_Request
       and then Alignment <= Maximum_Alignment,
     Post => State_Valid and then Ready and then Lock_Held
   is
      Table : Block_Table (1 .. Last) with Import, Alignment => 8, Address => Page_Address (Base);
      Align : constant Unsigned_64 := Unsigned_64'Max (Alignment, Page_Bytes);
      --  The reservation starts on an arena span boundary, the block Offset
      --  above it: below the span, one alignment unit (at least the header
      --  page, never handed out, reading zero); from the span up, the block
      --  is the span boundary itself. Either way a sized free that probes
      --  the arena header at Item rounded down to the span (Free_Sized)
      --  reads this reservation's zero header page or is refused before
      --  the probe, never reads client data or unmapped memory.
      Offset : constant Unsigned_64 := (if Align >= ARENA_SPAN then 0 else Round_Up_Pages (Align));
      Placement : constant Unsigned_64 := Unsigned_64'Max (Align, ARENA_SPAN);
      Payload_Bytes : constant Unsigned_64 := Round_Up_Pages (Bytes);
      Total : constant Unsigned_64 := Offset + Payload_Bytes;
      Reserved : Unsigned_64;
      R : Block_Record;
      Position : Block_Number := No_Block;
      Done : Boolean;
   begin
      pragma Assume (Directory_Valid (Table, Count), "the directory is as the last locked operation left it");
      Item := No_Address;
      if Count >= Table'Last then
         return;
      end if;
      Provider_Reserve (Total, Placement, Reserved);
      if Reserved = 0 then
         return;
      end if;
      --  The provider's answer must be placed as asked and fit the address
      --  space; anything else is given back.
      if Reserved > Unsigned_64'Last - Total or else Reserved mod Placement /= 0
        or else Reserved mod Page_Bytes /= 0
      then
         Provider_Release (Reserved, Total, Done);
         return;
      end if;
      R := (Base => Reserved, Total => Total, Payload => Reserved + Offset, Committed => 0,
            Bytes => Payload_Bytes);
      Ensure (R.Base, R.Total, R.Committed, Total, Done);
      if Done then
         Insert (Table, R, Position);
      end if;
      if not Done or else Position = No_Block then
         Provider_Release (Reserved, Total, Done);
         return;
      end if;
      Item := R.Payload;
   end Allocate_Huge_Over;

   --  Free (Item) when it is a huge block: Found when it routed to one.
   procedure Free_Huge_Over (Last : Block_Number; Base, Item : Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Ready and then Lock_Held and then Last = Capacity
       and then Base = Directory.Base,
     Post => State_Valid and then Ready and then Lock_Held
   is
      Table : Block_Table (1 .. Last) with Import, Alignment => 8, Address => Page_Address (Base);
      Position : Block_Number;
      Released : Boolean;
   begin
      pragma Assume (Directory_Valid (Table, Count), "the directory is as the last locked operation left it");
      Position := Block_Of (Table, Item);
      --  Bytes = 0: freed already, its release refused; retry it.
      if Position /= No_Block and then Item = Table (Position).Payload then
         Provider_Release (Table (Position).Base, Table (Position).Total, Released);
         if Released then
            Remove (Table, Position);
         else
            --  Kept, and still counted: the block is gone, the reservation
            --  is not. Freeing it again retries.
            Table (Position).Bytes := 0;
         end if;
      end if;
   end Free_Huge_Over;

   function Usable_Huge_Over (Last : Block_Number; Base, Item : Unsigned_64) return Unsigned_64 with
     No_Inline,
     Pre => State_Valid and then Ready and then Lock_Held and then Last = Capacity
       and then Base = Directory.Base,
     Post => Usable_Huge_Over'Result <= Maximum_Request
   is
      Table : Block_Table (1 .. Last) with Import, Alignment => 8, Address => Page_Address (Base);
      Position : Block_Number;
   begin
      pragma Assume (Directory_Valid (Table, Count), "the directory is as the last locked operation left it");
      Position := Block_Of (Table, Item);
      if Position /= No_Block and then Item = Table (Position).Payload
        and then Table (Position).Bytes <= Maximum_Request
      then
         return Table (Position).Bytes;
      end if;
      return 0;
   end Usable_Huge_Over;

   --  The directory's reservation plus each block's (Committed: backed bytes).
   function Sum_Huge_Over (Last : Block_Number; Base : Unsigned_64; Committed : Boolean) return Unsigned_64 with
     No_Inline,
     Pre => State_Valid and then Ready and then Lock_Held and then Last = Capacity
       and then Base = Directory.Base
   is
      Table : Block_Table (1 .. Last) with Import, Alignment => 8, Address => Page_Address (Base);
      Sum, Part : Unsigned_64 := 0;
   begin
      pragma Assume (Directory_Valid (Table, Count), "the directory is as the last locked operation left it");
      for P in 1 .. Count loop
         Part := (if Committed then Table (P).Committed else Table (P).Total);
         Sum := (if Sum > Unsigned_64'Last - Part then Unsigned_64'Last else Sum + Part);
      end loop;
      return Sum;
   end Sum_Huge_Over;

   ------------------------------------------------------------------------
   --  The public operations
   ------------------------------------------------------------------------
   function Invariant return Boolean is (State_Valid);
   function Unlocked return Boolean is (not Lock_Held and then not Fallback_Held);

   procedure Allocate_Huge (Wanted, Align : Unsigned_64; Item : out Unsigned_64) with
     Pre => State_Valid and then not Lock_Held and then Wanted in 1 .. Maximum_Request
       and then Align <= Maximum_Alignment,
     Post => State_Valid and then not Lock_Held
   is
      Started, Room : Boolean;
   begin
      Item := No_Address;
      Lock;
      Start (Started);
      if Started then
         --  Without room, Allocate_Huge_Over refuses.
         Make_Room (Room);
         Allocate_Huge_Over (Capacity, Directory.Base, Wanted, Align, Item);
      end if;
      Unlock;
   end Allocate_Huge;

   procedure Allocate_Slow (Bytes, Alignment : Unsigned_64; Zeroed : Boolean; Item : out Unsigned_64) with
     No_Inline,
     Pre => State_Valid and then Unlocked,
     Post => State_Valid and then Unlocked
   is
      Wanted : constant Unsigned_64 := Unsigned_64'Max (Bytes, 1);
      Align : constant Unsigned_64 := Unsigned_64'Max (Alignment, STANDARD_ALIGNMENT);
      Kind : constant Request_Kind := Kind_Of (Wanted, Align);
      Me, Base : Unsigned_64;
   begin
      Item := No_Address;
      case Kind is
         when Refused =>
            return;
         when Huge_Request =>
            --  Fresh backing, which reads as zero.
            Allocate_Huge (Wanted, Align, Item);
            return;
         when Small_Request | Medium_Request =>
            Enter (Me, Base);
            if Base /= 0 then
               Allocate_Here (Base, Me, Kind, Wanted, Align, Item);
            end if;
            Leave (Me);
            if Zeroed and then Item /= No_Address then
               Zero (Item, Wanted);
            end if;
      end case;
   end Allocate_Slow;

   procedure Allocate_Entry (Bytes, Alignment : Unsigned_64; Zeroed : Boolean; Item : out Unsigned_64) with
     Inline, Pre => State_Valid and then Unlocked, Post => State_Valid and then Unlocked
   is
      Hit : Boolean;
   begin
      --  Bytes in 1 .. SMALL_LIMIT (0 wraps above it), plain alignment.
      if Bytes - 1 < SMALL_LIMIT and then Alignment <= STANDARD_ALIGNMENT then
         Allocate_Fast (HC.Request_Size (Bytes), Item, Hit);
         if Hit then
            if Zeroed then
               Zero (Item, Bytes);
            end if;
            return;
         end if;
      end if;
      Allocate_Slow (Bytes, Alignment, Zeroed, Item);
   end Allocate_Entry;

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Allocate_Entry (Bytes, Alignment, False, Item);
      return Item;
   end Allocate;

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Allocate_Entry (Bytes, Alignment, True, Item);
      return Item;
   end Allocate_Zeroed;

   procedure Free_Slow (Item : Unsigned_64; Value : Unsigned_32) with
     No_Inline, Pre => State_Valid and then Ready and then Unlocked, Post => State_Valid and then Unlocked
   is
      Me, Base : Unsigned_64;
   begin
      if Value = 0 then
         Lock;
         Free_Huge_Over (Capacity, Directory.Base, Item);
         Unlock;
         return;
      end if;
      Enter (Me, Base);
      if Base /= 0 then
         Free_Here (Base, Me, Value, Item);
      end if;
      Leave (Me);
   end Free_Slow;

   procedure Free (Item : Unsigned_64) is
      Value : Unsigned_32;
      Hit : Boolean;
   begin
      if Item = No_Address or else not Ready then
         return;
      end if;
      Map_Lookup (Item, Value);
      if Value /= 0 and then (Value and ENTRY_MEDIUM) = 0 then
         Free_Fast (Item, Value, Hit);
         if Hit then
            return;
         end if;
      end if;
      Free_Slow (Item, Value);
   end Free;

   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64) is
      Wanted : constant Unsigned_64 := Unsigned_64'Max (Bytes, 1);
      Align : constant Unsigned_64 := Unsigned_64'Max (Alignment, STANDARD_ALIGNMENT);
      Value : Unsigned_32;
      Hit : Boolean;
   begin
      if Item = No_Address or else not Ready then
         return;
      end if;
      --  A small or medium block: its arena's identity word names its heap
      --  and table place. The size is not trusted: the probe is made only
      --  where a small or medium block can start (past an arena's header
      --  page), and every huge block is placed so that the same address
      --  rounded down is its own zero header page (Allocate_Huge_Over); a
      --  zero word, or an entry that is not this heap's small arena, takes
      --  the radix map (Free). A wrong size never makes it read client data
      --  or unmapped memory.
      if Item < ADDRESS_LIMIT and then Item mod ARENA_SPAN >= ARENA_HEADER
        and then Kind_Of (Wanted, Align) in Small_Request | Medium_Request
      then
         Load_32 (Item - Item mod ARENA_SPAN, Value);
         if Value /= 0 and then (Value and ENTRY_MEDIUM) = 0 then
            Free_Fast (Item, Value, Hit);
            if Hit then
               return;
            end if;
         end if;
      end if;
      Free (Item);
   end Free_Sized;

   procedure Usable_Entry (Item : Unsigned_64; Bytes : out Unsigned_64) with
     Pre => State_Valid and then Unlocked,
     Post => State_Valid and then Unlocked and then Bytes <= Maximum_Request
   is
      Value : Unsigned_32;
      R : Arena_Record;
      Found : Boolean;
   begin
      Bytes := 0;
      if Item = No_Address or else not Ready then
         return;
      end if;
      Map_Lookup (Item, Value);
      if Value = 0 then
         Lock;
         Bytes := Usable_Huge_Over (Capacity, Directory.Base, Item);
         Unlock;
         return;
      end if;
      Find_Arena (Value, R, Found);
      if Found then
         Bytes := Usable_In_Arena (R, Item);
      end if;
   end Usable_Entry;

   function Usable_Size (Item : Unsigned_64) return Unsigned_64 is
      Bytes : Unsigned_64;
   begin
      Usable_Entry (Item, Bytes);
      return Bytes;
   end Usable_Size;

   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 is
      Moved : Unsigned_64 := No_Address;
      Held : Unsigned_64;
   begin
      if Item = No_Address then
         Allocate_Entry (Bytes, STANDARD_ALIGNMENT, False, Moved);
         return Moved;
      end if;
      Usable_Entry (Item, Held);
      if Held = 0 then
         return No_Address;
      end if;
      --  It stays where it is when it fits, except a huge block shrunk to
      --  a small or medium size: it moves, so a sized free of the new size
      --  (Free_Sized) finds it in an arena.
      if Unsigned_64'Max (Bytes, 1) <= Held
        and then (Bytes > HUGE_THRESHOLD or else Held <= HUGE_THRESHOLD)
      then
         return Item;
      end if;
      Allocate_Entry (Bytes, STANDARD_ALIGNMENT, False, Moved);
      if Moved /= No_Address then
         Copy (Moved, Item, Unsigned_64'Min (Held, Unsigned_64'Max (Bytes, 1)));
         Free (Item);
      end if;
      return Moved;
   end Reallocate;

   procedure Thread_Exit is
      Me, Base : Unsigned_64;
      Left : Boolean;
   begin
      Get_Token (Me);
      if Me = 0 or else Me = FALLBACK_TOKEN or else not Ready then
         return;
      end if;
      Get_Thread_Heap (Base);
      if Base = 0 or else Base mod Page_Bytes /= 0 or else Base > Unsigned_64'Last - HEAP_BYTES then
         return;
      end if;
      Exit_Here (Base, Me, Left);
      Set_Thread_Heap (0);
      if Left then
         Lock;
         Write_State (Base, HEAP_ABANDONED);
         if Abandoned < Heaps then
            Abandoned := Abandoned + 1;
         end if;
         Unlock;
      end if;
   end Thread_Exit;

   procedure Statistics (Arenas, Reserved, Committed : out Unsigned_64) is
      function Add (A, B : Unsigned_64) return Unsigned_64 is
        (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);
      Base : Unsigned_64;
   begin
      Arenas := 0;
      Reserved := 0;
      Committed := 0;
      Lock;
      if Ready then
         Arenas := Unsigned_64 (Count);
         Reserved := Add (Add (Add (Directory.Total, Map.Total), Registry.Total),
                          Sum_Huge_Over (Capacity, Directory.Base, False));
         Committed := Add (Add (Add (Directory.Committed, Map.Committed), Registry.Committed),
                           Sum_Huge_Over (Capacity, Directory.Base, True));
         for Id in 1 .. Heaps loop
            pragma Loop_Invariant (State_Valid and then Ready and then Lock_Held);
            Heap_Base_Of (Id, Base);
            if Base /= 0 and then Base mod Page_Bytes = 0 and then Base <= Unsigned_64'Last - HEAP_BYTES then
               declare
                  P : Heap_Private with Import, Alignment => 8, Address => Page_Address (Base + PRIVATE_OFFSET);
                  Table : Arena_Table with Import, Alignment => 8, Address => Page_Address (Base + TABLE_OFFSET);
               begin
                  pragma Assume (Private_Valid (P), "a heap's private part is as its holder last left it");
                  Reserved := Add (Reserved, HEAP_BYTES);
                  Committed := Add (Committed, P.Committed);
                  for I in 1 .. P.Arenas loop
                     if Table (I).Kind /= Unused then
                        Arenas := Add (Arenas, 1);
                        Reserved := Add (Reserved, ARENA_SPAN);
                        Committed := Add (Committed, Unsigned_64 (Table (I).Pages) * Page_Bytes);
                     end if;
                  end loop;
               end;
            end if;
         end loop;
      end if;
      Unlock;
   end Statistics;
end CuAlloc;
