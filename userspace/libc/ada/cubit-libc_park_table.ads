------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's parked file handles (docs/filesystem-data-plane.md,
--  "Metadata operations"; docs/c-removal.md): closed handles kept open, by
--  their resolved name, for a later read-only open of the same name, the
--  least recently parked dropped first.
--
--  @description
--  Each service handle slot keeps its open's name here (CuBit names fit
--  Name_Bytes), its hash chain and its place in the recency list. Links are
--  a slot plus one, zero ending a list. Proved (tests/libc-ada): every
--  index and link stays in the table, every walk ends, Find returns only a
--  parked slot with exactly the asked name, and the count stays in bounds.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Filesystem_Queues;

package CuBit.Libc_Park_Table with Pure, SPARK_Mode is

   Slots : constant := CuBit.Filesystem_Queues.Maximum_Delegations;
   --  Names parked inline; a longer name is simply never parked (parking
   --  only saves a reopen), so names up to the path limit cost no memory.
   Name_Bytes : constant := 256;
   Buckets : constant := 2 * Slots;                --  a power of two
   --  The most handles parked at once (0 would turn parking off).
   Maximum_Parked : constant := 1_024;

   subtype Slot is Natural range 0 .. Slots - 1;
   subtype Link is Natural range 0 .. Slots;          --  slot + 1
   subtype Name_Length is Natural range 0 .. Name_Bytes;
   subtype Bucket is Natural range 0 .. Buckets - 1;
   No_Link : constant Link := 0;

   type Slot_Entry is record
      Name     : String (1 .. Name_Bytes) := [others => ' '];
      Length   : Name_Length := 0;       --  0: no name (cannot park)
      Hash     : Unsigned_64 := 0;
      Parked   : Boolean := False;
      Chain    : Link := No_Link;
      Newer    : Link := No_Link;        --  toward the newest parked
      Older    : Link := No_Link;
   end record;
   type Slot_Table is array (Slot) of Slot_Entry;
   type Bucket_Heads is array (Bucket) of Link;

   type Table is record
      Entries : Slot_Table;
      Heads   : Bucket_Heads := [others => No_Link];
      Newest  : Link := No_Link;
      Oldest  : Link := No_Link;
      Count   : Natural range 0 .. Slots := 0;
   end record;

   --  FNV-1a.
   function Name_Hash (Name : String) return Unsigned_64
   with Pre => Name'Length <= Name_Bytes;

   --  Remember Name as slot S's open (S is then not parked).
   procedure Set_Name (T : in out Table; S : Slot; Name : String)
   with Pre => Name'Length <= Name_Bytes and then not T.Entries (S).Parked,
        Post => T.Entries (S).Length = Name'Length and then not T.Entries (S).Parked;

   procedure Forget_Name (T : in out Table; S : Slot)
   with Pre => not T.Entries (S).Parked,
        Post => T.Entries (S).Length = 0 and then not T.Entries (S).Parked;

   --  The parked slot for Name, or No_Link.
   procedure Find (T : Table; Name : String; Found : out Link)
   with Pre => Name'Length <= Name_Bytes,
        Post => (if Found /= No_Link then T.Entries (Found - 1).Parked
                   and then T.Entries (Found - 1).Length = Name'Length
                   and then T.Entries (Found - 1).Name (1 .. Name'Length) = Name);

   procedure Insert (T : in out Table; S : Slot)
   with Pre => not T.Entries (S).Parked and then T.Entries (S).Length > 0,
        Post => T.Entries (S).Parked;

   procedure Remove (T : in out Table; S : Slot)
   with Pre => T.Entries (S).Parked,
        Post => not T.Entries (S).Parked
                and then T.Entries (S).Length = T.Entries'Old (S).Length;

end CuBit.Libc_Park_Table;
