------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Channel arenas: one grant a process lends netstack, cut into equal
--  channel buffers (each laid out as in Channel_Geometry: header, send ring,
--  receive ring). A channel, outbound or accepted, occupies one buffer, so a
--  process needs one grant for many channels rather than one per channel.
--
--  This unit keeps only the bookkeeping: which process owns an arena, how
--  it is cut, and which buffers channels hold. netstack keeps the grant
--  itself (reference and mapped address) beside it, by arena index.
--
--  Proved (tests/net-tcp): an arena never spans more than one grant
--  (16 MiB); a claimed buffer lies wholly inside its arena; a buffer is
--  held by at most one channel at a time (Claim takes only a free buffer);
--  an arena with buffers in use cannot be unregistered; and handles are
--  never reused, so a stale handle names no arena.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Channel_Geometry;

package Channel_Arenas with SPARK_Mode is

   Maximum_Arenas  : constant := 32;
   Maximum_Buffers : constant := 1_024;
   Page_Bytes      : constant := 4_096;
   --  The most one grant can lend (4096 pages).
   Maximum_Span    : constant := 4_096 * Page_Bytes;
   --  A header page and two rings of the smallest size.
   Minimum_Buffer  : constant := Channel_Geometry.Header_Bytes + 2 * Page_Bytes;

   subtype Arena_Index is Positive range 1 .. Maximum_Arenas;
   subtype Buffer_Count is Natural range 0 .. Maximum_Buffers;
   subtype Buffer_Index is Natural range 0 .. Maximum_Buffers - 1;
   subtype Buffer_Bytes is Positive
     range Minimum_Buffer .. Channel_Geometry.Maximum_Grant;
   subtype Span_Bytes is Natural range 0 .. Maximum_Span;
   subtype Owner_Id is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Handle is new Unsigned_64;
   No_Handle : constant Handle := 0;
   type Arena_List is array (Arena_Index) of Boolean;

   type Table is private;

   --  Count buffers of Size bytes fit in one grant.
   function Fits (Size : Buffer_Bytes; Count : Buffer_Count) return Boolean is
     (Count in 1 .. Maximum_Span / Size);

   function Registered (Item : Table; Index : Arena_Index) return Boolean;
   function Owner_Of (Item : Table; Index : Arena_Index) return Unsigned_64;
   function Size_Of (Item : Table; Index : Arena_Index) return Buffer_Bytes;
   function Count_Of (Item : Table; Index : Arena_Index) return Buffer_Count;
   --  A channel holds buffer Slot of the arena.
   function Held
     (Item : Table; Index : Arena_Index; Slot : Buffer_Index) return Boolean;
   --  No channel holds a buffer of the arena.
   function Idle (Item : Table; Index : Arena_Index) return Boolean;

   --  Every registered arena fits one grant.
   function Consistent (Item : Table) return Boolean;

   --  Record a new arena of Count buffers of Size bytes for Owner.
   procedure Register
     (Item    : in out Table;
      Owner   : Owner_Id;
      Size    : Buffer_Bytes;
      Count   : Buffer_Count;
      Arena   : out Handle;
      Index   : out Arena_Index;
      Success : out Boolean)
   with Pre  => Consistent (Item),
        Post => Consistent (Item) and then
                (if Success then Arena /= No_Handle and then
                   Registered (Item, Index) and then
                   Owner_Of (Item, Index) = Owner and then
                   Size_Of (Item, Index) = Size and then
                   Count_Of (Item, Index) = Count and then
                   Idle (Item, Index));

   --  Look up Owner's arena by handle.
   procedure Find
     (Item    : Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Index   : out Arena_Index;
      Found   : out Boolean)
   with Post => (if Found then Registered (Item, Index) and then
                   Owner_Of (Item, Index) = Owner);

   --  Take buffer Buffer of Owner's arena for a channel. Offset is where
   --  it starts in the arena's grant; the whole buffer lies inside.
   procedure Claim
     (Item    : in out Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Buffer  : Unsigned_64;
      Index   : out Arena_Index;
      Slot    : out Buffer_Index;
      Offset  : out Span_Bytes;
      Success : out Boolean)
   with Pre  => Consistent (Item),
        Post => Consistent (Item) and then
                (if Success then
                   Registered (Item, Index) and then
                   Owner_Of (Item, Index) = Owner and then
                   not Held (Item'Old, Index, Slot) and then
                   Held (Item, Index, Slot) and then
                   Slot < Count_Of (Item, Index) and then
                   Offset = Slot * Size_Of (Item, Index) and then
                   Offset + Size_Of (Item, Index) <= Maximum_Span);

   --  Return a buffer a channel held.
   procedure Release
     (Item  : in out Table;
      Index : Arena_Index;
      Slot  : Buffer_Index)
   with Pre  => Consistent (Item),
        Post => Consistent (Item) and then not Held (Item, Index, Slot);

   --  Forget Owner's arena; refused while any channel holds a buffer.
   procedure Unregister
     (Item    : in out Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Index   : out Arena_Index;
      Success : out Boolean)
   with Pre  => Consistent (Item),
        Post => Consistent (Item) and then
                (if Success then Idle (Item'Old, Index) and then
                   not Registered (Item, Index));

   --  Forget every arena Owner holds (its process has exited, and its
   --  channels are already released). Released marks which went.
   procedure Release_Owner
     (Item     : in out Table;
      Owner    : Owner_Id;
      Released : out Arena_List)
   with Pre  => Consistent (Item),
        Post => Consistent (Item) and then
                (for all I in Arena_Index =>
                   (if Released (I) then not Registered (Item, I)));

private

   type Use_Map is array (Buffer_Index) of Boolean;

   type Arena_Record is record
      Id    : Handle := No_Handle;
      Owner : Unsigned_64 := 0;
      Size  : Buffer_Bytes := Minimum_Buffer;
      Count : Buffer_Count := 0;
      Used  : Use_Map := [others => False];
   end record;

   type Arena_Array is array (Arena_Index) of Arena_Record;

   type Table is record
      Entries : Arena_Array := [others => <>];
      Next_Id : Handle := 1;
   end record;

   function Registered (Item : Table; Index : Arena_Index) return Boolean is
     (Item.Entries (Index).Id /= No_Handle);
   function Owner_Of (Item : Table; Index : Arena_Index) return Unsigned_64 is
     (Item.Entries (Index).Owner);
   function Size_Of (Item : Table; Index : Arena_Index) return Buffer_Bytes is
     (Item.Entries (Index).Size);
   function Count_Of (Item : Table; Index : Arena_Index) return Buffer_Count is
     (Item.Entries (Index).Count);
   function Held
     (Item : Table; Index : Arena_Index; Slot : Buffer_Index) return Boolean is
     (Item.Entries (Index).Used (Slot));
   function Idle (Item : Table; Index : Arena_Index) return Boolean is
     (for all U of Item.Entries (Index).Used => not U);

   function Consistent (Item : Table) return Boolean is
     (for all E of Item.Entries =>
        (if E.Id /= No_Handle then Fits (E.Size, E.Count)
         else (for all U of E.Used => not U)));

end Channel_Arenas;
