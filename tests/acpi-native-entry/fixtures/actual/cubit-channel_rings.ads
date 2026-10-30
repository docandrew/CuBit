------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One direction of a shared byte ring between two processes: the index
--  bookkeeping for its producer and for its consumer. It is used by
--  netstack and by the network channel clients (docs/netstack-redesign.md,
--  "Async channels").
--
--  Each side keeps its own index privately and never reads it back from
--  shared memory. It reads the peer's index once per step and passes that
--  value to Accept_Consumed or Accept_Produced, which take it only if it
--  moves forward and stays within the ring. So a peer that writes arbitrary
--  indices cannot make this side read or write outside the ring, or reuse
--  bytes it has not yet released. Such a peer can only garble its own
--  stream.
--
--  Indices are free-running 32-bit byte counts, and a byte's place in the
--  ring is its index modulo the size. A size is a power of two, so it
--  divides 2 ** 32 and positions stay consistent across wrap-around.
--
--  Proved (tests/channel-rings): every slice lies inside the ring; the
--  fill never exceeds the size; private indices only move forward, by
--  exactly the bytes committed or consumed; a rejected peer index changes
--  nothing. Tested, not proved: that the byte stream arrives in order and
--  intact, including across wrap-around (host tests). Memory ordering
--  between the processes belongs to the callers' volatile accesses and
--  fences, which SPARK does not model.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Channel_Rings with Pure, SPARK_Mode is

   type Index is mod 2 ** 32;

   Minimum_Size : constant := 4_096;
   Maximum_Size : constant := 1_048_576;
   subtype Ring_Size is Positive range Minimum_Size .. Maximum_Size with
     Static_Predicate =>
       Ring_Size in 4_096 | 8_192 | 16_384 | 32_768 | 65_536 |
                    131_072 | 262_144 | 524_288 | 1_048_576;

   function Valid_Size (Size : Unsigned_64) return Boolean is
     (Size in 4_096 | 8_192 | 16_384 | 32_768 | 65_536 | 131_072 |
              262_144 | 524_288 | 1_048_576);

   --  How far To is ahead of From, as an integer.
   type Count is range 0 .. 2 ** 32 - 1;
   function Distance (From, To : Index) return Count is
     (Count (To - From));

   type Bytes is array (Natural range <>) of Unsigned_8;

   --  Where Value falls in a ring of Size bytes.
   function Position (Value : Index; Size : Ring_Size) return Natural
   with Post => Position'Result < Size;

   ------------------------------------------------------------------------
   --  The producing side: it writes into free space and commits; the
   --  peer consumes. The peer's consumed index is Produced - Fill.
   ------------------------------------------------------------------------
   type Producer is record
      Size     : Ring_Size := Minimum_Size;
      Produced : Index := 0;     --  ours
      Fill     : Natural := 0;   --  committed, not yet released by the peer
   end record;

   function Valid (P : Producer) return Boolean is (P.Fill <= P.Size);

   function Space (P : Producer) return Natural is (P.Size - P.Fill)
   with Pre => Valid (P);

   --  The peer's consumed index, as last accepted.
   function Consumed (P : Producer) return Index is
     (P.Produced - Index (P.Fill))
   with Pre => Valid (P);

   --  How many bytes the peer's consumed index Value would release.
   function Released (P : Producer; Value : Index) return Count is
     (Distance (Consumed (P), Value))
   with Pre => Valid (P);

   function New_Producer
     (Size : Ring_Size; Origin : Index := 0) return Producer
   is ((Size => Size, Produced => Origin, Fill => 0))
   with Post => Valid (New_Producer'Result);

   --  Take the peer's consumed index if it lies between the last one
   --  accepted and what has been produced; the bytes between are free.
   procedure Accept_Consumed
     (P : in out Producer; Value : Index; OK : out Boolean)
   with
     Pre  => Valid (P),
     Post => Valid (P) and then
             OK = (Released (P'Old, Value) <= Count (P'Old.Fill)) and then
             (if OK then
                P = (P'Old with delta
                       Fill => P'Old.Fill - Natural (Released (P'Old, Value)))
              else P = P'Old);

   --  The free space: First_Length bytes at First, then Second_Length
   --  bytes at the start of the ring.
   procedure Free_Slices
     (P : Producer; First, First_Length, Second_Length : out Natural)
   with
     Pre  => Valid (P),
     Post => First = Position (P.Produced, P.Size) and then
             First_Length <= P.Size - First and then
             Second_Length <= First and then
             First_Length + Second_Length = Space (P) and then
             (if Second_Length > 0 then First_Length = P.Size - First);

   --  Publish N bytes already written into the free space.
   procedure Commit (P : in out Producer; N : Natural)
   with
     Pre  => Valid (P) and then N <= Space (P),
     Post => Valid (P) and then
             P = (P'Old with delta Produced => P'Old.Produced + Index (N),
                                   Fill     => P'Old.Fill + N);

   --  Copy as much of Data as fits and commit it.
   procedure Write
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Written : out Natural)
   with
     Pre  => Valid (P) and then Ring'First = 0 and then
             Ring'Last = P.Size - 1 and then Data'Last < Natural'Last,
     Post => Valid (P) and then
             Written = Natural'Min (Data'Length, Space (P'Old)) and then
             P = (P'Old with delta
                    Produced => P'Old.Produced + Index (Written),
                    Fill     => P'Old.Fill + Written);

   ------------------------------------------------------------------------
   --  The consuming side: the peer produces; it reads and releases. The
   --  peer's produced index is Consumed + Available.
   ------------------------------------------------------------------------
   type Consumer is record
      Size      : Ring_Size := Minimum_Size;
      Consumed  : Index := 0;     --  ours
      Available : Natural := 0;   --  produced by the peer, not yet read
   end record;

   function Valid (C : Consumer) return Boolean is
     (C.Available <= C.Size);

   --  The peer's produced index, as last accepted.
   function Produced (C : Consumer) return Index is
     (C.Consumed + Index (C.Available))
   with Pre => Valid (C);

   function New_Consumer
     (Size : Ring_Size; Origin : Index := 0) return Consumer
   is ((Size => Size, Consumed => Origin, Available => 0))
   with Post => Valid (New_Consumer'Result);

   --  Take the peer's produced index if it does not go back past the
   --  last one accepted and does not overfill the ring.
   procedure Accept_Produced
     (C : in out Consumer; Value : Index; OK : out Boolean)
   with
     Pre  => Valid (C),
     Post => Valid (C) and then
             OK = (Distance (C'Old.Consumed, Value) <= Count (C'Old.Size)
                   and then Distance (C'Old.Consumed, Value) >=
                            Count (C'Old.Available)) and then
             (if OK then
                C = (C'Old with delta
                       Available =>
                         Natural (Distance (C'Old.Consumed, Value)))
              else C = C'Old);

   --  The readable bytes: First_Length at First, then Second_Length at
   --  the start of the ring.
   procedure Data_Slices
     (C : Consumer; First, First_Length, Second_Length : out Natural)
   with
     Pre  => Valid (C),
     Post => First = Position (C.Consumed, C.Size) and then
             First_Length <= C.Size - First and then
             Second_Length <= First and then
             First_Length + Second_Length = C.Available and then
             (if Second_Length > 0 then First_Length = C.Size - First);

   --  Release N bytes that have been read.
   procedure Consume (C : in out Consumer; N : Natural)
   with
     Pre  => Valid (C) and then N <= C.Available,
     Post => Valid (C) and then
             C = (C'Old with delta Consumed  => C'Old.Consumed + Index (N),
                                   Available => C'Old.Available - N);

   --  Copy up to Data'Length bytes out and release them.
   procedure Read
     (C : in out Consumer; Ring : Bytes; Data : in out Bytes;
      Copied : out Natural)
   with
     Pre  => Valid (C) and then Ring'First = 0 and then
             Ring'Last = C.Size - 1 and then Data'Last < Natural'Last,
     Post => Valid (C) and then
             Copied = Natural'Min (Data'Length, C'Old.Available) and then
             C = (C'Old with delta
                    Consumed  => C'Old.Consumed + Index (Copied),
                    Available => C'Old.Available - Copied);

end CuBit.Channel_Rings;
