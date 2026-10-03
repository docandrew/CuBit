with CCL.Streams;
with CuBit.Slot_Rings;
with Interfaces;

--  A session's streams (docs/ccl-streams.md): each a ring of its newest
--  elements with its arrival and loss counts. The session host owns the
--  table; evaluations only read views of it through CCL.Streams. A handle
--  names a slot and the generation that opened it, so a closed stream's
--  handle never reads its slot's next stream.
--
--  Phase 1 holds Integer elements, from timers (timer.every). Each
--  stream's history is a CuBit.Slot_Rings ring (docs/async-rings.md), the
--  proved generic the system's shared-memory data planes use. The table is
--  its producer and, when the ring is full, consumes the oldest element
--  itself and counts it lost. A source delivering through a shared ring
--  will be this ring's peer instead.
package CCL_Stream_Table with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   MAX_STREAMS : constant := 16;
   --  2 ** 8 = 256 elements of history per stream; a window shows at most
   --  CCL.Streams.Maximum_Window of the newest.
   HISTORY_BITS : constant := 8;
   package History is new CuBit.Slot_Rings (Interfaces.Integer_64, HISTORY_BITS);
   CAPACITY : constant Positive := History.Slots;

   --  A timer's period, in milliseconds: at most a hundred ticks a second
   --  (a frame's worth), at least one an hour.
   MIN_PERIOD_MS : constant := 10;
   MAX_PERIOD_MS : constant := 3_600_000;
   subtype Period_Ms is Interfaces.Unsigned_64 range MIN_PERIOD_MS .. MAX_PERIOD_MS;

   type Table is private;

   --  A timer stream: from Now + Period, every Period, the tick's time in
   --  monotonic milliseconds. No_Handle when every slot is open.
   procedure Open_Timer
     (Item : in out Table; Period : Period_Ms; Now : Interfaces.Unsigned_64;
      Handle : out CCL.Streams.Handle);

   --  Deliver what is due by Now. Changed: at least one element arrived.
   --  A timer more than a ring behind skips ahead instead of replaying.
   procedure Pump (Item : in out Table; Now : Interfaces.Unsigned_64; Changed : out Boolean);

   --  Answer a view: elements as an image of Integer or List<Integer>
   --  (the evaluation checks it against its own type).
   procedure Read
     (Item : Table; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply);

   procedure Close (Item : in out Table; Handle : CCL.Streams.Handle);
   --  Close every open stream that Held does not claim.
   generic
      with function Held (Handle : CCL.Streams.Handle) return Boolean;
   procedure Retain (Item : in out Table);
   --  Close every stream (the session was reset).
   procedure Clear (Item : in out Table);

   function Open_Count (Item : Table) return Natural
     with Post => Open_Count'Result <= MAX_STREAMS;
   --  When the next element is due (monotonic milliseconds), or
   --  Unsigned_64'Last when nothing is open.
   function Next_Due (Item : Table) return Interfaces.Unsigned_64;

private
   subtype Slot_Index is Positive range 1 .. MAX_STREAMS;
   --  Generations count opens of a slot; a handle is
   --  Generation * MAX_STREAMS + Slot.
   MAX_GENERATION : constant := (CCL.Streams.Maximum_Handle - MAX_STREAMS) / MAX_STREAMS;
   subtype Generation is Natural range 0 .. MAX_GENERATION;

   type Stream is record
      Open : Boolean := False;
      Opened : Generation := 0;
      Period : Period_Ms := MIN_PERIOD_MS;
      Due : Interfaces.Unsigned_64 := 0;
      --  The history: Producer.Fill elements, the newest at Produced - 1.
      Producer : History.Producer := History.New_Producer;
      Elements : History.Ring := [others => 0];
      Arrived : CCL.Streams.Element_Total := 0;
      Lost : CCL.Streams.Element_Total := 0;
   end record;
   type Stream_Array is array (Slot_Index) of Stream;

   type Table is record
      Streams : Stream_Array;
   end record;
end CCL_Stream_Table;
