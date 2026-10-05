with CCL.Objects;
with CCL.Streams;
with CuBit.Slot_Rings;
with Interfaces;

--  A session's streams (docs/ccl-streams.md): each a ring of its newest
--  elements with its arrival and loss counts. The session host owns the
--  table; evaluations only read views of it through CCL.Streams. A handle
--  names a slot and the generation that opened it, so a closed stream's
--  handle never reads its slot's next stream.
--
--  Timers (timer.every) deliver Integer elements by time. Outlet streams
--  are fed by the host from a launched program's connectors
--  (docs/ccl-launch-parameters.md, "Connectors, not stdio"): Integer or text
--  elements, each line a String; a one-shot connector ends after its value. Each
--  stream's history is a CuBit.Slot_Rings ring (docs/async-rings.md), the
--  proved generic the system's shared-memory data planes use. The table is
--  its producer and, when the ring is full, consumes the oldest element
--  itself and counts it lost. A source delivering through a shared ring
--  will be this ring's peer instead.
package CCL_Stream_Table with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   MAX_STREAMS : constant := 48;
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

   --  A outlet stream's elements; a text element is one line, cut to
   --  MAX_LINE characters, and TEXT_HISTORY of the newest are kept.
   type Element_Kind is (Integer_Elements, Text_Elements, Task_Result);
   TEXT_HISTORY : constant := 64;
   MAX_LINE : constant := 200;

   --  A outlet stream, fed by Push_*. No_Handle when every slot is open. A
   --  pinned stream survives Retain until Unpin: a launched program's connectors
   --  are open before any name binds them.
   procedure Open_Outlet
     (Item : in out Table; Kind : Element_Kind; Handle : out CCL.Streams.Handle;
      Pinned : Boolean := False);
   procedure Unpin (Item : in out Table; Handle : CCL.Streams.Handle);

   --  Tasks (Task<T>, docs/control-language.md): an entry that completes
   --  once with a result, read by (wait t). The result is an image of T,
   --  as a view's elements are: kept without its schema key, since the
   --  reader checks its structure against the type it waits for. At most
   --  MAX_RESULTS completed tasks keep their results at a time.
   MAX_RESULTS : constant := 16;
   procedure Open_Task
     (Item : in out Table; Handle : out CCL.Streams.Handle; Pinned : Boolean := False);
   --  Completed False when Handle names no open, pending task, or every
   --  result slot is in use.
   procedure Complete_Task
     (Item : in out Table; Handle : CCL.Streams.Handle; Result : CCL.Objects.Image;
      Completed : out Boolean);
   function Task_Done (Item : Table; Handle : CCL.Streams.Handle) return Boolean;
   --  Append to a outlet stream of that kind; Pushed False when Handle names
   --  no such open stream, or it has ended.
   procedure Push_Integer
     (Item : in out Table; Handle : CCL.Streams.Handle; Value : Interfaces.Integer_64;
      Pushed : out Boolean);
   procedure Push_Text
     (Item : in out Table; Handle : CCL.Streams.Handle; Line : String; Pushed : out Boolean);
   --  No more elements will arrive (a one-shot connector delivered, or the
   --  program ended); what arrived stays readable.
   procedure End_Stream (Item : in out Table; Handle : CCL.Streams.Handle);
   function Ended (Item : Table; Handle : CCL.Streams.Handle) return Boolean;

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

   subtype Line_Length is Natural range 0 .. MAX_LINE;
   type Line_Array is array (1 .. TEXT_HISTORY) of String (1 .. MAX_LINE);
   type Length_Array is array (1 .. TEXT_HISTORY) of Line_Length;
   subtype Line_Slot is Positive range 1 .. TEXT_HISTORY;
   subtype Line_Fill is Natural range 0 .. TEXT_HISTORY;

   type Stream is record
      Open : Boolean := False;
      Opened : Generation := 0;
      --  A timer delivers by time; a outlet stream by Push_*.
      Timer : Boolean := False;
      Kind : Element_Kind := Integer_Elements;
      Finished : Boolean := False;
      Pinned : Boolean := False;
      --  A completed task: its result's slot in Results (0: none).
      Result_Slot : Natural range 0 .. MAX_RESULTS := 0;
      --  Text history: Text_Fill lines, the newest at Text_Newest.
      Lines : Line_Array := [others => [others => ' ']];
      Lengths : Length_Array := [others => 0];
      Text_Newest : Line_Slot := TEXT_HISTORY;
      Text_Fill : Line_Fill := 0;
      Period : Period_Ms := MIN_PERIOD_MS;
      Due : Interfaces.Unsigned_64 := 0;
      --  The history: Producer.Fill elements, the newest at Produced - 1.
      Producer : History.Producer := History.New_Producer;
      Elements : History.Ring := [others => 0];
      Arrived : CCL.Streams.Element_Total := 0;
      Lost : CCL.Streams.Element_Total := 0;
   end record;
   type Stream_Array is array (Slot_Index) of Stream;

   type Result_Array is array (1 .. MAX_RESULTS) of CCL.Objects.Image;
   type Result_Used is array (1 .. MAX_RESULTS) of Boolean;
   type Table is record
      Streams : Stream_Array;
      Results : Result_Array;
      Used : Result_Used := [others => False];
   end record;
end CCL_Stream_Table;
