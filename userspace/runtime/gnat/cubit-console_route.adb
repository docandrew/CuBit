pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with CuBit.Console_Lines;
with CuBit.Futex_Locks;
with CuBit.Logging;
with CuBit.Messages;
with CuBit.Handles.Startup;
with CuBit.Introductions;
with CuBit.Waits;

package body CuBit.Console_Route is
   package Lines renames CuBit.Console_Lines;
   use type Logs.Severity;

   type Route_State is (Off, Awaiting, Bound);
   State : Route_State := Off;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Awaiting_Grant;
   Framer : Lines.Framer;
   Guard  : CuBit.Futex_Locks.Mutex;
   --  This thread is inside the route (holding Guard): a debugPrint it
   --  causes now is counted, not published (it would wait on Guard).
   Inside : Boolean := False with Thread_Local_Storage;

   --  Records lost here (the ring's own sheds are the Writer's), and those
   --  of them lost while queued, reported as a gap when the queue drains.
   Losses, Queue_Losses : Unsigned_64 := 0;

   --  The queue: encoded records, oldest first.
   type Node;
   type Node_Access is access Node;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   type Node (Used : Logs.Wire_Count) is record
      Next  : Node_Access;
      Bytes : Byte_Array (1 .. Used);
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);
   Head, Tail : Node_Access;
   Queued_Bytes : Natural := 0;

   procedure Lose (Queued : Boolean := False);
   procedure Enqueue (Value : Logs.Log_Record);
   procedure Put (Value : Logs.Log_Record; Accepted : out Boolean);
   procedure Drain_Queue;
   procedure Publish_Line;
   procedure Sink (Text : String);
   --  CuBit.Logging's announcements through the route (Set_Route).
   procedure Logging_Emit (Value : Logs.Log_Record; Accepted : out Boolean);
   procedure Logging_Flush (Drained : out Boolean);
   function Logging_Minimum return Logs.Severity;

   procedure Lose (Queued : Boolean := False) is
   begin
      if Losses < Unsigned_64'Last then
         Losses := Losses + 1;
      end if;
      if Queued and then Queue_Losses < Unsigned_64'Last then
         Queue_Losses := Queue_Losses + 1;
      end if;
   end Lose;

   function Mark (Value : Logs.Log_Record) return Logs.Decoded is
     (Logs.With_Field (Value, Lines.Console_Field, Logs.Truth, 1));

   --  Under Guard.
   procedure Enqueue (Value : Logs.Log_Record) is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Item : Node_Access;
   begin
      Logs.Encode (Value, Bytes, Used);
      if Used > Maximum_Queued_Bytes - Queued_Bytes then
         Lose (Queued => True);
         return;
      end if;
      --  CuAlloc may refuse (memory exhausted): the record is lost and
      --  counted, the caller goes on, and the lock is still released.
      begin
         Item := new Node (Used);
      exception
         when Storage_Error =>
            Lose (Queued => True);
            return;
      end;
      for I in 1 .. Used loop
         Item.Bytes (I) := Bytes (I);
      end loop;
      if Tail = null then
         Head := Item;
      else
         Tail.Next := Item;
      end if;
      Tail := Item;
      Queued_Bytes := Queued_Bytes + Used;
   end Enqueue;

   --  Under Guard.
   procedure Put (Value : Logs.Log_Record; Accepted : out Boolean) is
   begin
      Accepted := False;
      case State is
         when Off =>
            Lose;
         when Awaiting =>
            Enqueue (Value);
            Accepted := True;
         when Bound =>
            CuBit.Logging.Emit (Writer, Value, Accepted);
      end case;
   end Put;

   --  "console: N records lost before logging started (queue full)", made
   --  in a fixed buffer (no secondary stack: netstack and libc use this).
   procedure Gap_Line (Count : Unsigned_64; Made : out Logs.Decoded);
   procedure Gap_Line (Count : Unsigned_64; Made : out Logs.Decoded) is
      Before : constant String := "console: ";
      After : constant String := " records lost before logging started (queue full)";
      Digits_Of_Count : constant := 20;   --  Unsigned_64'Last has 20
      Number : String (1 .. Digits_Of_Count);
      First : Positive range 1 .. Digits_Of_Count := Digits_Of_Count;
      Value : Unsigned_64 := Count;
      Text : Lines.Line_Buffer;
      Used : Natural := 0;
   begin
      for I in reverse Number'Range loop
         Number (I) := Character'Val (Character'Pos ('0') + Natural (Value mod 10));
         First := I;
         Value := Value / 10;
         exit when Value = 0;
      end loop;
      Text (1 .. Before'Length) := Before;
      Used := Before'Length;
      Text (Used + 1 .. Used + (Digits_Of_Count - First + 1)) := Number (First .. Digits_Of_Count);
      Used := Used + (Digits_Of_Count - First + 1);
      Text (Used + 1 .. Used + After'Length) := After;
      Used := Used + After'Length;
      Made := Logs.Make (Text (1 .. Used), Logs.Warning);
   end Gap_Line;

   --  Under Guard: the queue into the ring, then the gap, if any.
   procedure Drain_Queue is
      Item : Node_Access := Head;
      Next : Node_Access;
      Decoded : Logs.Decoded;
      Accepted : Boolean;
      Bytes : Logs.Wire_Buffer := [others => 0];
   begin
      while Item /= null loop
         Next := Item.Next;
         for I in 1 .. Item.Used loop
            Bytes (I) := Item.Bytes (I);
         end loop;
         Decoded := Logs.Decode (Bytes, Item.Used);
         if Decoded.Success
           and then CuBit.Logging.Wanted (Writer, Logs.Level (Decoded.Value))
         then
            CuBit.Logging.Emit (Writer, Decoded.Value, Accepted);
         end if;
         Free (Item);
         Item := Next;
      end loop;
      Head := null;
      Tail := null;
      Queued_Bytes := 0;
      if Queue_Losses > 0 then
         declare
            Gap : Logs.Decoded;
         begin
            Gap_Line (Queue_Losses, Gap);
            if Gap.Success then
               Gap := Logs.With_Field (Gap.Value, "lost", Logs.Unsigned_Integer, Queue_Losses);
            end if;
            if Gap.Success then
               CuBit.Logging.Emit (Writer, Gap.Value, Accepted);
            end if;
         end;
         Queue_Losses := 0;
      end if;
   end Drain_Queue;

   --  Under Guard: the framer's completed line as a Debug console record.
   procedure Publish_Line is
      Text : Lines.Line_Buffer;
      Used : Lines.Line_Length;
      Made : Logs.Decoded;
      Accepted : Boolean;
   begin
      if Lines.Length (Framer) = 0 or else not CuBit.Logging.Wanted (Writer, Logs.Debug) then
         return;
      end if;
      Lines.Line (Framer, Text, Used);
      Lines.Plain (Text (1 .. Used));
      Made := Logs.Make (Text (1 .. Used), Logs.Debug);
      if Made.Success then
         Made := Mark (Made.Value);
      end if;
      if Made.Success then
         Put (Made.Value, Accepted);
      else
         Lose;
      end if;
   end Publish_Line;

   procedure Sink (Text : String) is
      Complete : Boolean;
   begin
      if Inside then
         Lose;
         return;
      end if;
      Inside := True;
      CuBit.Futex_Locks.Lock (Guard);
      for C of Text loop
         Lines.Feed (Framer, C, Complete);
         if Complete then
            Publish_Line;
            Lines.Clear (Framer);
         end if;
      end loop;
      CuBit.Futex_Locks.Unlock (Guard);
      Inside := False;
   end Sink;

   procedure Route_To (Target : Logstore_Handles.Endpoint) is
   begin
      Route_Awaiting;
      Attach (Target);
   end Route_To;

   procedure Route_Awaiting is
   begin
      if State = Off then
         State := Awaiting;
         CuBit.Messages.Set_Console_Sink (Sink'Access);
         --  One channel per program: CuBit.Logging's announcements (and
         --  last words) go through this one, never a second open, which
         --  logstore would take as this publisher replacing the route's.
         CuBit.Logging.Mark_Route (Writer);
         CuBit.Logging.Set_Route
           (Logging_Emit'Access, Logging_Flush'Access, Logging_Minimum'Access);
      end if;
   end Route_Awaiting;

   procedure Attach (Target : Logstore_Handles.Endpoint) is
      Binding : Boolean;
      Opened : Boolean;
   begin
      Inside := True;
      CuBit.Futex_Locks.Lock (Guard);
      Binding := State = Awaiting;
      if Binding then
         --  Each attempt takes the caller's endpoint of now (an earlier,
         --  refused attempt's may have been closed since).
         CuBit.Logging.Retarget (Writer, Target);
      end if;
      CuBit.Futex_Locks.Unlock (Guard);
      if Binding then
         --  Opening the channel is a call to logstore: made outside the
         --  lock, so the program's other threads go on printing (queued,
         --  still Awaiting) and serving meanwhile. logstore may be calling
         --  this very program (filesystem.svc: its persistent log) and need
         --  an answer before it takes the open (IPC DAG). Only this thread
         --  touches Writer until State is Bound.
         CuBit.Logging.Open (Writer, Opened);
         CuBit.Futex_Locks.Lock (Guard);
         if Opened then
            State := Bound;
            Drain_Queue;
         end if;
         --  Otherwise refused (logstore's ring pages: memory exhausted):
         --  still Awaiting, so the caller may Attach again (the open is
         --  tried again); nothing queued is lost.
         CuBit.Futex_Locks.Unlock (Guard);
      end if;
      Inside := False;
   end Attach;

   function Routed return Boolean is (State /= Off);
   function Awaiting return Boolean is (State = Awaiting);

   --  On the introduction's own thread (CuBit.Introductions).
   procedure Attach_Introduced (Value : CuBit.Handles.Endpoint);
   procedure Attach_Introduced (Value : CuBit.Handles.Endpoint) is
      Ignore : Unsigned_64;
   begin
      for Attempt in 1 .. Attach_Attempts loop
         Attach (Logstore_Handles.Granted (Value));
         exit when State /= Awaiting;
         Ignore := CuBit.Waits.Sleep_For (Attach_Retry_Ms);
      end loop;
   end Attach_Introduced;

   procedure Route_Named (Label : String) is
      Now : constant CuBit.Handles.Startup.Optional_Endpoint :=
        CuBit.Handles.Startup.Optional_Endpoint_Named (Label);
      Started : Boolean;
   begin
      if State /= Off then
         return;
      elsif Now.Present then
         Route_To (Logstore_Handles.Granted (Now.Value));
      elsif CuBit.Introductions.Awaitable (Label) then
         Route_Awaiting;
         CuBit.Introductions.In_Background (Label, Attach_Introduced'Access, Started);
         if not Started then
            Lose;
         end if;
      end if;
   end Route_Named;

   function Wanted (Level : Logs.Severity) return Boolean is
     (State /= Bound or else CuBit.Logging.Wanted (Writer, Level));

   procedure Emit (Value : Logs.Log_Record; Console : Boolean; Accepted : out Boolean) is
      Made : Logs.Decoded := (Success => True, Value => Value);
   begin
      Accepted := False;
      if Inside then
         Lose;
         return;
      end if;
      if Console then
         Made := Mark (Value);
      end if;
      Inside := True;
      CuBit.Futex_Locks.Lock (Guard);
      if Made.Success then
         Put (Made.Value, Accepted);
      else
         Lose;
      end if;
      CuBit.Futex_Locks.Unlock (Guard);
      Inside := False;
   end Emit;

   procedure Flush is
      Drained : Boolean;
   begin
      if Inside then
         return;
      end if;
      Inside := True;
      CuBit.Futex_Locks.Lock (Guard);
      if State = Bound then
         CuBit.Logging.Flush (Writer, Drained);
      end if;
      CuBit.Futex_Locks.Unlock (Guard);
      Inside := False;
   end Flush;

   procedure Logging_Emit (Value : Logs.Log_Record; Accepted : out Boolean) is
   begin
      Emit (Value, Console => False, Accepted => Accepted);
   end Logging_Emit;

   procedure Logging_Flush (Drained : out Boolean) is
   begin
      Drained := False;
      if Inside then
         return;
      end if;
      Inside := True;
      CuBit.Futex_Locks.Lock (Guard);
      if State = Bound then
         CuBit.Logging.Flush (Writer, Drained);
      end if;
      CuBit.Futex_Locks.Unlock (Guard);
      Inside := False;
   end Logging_Flush;

   function Logging_Minimum return Logs.Severity is
     (if State = Bound then CuBit.Logging.Minimum (Writer) else Logs.Trace);

   function Lost return Unsigned_64 is
     (if Unsigned_64'Last - Losses < CuBit.Logging.Dropped (Writer) then Unsigned_64'Last
      else Losses + CuBit.Logging.Dropped (Writer));
   procedure Program_End is
   begin
      if Routed then
         Flush;
      end if;
   end Program_End;

end CuBit.Console_Route;
