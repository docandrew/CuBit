------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  An input source (DEV-002, IPC-009): the driver side of one input device's
--  source ring to Desktop. A driver decodes hardware into its class's
--  reports and calls Publish; that is its whole input surface. A new input
--  driver only instantiates this.
--
--  @description
--  The instance owns:
--  * the class's retention (Pending: Keyboard_Pending, Pointer_Pending,
--    Absolute_Pending) with its coalescing rules, which merge only where
--    CuBit.Input's delivery class allows it and never across a transition;
--  * the acquisition stamps (LAT-002, Input_Stamps' encoding);
--  * the source ring (CuBit.Input_Rings) and its establishment: devmgr's
--    endpoint to Desktop, badged for input sources (Binding_Label), then a
--    channel opened over it. One ring per source device; the ring is the
--    source's identity at Desktop, so a source cannot name itself;
--  * back pressure: a full ring keeps reports retained here and arms for
--    room (Desktop rings back when it takes). A retention overflow is
--    marked on the report that follows (RESYNCHRONIZE) and counted in the
--    ring's overflow word: Desktop sees every loss, nothing is dropped
--    silently;
--  * the wake: Desktop is rung only when it armed its waiting word, once
--    per arming (CuBit.Channels.Signal, the channel doorbell; shared
--    futexes replace it in Wake_Desktop alone, IPC-010).
--  Until the first ring opens, reports wait in the retention (a device
--  used as it is plugged in loses nothing). When a ring ends (Desktop went
--  away) what it had not taken is dropped: the next Desktop has not seen
--  the state those reports change, so its ring starts a fresh stream.
--
--  The driver's loop calls Service after every wake (it looks again for
--  Desktop, takes back room, pushes what was retained) and sleeps no later
--  than Next_Deadline; it hands every kernel event to Note_Event.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Handles;
with CuBit.Input;
with CuBit.Input_Rings;
with Input_Pending;

generic
   --  The class's retention and the driver's decoded unit.
   type Pending is private;
   type Report is private;
   Device : CuBit.Input.Device_Class;
   --  Retain R, acquired at Observed_Us (CuBit.Monotonic; Unsigned_64'Last
   --  unstamped). Lost: retention overflowed (the next report is marked).
   with procedure Append
     (S : in out Pending; R : Report; Observed_Us : Unsigned_64; Lost : out Boolean);
   with function Count (S : Pending) return Input_Pending.Count_Type;
   with function Element
     (S : Pending; Offset : Input_Pending.Offset_Type) return Input_Pending.Item;
   with procedure Acknowledge (S : in out Pending);
   with procedure Reset (S : in out Pending);
   --  The button bits a report's snapshot carries (zero for keyboards).
   with function Buttons (Item : Input_Pending.Item) return Unsigned_64;
   --  The device itself lost reports (virtio-input's SYN_DROPPED): the
   --  next report is marked for recovery. Classes without it: null.
   with procedure Mark_Lost (S : in out Pending) is null;
package CuBit.Input_Source is

   type Source is limited private;

   --  Retain R and push what the ring has room for.
   procedure Publish (S : in out Source; R : Report; Observed_Us : Unsigned_64);

   --  The device said it lost reports: the next one is marked for recovery.
   procedure Device_Lost (S : in out Source);

   --  After every wake: establish the ring when it is due (through Manager,
   --  the driver's endpoint to devmgr), push what was retained.
   procedure Service (S : in out Source; Manager : CuBit.Handles.Endpoint);

   --  When the driver must call Service again at the latest (monotonic
   --  milliseconds), Unsigned_64'Last when only a wake needs it.
   function Next_Deadline (S : Source) return Unsigned_64;

   --  The device went away: its ring ends (Desktop forgets the source). A
   --  device that comes back is a new source: Service opens a new ring.
   procedure Withdraw (S : in out Source);

   --  A kernel event the driver took: Ends when it ended this source's ring
   --  (Desktop went away); the ring is reestablished from Service.
   procedure Note_Event
     (S : in out Source; Event : CuBit.Control_Events.Event; Ends : out Boolean);

   function Established (S : Source) return Boolean;
   --  Reports pushed into rings, and retention overflows, since start.
   function Published (S : Source) return Unsigned_64;
   function Overflows (S : Source) return Unsigned_64;

   --  How long one attempt to open a ring may take, and how often a
   --  source without one looks again.
   Open_Limit_Ms : constant := 50;
   Establish_Retry_Ms : constant := 100;

private
   type Source is limited record
      Retained : Pending;
      Channel : CuBit.Channels.Channel;
      Ring : CuBit.Input_Rings.Channels.Producer;
      Open : Boolean := False;
      Desktop : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Retry_At : Unsigned_64 := 0;
      Room_Armed : Boolean := False;
      Pushed, Lost : Unsigned_64 := 0;
   end record;
end CuBit.Input_Source;
