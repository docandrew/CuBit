------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Room in a client's event ring (docs/filesystem-protocol-v2.md step 4):
--  whether a change event goes in, or a Rescan_Needed record stands in for
--  it, so that no event is ever lost without the client being told.
--
--  @description
--  The ring (CuBit.Datagram_Rings records) always keeps room for one
--  Rescan_Needed record per Watching watch: Free >= Reserve_Each * Normal,
--  the invariant every decision here keeps (Holds). A record may need a pad
--  before it at the ring's end, so its cost is counted as Worst: twice its
--  ring bytes.
--  A watch's states:
--    Watching: its events go in while they leave the reserve intact; when
--      one would not, its Rescan_Needed goes in instead (Rescan_Posted).
--    Rescan_Posted: its events are covered by that record until the client
--      has read it; then the watch is Watching again if the reserve allows,
--      else the next event makes it Rescan_Owed.
--    Rescan_Owed: events were dropped after the client read the last
--      Rescan_Needed: another one goes in as soon as there is room.
--    End_Owed: the watch ended (Queue_Unwatch, or its folder went) while
--      its reserve was spent: its Watch_Ended goes in as soon as there is
--      room. No events meanwhile.
--    Ended: its Watch_Ended is in the ring, the last record of the watch.
--      The service reuses the number once the client has read past it
--      (Reached), so no record of an old watch is read as a new one's.
--  Proved (tests/filesystem-events, level 2): every decision keeps Holds.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
with CuBit.Filesystem_Events;

package Watch_Reserve with SPARK_Mode is

   package DR renames CuBit.Datagram_Rings;
   package FE renames CuBit.Filesystem_Events;

   --  The ring bytes a record of Payload bytes may take, a pad included.
   function Worst (Payload : FE.Record_Length) return Positive is
     (2 * DR.Record_Bytes (Payload));
   Reserve_Each : constant Positive := Worst (FE.Header_Bytes);
   --  The most a ring's free bytes can be (the largest channel ring).
   Maximum_Free : constant := 128 * 4_096;
   subtype Free_Bytes is Natural range 0 .. Maximum_Free;
   subtype Normal_Count is Natural range 0 .. FE.Maximum_Watches;

   function Holds (Free : Free_Bytes; Normal : Normal_Count) return Boolean is
     (Free >= Reserve_Each * Normal);

   type Watch_State is (Unused, Watching, Rescan_Posted, Rescan_Owed, End_Owed, Ended);
   --  A watch that still sees changes.
   subtype Live_State is Watch_State range Watching .. Rescan_Owed;

   --  A new watch is admitted (Watching) only with room for its reserve.
   function May_Admit (Free : Free_Bytes; Normal : Normal_Count) return Boolean is
     (Normal < FE.Maximum_Watches and then Free >= Reserve_Each * (Normal + 1));

   type Decision is
     (Put_Event,    --  the event goes in
      Put_Rescan,   --  its Rescan_Needed goes in instead (now Rescan_Posted)
      Drop);        --  covered by a Rescan_Needed posted or owed

   --  An event of Payload bytes for a watch in State; Read: the client has
   --  read the watch's last Rescan_Needed (Rescan_Posted only).
   procedure Decide
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Payload : FE.Record_Length; Read : Boolean; Result : out Decision)
   with Pre  => State in Live_State and then Holds (Free, Normal)
                and then (if State = Watching then Normal > 0),
        Post => State in Live_State and then
                (case Result is
                   when Put_Event  => State = Watching and then Normal > 0
                                      and then Free >= Worst (Payload)
                                      and then Holds (Free - Worst (Payload), Normal),
                   when Put_Rescan => State = Rescan_Posted
                                      and then Free >= Reserve_Each
                                      and then Holds (Free - Reserve_Each, Normal),
                   when Drop       => State in Rescan_Posted | Rescan_Owed
                                      and then Holds (Free, Normal));

   --  Between events (each service pass): a Rescan_Posted watch whose
   --  record was read becomes Watching if the reserve allows; a Rescan_Owed
   --  one gets its Rescan_Needed (now Rescan_Posted) and an End_Owed one its
   --  Watch_Ended (now Ended) if there is room (Put).
   procedure Settle
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Read : Boolean; Put : out Boolean)
   with Pre  => Holds (Free, Normal) and then (if State = Watching then Normal > 0),
        Post => (if Put then
                   ((State = Rescan_Posted and then State'Old = Rescan_Owed) or else
                    (State = Ended and then State'Old = End_Owed))
                   and then Normal = Normal'Old and then Free >= Reserve_Each
                   and then Holds (Free - Reserve_Each, Normal)
                 else Holds (Free, Normal))
                and then (if State = Watching then Normal > 0)
                and then (State = Unused) = (State'Old = Unused)
                and then (State in Live_State) = (State'Old in Live_State);

   --  The watch ends (Queue_Unwatch, or its folder went): its Watch_Ended
   --  goes in now (Put; Ended) or as soon as there is room (End_Owed). A
   --  Watching watch's reserve always has room for it.
   procedure End_Watch
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Put : out Boolean)
   with Pre  => State in Live_State and then Holds (Free, Normal)
                and then (if State = Watching then Normal > 0),
        Post => State in End_Owed | Ended and then
                (if State'Old = Watching then Put) and then
                (if Put then State = Ended and then Free >= Reserve_Each
                   and then Holds (Free - Reserve_Each, Normal)
                 else State = End_Owed and then Holds (Free, Normal));

   --  A change the service cannot name (a written file whose path it did
   --  not keep): the watch rescans. A Watching watch's Rescan_Needed goes in
   --  (Put); one whose record was read owes another.
   procedure Force_Rescan
     (State : in out Watch_State; Normal : in out Normal_Count; Free : Free_Bytes;
      Read : Boolean; Put : out Boolean)
   with Pre  => State in Live_State and then Holds (Free, Normal)
                and then (if State = Watching then Normal > 0),
        Post => (if Put then State = Rescan_Posted and then State'Old = Watching
                   and then Free >= Reserve_Each and then Holds (Free - Reserve_Each, Normal)
                 else Holds (Free, Normal))
                and then (if State = Watching then Normal > 0)
                and then State in Live_State;

   --  Whether the client has read past Mark (the producer's index after a
   --  Rescan_Needed or Watch_Ended record), given its Consumed index and the ring's Fill.
   function Reached (Mark, Consumed : CuBit.Channel_Rings.Index; Fill : Natural) return Boolean is
     (CuBit.Channel_Rings."=" (Mark, Consumed) or else
      CuBit.Channel_Rings.">" (CuBit.Channel_Rings."-" (Mark, Consumed),
                               CuBit.Channel_Rings.Index (Natural'Min (Fill, Maximum_Free))));

end Watch_Reserve;
