------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Whether Desktop answers a display-capture request now (REMOTE-001
--  "Remote screenshots"), as an explicit state machine in the house pattern
--  of Wifi_MLME and Desktop_Status_Machines: Step (State, Event, out
--  Decision), no I/O. Desktop_Capture carries the decision out.
--
--  @description
--  The display-capture grant makes capture possible; this decides whether
--  it is allowed. The person at the device holds the switch ("Allow screen
--  capture", the status panel's Remote access section):
--  - off at start, and off again whenever the holder's session ends or a
--    new host pairs (the switch is for the paired session, not forever);
--  - it can only be turned on while a session is announced (the switch is
--    shown only then);
--  - a capture needs the switch on and at least Minimum_Interval_Ms since
--    the previous capture (a named rate limit: single frames, no stream);
--  - each capture shows the Deskbar's "screen captured" mark for Mark_Ms.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Desktop_Capture_Policy with SPARK_Mode, Pure is
   --  The rate limit: at most one frame per this many milliseconds.
   Minimum_Interval_Ms : constant := 1_000;
   --  How long the Deskbar shows that the screen was captured.
   Mark_Ms : constant := 4_000;

   type Session is (No_Session, Listening, Paired);

   type State is record
      Holder   : Session := No_Session;
      Allowed  : Boolean := False;       --  the person's switch
      Captured : Boolean := False;       --  a capture happened (Last_Ms valid)
      Last_Ms  : Unsigned_64 := 0;       --  when the last capture happened
      Marked   : Boolean := False;       --  the Deskbar mark is shown
      Mark_End : Unsigned_64 := 0;       --  until then
   end record;
   Initial : constant State := (others => <>);

   --  The switch can be on only while a session is announced.
   function Consistent (S : State) return Boolean is
     (if S.Allowed then S.Holder /= No_Session);

   type Event_Kind is
     (Announce,      --  the holder says what its session is
      Set_Allowed,   --  the person flipped the switch
      Request,       --  the holder asks for a frame
      Tick);         --  time passed (the mark may end)
   type Event (Kind : Event_Kind := Tick) is record
      Now : Unsigned_64 := 0;
      case Kind is
         when Announce =>
            Holder : Session := No_Session;
            --  A host paired that was not paired before (a new pairing).
            New_Pairing : Boolean := False;
         when Set_Allowed =>
            Allowed : Boolean := False;
         when Request | Tick =>
            null;
      end case;
   end record;

   type Decision is
     (None,              --  nothing to do (Announce, Set_Allowed, Tick)
      Capture,           --  copy the frame now
      Refuse_Off,        --  the switch is off
      Refuse_Rate,       --  too soon after the last capture
      Refuse_No_Session);

   --  Whether Now is far enough after the last capture.
   function Rate_Allows (S : State; Now : Unsigned_64) return Boolean is
     (not S.Captured or else (Now >= S.Last_Ms and then Now - S.Last_Ms >= Minimum_Interval_Ms));

   function Mark_End_After (Now : Unsigned_64) return Unsigned_64 is
     (if Now > Unsigned_64'Last - Mark_Ms then Unsigned_64'Last else Now + Mark_Ms);

   procedure Step (S : in out State; E : Event; D : out Decision)
   with Pre  => Consistent (S),
        Post => Consistent (S)
     --  Only a request decides anything, and it captures exactly when the
     --  switch is on and the rate allows it.
     and then (if E.Kind /= Request then D = None)
     and then (if E.Kind = Request then
                 (if S'Old.Holder = No_Session then D = Refuse_No_Session
                  elsif not S'Old.Allowed then D = Refuse_Off
                  elsif not Rate_Allows (S'Old, E.Now) then D = Refuse_Rate
                  else D = Capture))
     and then (if D = Capture then
                 S.Captured and then S.Last_Ms = E.Now and then S.Marked
                 and then S.Mark_End = Mark_End_After (E.Now))
     --  Nothing but the person turns the switch on, and only for a session.
     and then (if S.Allowed and then not S'Old.Allowed then
                 E.Kind = Set_Allowed and then E.Allowed and then S.Holder /= No_Session)
     --  A session that ends, or a new pairing, turns it off.
     and then (if E.Kind = Announce and then (E.Holder = No_Session or else E.New_Pairing)
               then not S.Allowed)
     and then (if E.Kind = Set_Allowed and then not E.Allowed then not S.Allowed)
     --  A refused request changes nothing.
     and then (if D in Refuse_Off | Refuse_Rate | Refuse_No_Session then S = S'Old)
     --  The mark ends at its time, and only a capture starts it.
     and then (if S.Marked and then not S'Old.Marked then D = Capture)
     and then (if E.Kind = Tick and then S'Old.Marked and then E.Now >= S'Old.Mark_End
               then not S.Marked);
end Desktop_Capture_Policy;
