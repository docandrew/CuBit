------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's view of one interface's link (docs/net-link.md): what it last
--  took from the driver's link block (CuBit.Net_Link's report and seqlock)
--  and what changed since, as one transition netstack acts on.
--
--  The driver's block is untrusted shared memory. Observe takes a report
--  only between two equal, even generation reads; a report that does not
--  decode counts as Down (Hardware_Fault), so a confused or hostile driver
--  can stop its own interface's traffic and nothing else. Reports coalesce
--  (the report is a level): when more than one was published since the
--  last one taken and the link is Up before and after, the link may have
--  gone down in between, and the transition is Bounced, never Unchanged.
--
--  Proved (tests/net-link-state, level 2): no run-time errors; the
--  postconditions of Observe below (taking, refusing and the meaning of
--  each transition); Usable is exactly "the last report taken is Up".
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Link;

package Link_Tracker with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;
   use type NL.Link_Condition, NL.Generation, NL.Link_Kind, NL.Network_Identity,
            NL.Up_Detail, NL.Link_State;

   type Observation is record
      State : NL.Link_State := NL.Not_Reported_State;
      --  The generation of the last report taken (Never_Reported: none).
      Seen  : NL.Generation := NL.Never_Reported;
   end record;

   function Valid (O : Observation) return Boolean is
     (NL.Normal (O.State) and then not NL.Writing (O.Seen));

   --  Traffic may flow: the last report taken says Up.
   function Usable (O : Observation) return Boolean is (O.State.Condition = NL.Up);

   --  What a pass found.
   type Outcome is
     (Same,            --  no new report since the last one taken
      Unstable,        --  the driver was writing: nothing taken, look again
      Taken,           --  a new report
      Refused);        --  a new generation whose words are no report: Down

   --  What changed, for netstack to act on:
   --  - Went_Up: from Down or Dormant to Up;
   --  - Went_Down: from Up to Down or Dormant;
   --  - Bounced: Up before and after, but it may have been down in between
   --    (more than one report since) or it is now on another network:
   --    act as on Went_Down, then Went_Up;
   --  - Moved: Up before and after on the same network, one report since,
   --    with another BSSID or speed (roaming within an ESS): nothing to
   --    flush, only to tell;
   --  - Inactive_Change: neither before nor after Up (Down <-> Dormant,
   --    another reason);
   --  - Unchanged.
   type Transition is (Unchanged, Went_Up, Went_Down, Bounced, Moved, Inactive_Change);

   type Step is record
      Found   : Outcome := Same;
      Change  : Transition := Unchanged;
      --  Reports published since the last one taken (when Found = Taken or
      --  Refused).
      Reports : NL.Generation := 0;
   end record;

   --  Leaves traffic stopped: what Went_Down and the first half of Bounced
   --  do (stop transmit, routes unusable, neighbors flushed).
   function Stops (T : Transition) return Boolean is (T in Went_Down | Bounced);
   --  Starts traffic: re-detect duplicates, announce, tell netmgr.
   function Starts (T : Transition) return Boolean is (T in Went_Up | Bounced);

   --  Same ESS (Ethernet links are one network).
   function Same_Network (A, B : NL.Up_Detail) return Boolean is
     (A.Kind = B.Kind and then
      (if A.Kind = NL.Wireless and then B.Kind = NL.Wireless
       then A.Network = B.Network else True));

   --  The transition from Old to New, Reports reports apart.
   function Change_Of (Old, New_State : NL.Link_State; Reports : NL.Generation)
     return Transition
   is (if Old.Condition = NL.Up and then New_State.Condition = NL.Up then
         (if Reports > 1 or else not Same_Network (Old.Detail, New_State.Detail) then Bounced
          elsif Old.Detail /= New_State.Detail then Moved
          else Unchanged)
       elsif New_State.Condition = NL.Up then Went_Up
       elsif Old.Condition = NL.Up then Went_Down
       elsif Old /= New_State then Inactive_Change
       else Unchanged);

   --  The state a refused report leaves.
   Fault_State : constant NL.Link_State :=
     (Condition => NL.Down, Reason => NL.Hardware_Fault);

   --  One pass over the block: Before and After, the generation read before
   --  and after reading Words.
   procedure Observe
     (O      : in out Observation;
      Before : NL.Generation;
      Words  : NL.Report_Words;
      After  : NL.Generation;
      Result : out Step)
   with
     Pre  => Valid (O),
     Post => Valid (O) and then
       (case Result.Found is
          when Unstable =>
            not NL.Stable (Before, After) and then O = O'Old
            and then Result.Change = Unchanged,
          when Same =>
            NL.Stable (Before, After) and then Before = O'Old.Seen and then O = O'Old
            and then Result.Change = Unchanged,
          when Taken =>
            NL.Stable (Before, After) and then Before /= O'Old.Seen
            and then O.Seen = Before
            and then NL.Decode (Words).Valid
            and then O.State = NL.Decode (Words).State
            and then Result.Reports = NL.Reports_Between (O'Old.Seen, Before)
            and then Result.Change = Change_Of (O'Old.State, O.State, Result.Reports),
          when Refused =>
            NL.Stable (Before, After) and then Before /= O'Old.Seen
            and then O.Seen = Before
            and then not NL.Decode (Words).Valid
            and then O.State = Fault_State
            and then Result.Reports = NL.Reports_Between (O'Old.Seen, Before)
            and then Result.Change = Change_Of (O'Old.State, O.State, Result.Reports));

   --  Up before and after with reports missed is never Unchanged (proved).
   procedure Lemma_No_Hidden_Flap
     (Old, New_State : NL.Link_State; Reports : NL.Generation)
   with Ghost, Global => null,
     Pre  => Old.Condition = NL.Up and then New_State.Condition = NL.Up and then Reports > 1,
     Post => Change_Of (Old, New_State, Reports) = Bounced;

end Link_Tracker;
