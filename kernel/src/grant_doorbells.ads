-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Channel wakes (CAP-002 step 3, docs/capability-handles.md
-- section 4): the bookkeeping of one grant's pending wake, for its grantee.
--
-- A channel side wakes its peer with SEND_EVENT and Channel_Wake_ABI's
-- label, naming the grant of the region it owns; the peer is that grant's
-- grantee. The wake is a fact, not a count: one pending wake per grant (per
-- sender and channel), a repeat while pending is the same fact (coalesced),
-- so wakes never consume request credits, never overflow and are never lost.
-- The caller keeps the grant on the grantee's bell list exactly while its
-- wake is pending: Ring says when to link (Newly), Take and Finish when to
-- unlink. Ended: the grant life is over; every later wake is refused (the
-- grant-end notices, IPC-002, already wake the grantee).
--
-- The caller (Process.IPC) holds grantLock and checks authority (the owner
-- wakes; the grantee's receive takes); this unit is the state machine.
-------------------------------------------------------------------------------
package Grant_Doorbells with SPARK_Mode, Pure is

    type Bell is record
        Pending : Boolean := False;
        Ended   : Boolean := False;
    end record;

    Silent : constant Bell := (Pending => False, Ended => False);

    type Ring_Outcome is (Rung, Ended_Grant);

    -- The owner wakes the grantee. Rung: pending; Newly when it was not
    -- pending before (the caller links the grant into the grantee's list).
    procedure Ring (B : in out Bell; Outcome : out Ring_Outcome; Newly : out Boolean)
      with Post =>
        (if B'Old.Ended then
            Outcome = Ended_Grant and then B = B'Old and then not Newly
         else
            Outcome = Rung and then B.Pending and then not B.Ended
            and then Newly = not B'Old.Pending);

    -- The grantee's receive takes the wake. Taken: it was pending (the
    -- caller unlinks it).
    procedure Take (B : in out Bell; Taken : out Boolean)
      with Post => not B.Pending and then Taken = B'Old.Pending and then B.Ended = B'Old.Ended;

    -- The grant life ends. Was_Pending: it was linked (the caller unlinks).
    procedure Finish (B : in out Bell; Was_Pending : out Boolean)
      with Post => B.Ended and then not B.Pending and then Was_Pending = B'Old.Pending;

    -- Never lost: after a wake, the grantee's next take finds it. Never
    -- doubled: a second take after it does not, however many wakes came
    -- before the first take (coalesced into one).
    procedure Lemma_Ring_Taken_Once (B : in out Bell; Repeats : Natural; First, Second : out Boolean)
      with Ghost,
           Pre  => not B.Ended,
           Post => First and then not Second;

    -- After the end every wake is refused and nothing is pending.
    procedure Lemma_Ended_Refuses (B : in out Bell; Outcome : out Ring_Outcome)
      with Ghost,
           Post => Outcome = Ended_Grant and then not B.Pending;

end Grant_Doorbells;
