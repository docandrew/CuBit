------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The status area's items as explicit state machines (UI-027), in the
--  house pattern of Wifi_MLME: Step (State, Event, out Action), no I/O.
--  Desktop's event loop feeds each one the events (a due time, a reply, a
--  failed delivery) and carries out the action (submit, lend, revoke).
--
--  @description
--  Every machine recovers from a failed request: a completion that was not
--  delivered (the service died or restarted, the queue overflowed) or was
--  refused returns the machine to a state that asks again at its next due
--  time. The one-in-flight tracker this replaces (Compositor_Requests)
--  stayed Faulted after any such completion, so the clock stopped and a
--  queued volume setting was never sent.
--
--  - Clock: one read in flight; the next one at the next due time.
--  - Audio: one request in flight; a wanted volume setting is kept until a
--    reply confirms it (a newer setting replaces it): one that failed is
--    queued again and sent at the next due time, never dropped.
--  - Loan: a page lent to a service. A lent page is never reused or lent
--    again until the kernel confirms the grant retired (Revoke accepted is
--    not retirement): Free -> Lent -> Retiring -> Free.
--  - Watch: a service's parked watch and its lent report page. A failed
--    watch hides the item, revokes the page and waits Retry_Ms before
--    lending a fresh one (a restarted service is a new grantee).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Desktop_Status_Machines with SPARK_Mode, Pure is

   --  How a submitted request ended: Answered (delivered and its reply
   --  accepted), Refused (delivered, the service said no or the reply did
   --  not decode), Not_Delivered (transport failure, or the submission
   --  itself refused). Only Answered confirms anything.
   type Outcome is (Answered, Refused, Not_Delivered);

   ---------------------------------------------------------------------------
   --  Clock
   ---------------------------------------------------------------------------
   type Clock_State is (Idle, Reading);
   type Clock_Event is (Due, Done);
   type Clock_Action is (None, Submit_Read);

   procedure Step (S : in out Clock_State; E : Clock_Event; Act : out Clock_Action)
   with Post => (case E is
                   when Due  => (if S'Old = Idle then S = Reading and then Act = Submit_Read
                                 else S = Reading and then Act = None),
                   when Done => S = Idle and then Act = None);

   ---------------------------------------------------------------------------
   --  Audio
   ---------------------------------------------------------------------------
   --  CuBit.Audio_Control.Percent (Desktop_Status_Refresh checks they agree).
   subtype Percent is Natural range 0 .. 100;

   type Setting is record
      Level : Percent := 0;
      Muted : Boolean := False;
   end record;

   type Audio_Phase is (Idle, Reading, Setting_Level);

   type Audio_State is record
      Phase  : Audio_Phase := Idle;
      Queued : Boolean := False;     --  Wanted is still to be sent
      Wanted : Setting;
      Sent   : Setting;              --  in flight (Setting_Level)
   end record;

   type Audio_Event_Kind is (Due, Set, Done);
   type Audio_Event (Kind : Audio_Event_Kind := Due) is record
      case Kind is
         when Set  => Value : Setting;
         when Done => Result : Outcome;
         when Due  => null;
      end case;
   end record;

   type Audio_Action_Kind is (None, Submit_Get, Submit_Set);
   --  Value: the setting to send (Submit_Set only).
   type Audio_Action is record
      Kind  : Audio_Action_Kind := None;
      Value : Setting;
   end record;

   --  The setting the user asked for last is not lost: it is queued,
   --  in flight, or confirmed.
   function Pending (S : Audio_State) return Boolean is
     (S.Queued or else S.Phase = Setting_Level);

   procedure Step (S : in out Audio_State; E : Audio_Event; Act : out Audio_Action)
   with Post =>
     --  A new setting is kept until sent (newest wins).
     (if E.Kind = Set then
        (S.Queued and then S.Wanted = E.Value) or else
        (Act.Kind = Submit_Set and then Act.Value = E.Value and then S.Phase = Setting_Level
         and then S.Sent = E.Value))
     --  A setting that did not reach the mixer goes back in the queue.
     and then (if E.Kind = Done and then S'Old.Phase = Setting_Level and then E.Result /= Answered
                 and then not S'Old.Queued
               then S.Queued and then S.Wanted = S'Old.Sent)
     --  Nothing queued is ever dropped.
     and then (if S'Old.Queued and then E.Kind /= Set then
                 S.Queued = (Act.Kind /= Submit_Set)
                 and then (if S.Queued then S.Wanted = S'Old.Wanted
                           else Act.Value = S'Old.Wanted))
     --  One request at a time.
     and then (if Act.Kind /= None then S'Old.Phase = Idle or else E.Kind = Done)
     and then (if E.Kind = Done then
                 (if Act.Kind = None then S.Phase = Idle));

   ---------------------------------------------------------------------------
   --  Lent pages
   ---------------------------------------------------------------------------
   type Loan_State is (Free, Lent, Retiring);
   type Loan_Event is (Lend_Done, Lend_Failed, Request_Over, Retired);
   type Loan_Action is (None, Revoke);

   procedure Step (S : in out Loan_State; E : Loan_Event; Act : out Loan_Action)
   with Post => (case E is
                   when Lend_Done    => (if S'Old = Free then S = Lent else S = S'Old),
                   when Lend_Failed  => S = S'Old,
                   when Request_Over => (if S'Old = Lent then S = Retiring and then Act = Revoke
                                         else S = S'Old and then Act = None),
                   when Retired      => (if S'Old = Retiring then S = Free else S = S'Old))
                and then (if E /= Request_Over then Act = None);

   ---------------------------------------------------------------------------
   --  Watches
   ---------------------------------------------------------------------------
   --  How long a failed watch waits before it is tried again.
   Retry_Ms : constant := 2_000;

   type Watch_Phase is (Idle, Watching, Waiting);

   type Watch_State is record
      Phase    : Watch_Phase := Idle;
      Loan     : Loan_State := Free;
      Retry_At : Unsigned_64 := 0;
      Shown    : Boolean := False;    --  a report arrived since the last failure
   end record;

   type Watch_Event_Kind is (Tick, Lend_Failed, Done, Retired);
   type Watch_Event (Kind : Watch_Event_Kind := Tick) is record
      Now : Unsigned_64 := 0;
      case Kind is
         when Done => Result : Outcome;
         when Tick | Lend_Failed | Retired => null;
      end case;
   end record;

   --  Lend_And_Submit: lend a fresh page, then submit the watch on it;
   --  Submit: submit the watch on the page lent already; Revoke: take the
   --  page back (it is Retiring until the kernel confirms).
   type Watch_Action is (None, Lend_And_Submit, Submit, Revoke);

   procedure Step (S : in out Watch_State; E : Watch_Event; Act : out Watch_Action)
   with Post =>
     (if Act = Lend_And_Submit then S'Old.Loan = Free and then S.Loan = Lent and then S.Phase = Watching)
     and then (if Act = Submit then S'Old.Loan = Lent and then S.Phase = Watching)
     and then (if Act = Revoke then S'Old.Loan = Lent and then S.Loan = Retiring)
     and then (if E.Kind = Done and then E.Result /= Answered then not S.Shown and then S.Phase = Waiting)
     and then (if E.Kind = Done and then E.Result = Answered and then S'Old.Phase = Watching
               then S.Shown);

   ---------------------------------------------------------------------------
   --  A switch another service holds (procmgr's "Allow app replacement")
   ---------------------------------------------------------------------------
   --  Confirmed: what the service last confirmed (off until then: procmgr
   --  starts with it off). One request in flight; a flip meanwhile is
   --  queued (the newest wins) and sent when it ends. A refused or
   --  undelivered request leaves Confirmed as it was, so the panel goes
   --  back to the service's state: the flip is answered, never lost.
   type Switch_Phase is (Idle, Switching);

   type Switch_State is record
      Phase     : Switch_Phase := Idle;
      Confirmed : Boolean := False;
      Sent      : Boolean := False;   --  in flight (Switching)
      Queued    : Boolean := False;   --  Wanted is still to be sent
      Wanted    : Boolean := False;
   end record;

   type Switch_Event_Kind is (Flip, Done);
   type Switch_Event (Kind : Switch_Event_Kind := Flip) is record
      case Kind is
         when Flip => Value : Boolean;
         when Done => Result : Outcome;
      end case;
   end record;

   --  Submit: send Value.
   type Switch_Action is record
      Submit : Boolean := False;
      Value  : Boolean := False;
   end record;

   --  What the panel shows: the newest wish while one is pending.
   function Shown (S : Switch_State) return Boolean is
     (if S.Queued then S.Wanted elsif S.Phase = Switching then S.Sent else S.Confirmed);

   procedure Step (S : in out Switch_State; E : Switch_Event; Act : out Switch_Action)
   with Post =>
     --  A flip shows at once.
     (if E.Kind = Flip then Shown (S) = E.Value)
     --  Only an answered request changes what is confirmed.
     and then (if E.Kind = Done and then E.Result = Answered and then S'Old.Phase = Switching
               then S.Confirmed = S'Old.Sent else S.Confirmed = S'Old.Confirmed)
     --  One request at a time, and it is what is in flight.
     and then (if Act.Submit then S.Phase = Switching and then S.Sent = Act.Value
                 and then (S'Old.Phase = Idle or else E.Kind = Done))
     and then (if E.Kind = Done and then S'Old.Phase = Switching and then not Act.Submit
               then S.Phase = Idle and then not S.Queued);
end Desktop_Status_Machines;
