------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How long an app may take to close at shutdown, and how its close ends
--  (PWR-003, apps close safely): the pure policy, proved.
--
--  @description
--  By default an app gets its stop grace and no more: it writes a recovery
--  copy (CuBit.UI.Recovery) and exits; shutdown is never stalled. Only an
--  app whose manifest declares the delay capability, with a declared
--  maximum (Delay_Capability), may ask to wait for the person (Save /
--  Discard / Cancel shutdown); it then has until the later of its grace and
--  the delay it asked for, never past its declared maximum. Cancel ends
--  the whole shutdown; a forced end happens only past that deadline or
--  after the person confirmed it.
--
--  Status: this policy is proved and hosted-tested; the manifest request,
--  procmgr's use of the deadline, Desktop's prompt and the cancel path are
--  the follow-up slice (not wired yet).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Close_Policies with Pure, SPARK_Mode is

   --  The longest delay any manifest may declare (a named ceiling).
   Maximum_Delay_Ms : constant := 120_000;
   subtype Delay_Ms is Unsigned_32 range 0 .. Maximum_Delay_Ms;
   --  CuBit.Child_Specs.Maximum_Grace_Ms.
   Maximum_Grace_Ms : constant := 600_000;
   subtype Grace_Ms is Unsigned_32 range 1 .. Maximum_Grace_Ms;

   --  The latest time a deadline is set from, so additions never wrap.
   Latest_Time : constant := Unsigned_64'Last - Maximum_Grace_Ms - Maximum_Delay_Ms;
   subtype Time is Unsigned_64 range 0 .. Latest_Time;

   --  What the app's launch granted: the capability, and the maximum its
   --  manifest declared (0 when not granted).
   type Delay_Capability is record
      Granted      : Boolean := False;
      Declared_Max : Delay_Ms := 0;
   end record;

   --  The delay an app gets for Requested: none without the capability,
   --  never more than its declared maximum.
   function Allowed_Delay (Cap : Delay_Capability; Requested : Delay_Ms) return Delay_Ms is
     (if Cap.Granted then Delay_Ms'Min (Requested, Cap.Declared_Max) else 0)
   with Post => Allowed_Delay'Result <= Requested
                and then (if not Cap.Granted then Allowed_Delay'Result = 0)
                and then (if Cap.Granted then Allowed_Delay'Result <= Cap.Declared_Max);

   --  When the app's close ends at the latest (Stop sent at Stop_At).
   function Close_Deadline
     (Stop_At : Time; Grace : Grace_Ms; Cap : Delay_Capability; Requested : Delay_Ms) return Unsigned_64
   is (Stop_At + Unsigned_64 (Unsigned_32'Max (Grace, Allowed_Delay (Cap, Requested))))
   with Post => Close_Deadline'Result >= Stop_At + Unsigned_64 (Grace)
                and then (if not Cap.Granted then Close_Deadline'Result = Stop_At + Unsigned_64 (Grace));

   --  The person's answer to the prompt (only an app with the capability
   --  can raise one).
   type Answer is (No_Answer, Save, Discard, Cancel_Shutdown, Force_Confirmed);

   type Outcome is
     (Waiting,             --  not decided yet
      Closed,              --  the app ended by itself
      Shutdown_Cancelled,  --  the person cancelled: the whole stop ends, the app runs on
      Forced_After_Grace,  --  ended at its grace (no capability, or no request)
      Forced_After_Delay,  --  ended at its declared maximum
      Forced_By_User);     --  the person confirmed ending it

   --  Where an app's close stands at Now.
   function Decide
     (Cap : Delay_Capability; Requested : Delay_Ms; Exited : Boolean; Given : Answer;
      Stop_At : Time; Grace : Grace_Ms; Now : Unsigned_64) return Outcome
   is (if Exited then Closed
       elsif Cap.Granted and then Given = Cancel_Shutdown then Shutdown_Cancelled
       elsif Cap.Granted and then Given = Force_Confirmed then Forced_By_User
       elsif Now >= Close_Deadline (Stop_At, Grace, Cap, Requested) then
         (if Allowed_Delay (Cap, Requested) > Grace then Forced_After_Delay else Forced_After_Grace)
       else Waiting)
   with Post =>
     --  Without the capability nothing can cancel, nothing waits past the grace.
     (if not Cap.Granted then Decide'Result /= Shutdown_Cancelled and then Decide'Result /= Forced_By_User
                              and then (if Now >= Stop_At + Unsigned_64 (Grace) then Decide'Result /= Waiting))
     --  Forced only past the deadline, or by the person's confirmation.
     and then (if Decide'Result in Forced_After_Grace | Forced_After_Delay then
                 Now >= Close_Deadline (Stop_At, Grace, Cap, Requested))
     and then (if Decide'Result = Forced_By_User then Cap.Granted and then Given = Force_Confirmed)
     --  An app that ended is never reported forced.
     and then (if Exited then Decide'Result = Closed);
end CuBit.Close_Policies;
