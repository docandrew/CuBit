------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The image's debugging gate (DBG-001; CCL.Interfaces.System_Policy): the
--  check reports what procmgr gives it under the plan's (policy ...), and
--  headless --test debug-gate-<value> requires the outcome for that value:
--  * the session's debug authority (debug-session): granted, or withheld
--    at launch (then debug.attach, which needs it, refuses);
--  * Debug_Observe through it on clock.svc, when granted;
--  * Debug_Observe through a launch handle's Debug right on debug-plain.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Handles;
with CuBit.Handles.Debug;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Kernel_ABI;
with CuBit.Launch_Handles;
with CuBit.Launching;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Arguments;
with CuBit.Waits;
with Debug_ABI;
with Startup;

procedure Main is
   package LH renames CuBit.Launch_Handles;
   package HD renames CuBit.Handles.Debug;
   package IPC renames CuBit.Handles.IPC;
   use type LH.Outcome, CuBit.Launching.Launch_Result;
   Mint_Ms   : constant := 2_000;
   Settle_Ms : constant := 300;

   procedure Say (Text : String) is
   begin
      debugPrint ("debug-gate: " & Text & ASCII.LF);
   end Say;

   function Server_Of (Item : CuBit.Handles.Endpoint) return Unsigned_64 is
      D : constant IPC.Description := IPC.Describe (Item);
   begin
      return (if D.Current then D.Object else 0);
   end Server_Of;

   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   Plain : CuBit.Launching.Child;
   Launched : CuBit.Launching.Launch_Result;
   Failure : CuBit.Launch_Arguments.Launch_Failure;
   Session : HD.Session;
   Outcome : LH.Outcome;
   Ignore_Closed : Boolean;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Say ("starting");
   --  The session's debug authority, through its badge, on a system service.
   if not Startup.Debug_Session.Present then
      Say ("debug-session withheld");
   else
      declare
         Badge : constant CuBit.Handles.Endpoint :=
           CuBit.Handles.Services.Process_Manager.Handle (Startup.Debug_Session.Value);
         Request : Message := NULL_MESSAGE;
         Received : IPC.Carried;
         Answer : MessageTag;
         Ignore : IPC.Close_Result;
      begin
         Request.tag := (label => LH.Session_Debug_Operation, length => LH.Session_Debug_Words,
                         protocol => 0, ipc => <>, reserved => 0);
         Request.words := [Server_Of (Startup.Clock), Debug_ABI.Kind_Word (Debug_ABI.Observe), 0, 0];
         Answer := IPC.Call_Accepting (Badge, Request, Deadline_After (Mint_Ms), Received);
         Outcome := (if Answer.label = CuBit.Kernel_ABI.Reply_OK and then LH.Is_Outcome_Word (Request.words (0))
                     then LH.To_Outcome (Request.words (0)) else LH.Failed);
         Say ("debug-session granted; session debug answers " & LH.Outcome'Image (Outcome));
         Ignore := IPC.Close (Received);
      end;
   end if;
   --  A launch handle's Debug right.
   CuBit.Launching.Launch (Launcher, "debug-plain.app", [], [], Plain, Launched, Failure);
   if Launched /= CuBit.Launching.Launched then
      Say ("debug-plain did not start");
   else
      CuBit.Waits.Sleep_For (Settle_Ms);
      CuBit.Launching.Debug (Plain, Deadline_After (Mint_Ms), Session, Outcome);
      Say ("handle Debug answers " & LH.Outcome'Image (Outcome));
      if HD.Valid (Session) then
         Ignore_Closed := HD.Close (Session);
      end if;
      CuBit.Launching.End_Child (Plain, Launched);
   end if;
   Say ("done");
end Main;
