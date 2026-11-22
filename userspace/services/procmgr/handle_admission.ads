------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The launch handle's decisions (CAP-012, docs/authority-model.md 7.5 item
--  5; DBG-001, docs/debugging.md 2.2 and 2.7): which requests on procmgr's
--  endpoint a badge admits, what a handle's holder may do, what a launch
--  issues, how a handle narrows, and when procmgr mints a debug session.
--
--  @description
--  Pure SPARK, proved at level 2 (tests/launch-handles). procmgr's main
--  gathers the facts (its handle table, the target's class) and acts on the
--  answer; nothing here holds state.
--
--  Stand-ins, stated so they are not mistaken for the design: the session's
--  debug envelope is User_Session_Debug below, a constant like
--  Session_Launch.User_Session, until security contexts (CTX-001) carry it
--  as a typed CCL value. Only Debug_Observe is ever minted: the kernel's
--  other three kinds answer Not_Yet, and the envelope does not offer them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Debug_ABI;
with CuBit.Launch_Handles;
with Session_Launch;

package Handle_Admission with SPARK_Mode, Pure is

   package LH renames CuBit.Launch_Handles;
   use type LH.Operation, LH.Outcome, LH.Right, LH.Rights, LH.Debug_Path;
   use type Session_Launch.Badge;
   use type Debug_ABI.Debug_Kind;

   ---------------------------------------------------------------------------
   --  1. Routing: a request on procmgr's endpoint, by the badge the kernel
   --  stamped on it. A launch handle admits only the handle operations, and
   --  the session's debug badge only its one operation: holding either is
   --  never a way to procmgr's other services.
   ---------------------------------------------------------------------------
   type Route is (Handle_Request, Session_Debug_Request, Ordinary, Refused);

   function Route_Of (Tag : Unsigned_64; Label : Unsigned_32) return Route is
     (if LH.Is_Handle_Tag (Tag) then
        (if LH.Is_Operation_Label (Label) then Handle_Request else Refused)
      elsif LH.Is_Operation_Label (Label) then Refused
      elsif Tag = LH.Session_Debug_Tag then
        (if Label = LH.Session_Debug_Operation then Session_Debug_Request else Refused)
      elsif Label = LH.Session_Debug_Operation then Refused
      else Ordinary)
   with Post =>
     (case Route_Of'Result is
        when Handle_Request =>
          LH.Is_Handle_Tag (Tag) and then LH.Is_Operation_Label (Label),
        when Session_Debug_Request =>
          Tag = LH.Session_Debug_Tag and then Label = LH.Session_Debug_Operation,
        when Ordinary =>
          not LH.Is_Handle_Tag (Tag) and then Tag /= LH.Session_Debug_Tag
          and then not LH.Is_Operation_Label (Label)
          and then Label /= LH.Session_Debug_Operation,
        when Refused => True);

   ---------------------------------------------------------------------------
   --  2. An operation through a launch handle.
   ---------------------------------------------------------------------------
   --  The issuance the tag names, in procmgr's table: unknown (never issued,
   --  or forgotten), live, its child ended, or revoked.
   type Issuance_State is (Unknown, Live, Child_Gone, Revoked);

   type Facts is record
      State  : Issuance_State := Unknown;
      Rights : LH.Rights := LH.No_Rights;
   end record;

   --  Holds the right Op needs, if any.
   function Carries (Held : LH.Rights; Op : LH.Operation) return Boolean is
     (not LH.Needed (Op).Needs or else Held (LH.Needed (Op).Of_Right));

   function Decide (F : Facts; Op : LH.Operation; Length : Unsigned_8) return LH.Outcome is
     (if F.State = Unknown then LH.Not_A_Handle
      elsif F.State = Revoked then LH.Revoked
      elsif Length /= LH.Request_Words (Op) then LH.Malformed
      elsif F.State = Child_Gone and then Op not in LH.Observe_Op | LH.Revoke_Op then LH.Child_Ended
      elsif not Carries (F.Rights, Op) then LH.Right_Missing
      else LH.Done)
   with Post =>
     (if Decide'Result = LH.Done then
        F.State in Live | Child_Gone
        and then Length = LH.Request_Words (Op)
        --  It dies with the child: afterwards only the exit outcome, and
        --  letting go.
        and then (if F.State = Child_Gone then Op in LH.Observe_Op | LH.Revoke_Op)
        --  Each right gates its operation.
        and then (if Op = LH.Observe_Op then F.Rights (LH.Observe))
        and then (if Op = LH.Control_Op then F.Rights (LH.Stop))
        and then (if Op = LH.Attach_Op then F.Rights (LH.Attach))
        and then (if Op = LH.Introduce_Op then F.Rights (LH.Introduce))
        and then (if Op = LH.Debug_Op then F.Rights (LH.Debug)));

   ---------------------------------------------------------------------------
   --  3. What a launch issues, and narrowing.
   ---------------------------------------------------------------------------
   --  A Launch badge into the user session gives every right; a remote one
   --  (ccl-remote) no Debug or Introduce: remote debugging needs its own
   --  grants (docs/debugging.md 7.4). No badge, no handle.
   Remote_Rights : constant LH.Rights :=
     [LH.Observe | LH.Stop | LH.Attach => True, others => False];

   function Issued (B : Session_Launch.Badge) return LH.Rights is
     (case B is
        when Session_Launch.Session_Badge => LH.All_Rights,
        when Session_Launch.Remote_Badge  => Remote_Rights,
        when Session_Launch.No_Badge      => LH.No_Rights)
   with Post =>
     (if B = Session_Launch.No_Badge then Issued'Result = LH.No_Rights)
     and then (if B = Session_Launch.Remote_Badge then
                 not Issued'Result (LH.Debug) and then not Issued'Result (LH.Introduce));

   --  A narrowed issuance: at most what is held, and only what is asked.
   function Narrowed (Held, Asked : LH.Rights) return LH.Rights is
     [for R in LH.Right => Held (R) and then Asked (R)]
   with Post =>
     LH.Covers (Held, Narrowed'Result)
     and then (for all R in LH.Right => (if Narrowed'Result (R) then Asked (R)));

   ---------------------------------------------------------------------------
   --  4. Minting a debug session (procmgr holds every child's Control).
   ---------------------------------------------------------------------------
   --  What the target is, as procmgr knows it.
   type Target_Class is
     (Session_Member,    --  launched into the session (Launch-into-S)
      System_Service,    --  started by the startup plan
      Secrets_Service,   --  secrets.svc: no debug envelope anywhere (2.10)
      Trusted_Path,      --  the Desktop, which draws the trusted prompt
      Process_Manager,   --  procmgr itself
      Device_Manager);   --  devmgr

   function Never_Debuggable (C : Target_Class) return Boolean is
     (C in Secrets_Service | Trusted_Path | Process_Manager | Device_Manager);

   type Kind_Set is array (Debug_ABI.Debug_Kind) of Boolean;
   Observe_Only : constant Kind_Set := [Debug_ABI.Observe => True, others => False];
   No_Kinds     : constant Kind_Set := [others => False];

   --  A session's debug envelope (docs/debugging.md 2.2's Debug_Policy):
   --  for what a holder launched itself, for other members of the
   --  session, and for system services.
   type Debug_Envelope is record
      Own_Launches    : Kind_Set := No_Kinds;
      Session_Peers   : Kind_Set := No_Kinds;
      System_Services : Kind_Set := No_Kinds;
   end record;

   --  The user session (provisional answers 4 and 5 of docs/debugging.md:
   --  system services Observe only; agents Observe through the engine).
   User_Session_Debug : constant Debug_Envelope :=
     (Own_Launches    => [Debug_ABI.Observe => True, others => False],
      Session_Peers   => [Debug_ABI.Observe => True, others => False],
      System_Services => [Debug_ABI.Observe => True, others => False]);

   --  The kinds the envelope allows on a target of that class by that path.
   --  Through a launch handle, the target is the handle's own child.
   function Allowed (E : Debug_Envelope; Path : LH.Debug_Path; C : Target_Class) return Kind_Set is
     (if Never_Debuggable (C) then No_Kinds
      elsif Path = LH.Launch_Handle_Path then
        (if C = Session_Member then E.Own_Launches else No_Kinds)
      elsif C = Session_Member then E.Session_Peers
      else E.System_Services);

   --  The image's debugging gate (its system policy, fixed at boot:
   --  CCL.Interfaces.System_Policy). Only Development debugs at all;
   --  Production will need the owner's approval per session (not built),
   --  so it refuses as Disabled does, saying why.
   type Debugging_Gate is (Disabled, Production, Development);

   --  The session's debug authority (role debug-session) is granted only
   --  in a Development image.
   function Debug_Authority_Granted (Gate : Debugging_Gate) return Boolean is
     (Gate = Development)
   with Post => Debug_Authority_Granted'Result = (Gate = Development);

   function Gate_Refusal (Gate : Debugging_Gate) return LH.Outcome is
     (if Gate = Production then LH.Approval_Needed else LH.Debugging_Disabled);

   type Debug_Facts is record
      Path         : LH.Debug_Path := LH.Session_Authority_Path;
      Class        : Target_Class := Process_Manager;
      --  procmgr holds the target's Control (it started it and it runs).
      Held_Control : Boolean := False;
      Kind         : Debug_ABI.Debug_Kind := Debug_ABI.Observe;
   end record;

   function Debug_Decision
     (F : Debug_Facts; E : Debug_Envelope; Gate : Debugging_Gate) return LH.Outcome is
     (if Gate /= Development then Gate_Refusal (Gate)
      elsif Never_Debuggable (F.Class) then LH.Never_Debuggable
      elsif not F.Held_Control then LH.Not_Held
      elsif not Allowed (E, F.Path, F.Class) (F.Kind) then LH.Kind_Withheld
      else LH.Done)
   with Post =>
     (if Gate = Disabled then Debug_Decision'Result = LH.Debugging_Disabled)
     and then (if Gate = Production then Debug_Decision'Result = LH.Approval_Needed)
     and then
     (if Debug_Decision'Result = LH.Done then
        Gate = Development
        and then not Never_Debuggable (F.Class)
        and then F.Held_Control
        and then F.Class not in Secrets_Service | Trusted_Path | Process_Manager | Device_Manager
        and then (if F.Path = LH.Launch_Handle_Path then
                    F.Class = Session_Member and then E.Own_Launches (F.Kind))
        and then (if F.Path = LH.Session_Authority_Path and then F.Class = Session_Member
                  then E.Session_Peers (F.Kind))
        and then (if F.Path = LH.Session_Authority_Path and then F.Class = System_Service
                  then E.System_Services (F.Kind)));

   --  In the user session, only Debug_Observe is ever minted.
   procedure Lemma_User_Session_Observes_Only (F : Debug_Facts; Gate : Debugging_Gate)
   with Ghost, Global => null,
        Post => (if Debug_Decision (F, User_Session_Debug, Gate) = LH.Done
                 then F.Kind = Debug_ABI.Observe);

end Handle_Admission;
