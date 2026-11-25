------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Apps menu's launch request to procmgr (OP_LAUNCH through Desktop's
--  Launch badge): what its answer says, and how long the Desktop waits for
--  it (BUG-008).
--
--  @description
--  Desktop submits one launch at a time and learns the outcome from its
--  completion. Classify reads that completion by the shape procmgr answers
--  (procmgr's answerLaunched and handleLaunch):
--  - Started: REPLY_OK with CuBit.Launch_Handles.Launched_Reply_Words
--    words, the child's identity, the launch handle's rights word and the
--    handle's issuance outcome. Desktop's submission accepts no capability,
--    so the handle itself is never transferred (the outcome then says
--    Not_Accepted): Desktop follows its children by their exit notices.
--  - Refused: REPLY_ERR with a typed CuBit.Launch_Arguments.Launch_Failure,
--    or Request_Withheld with the withheld request's label after it.
--  - Unreadable: anything else. The launch may or may not have started.
--  A request is never waited for forever: past Answer_Limit_Ms Desktop
--  stops waiting, and Stale_Tokens remembers the token so an answer that
--  still comes is recognised and dropped, not taken for an unknown
--  completion. Whatever procmgr may still hold of the old request (its
--  grant) Desktop revokes and never reuses.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Desktop_Launch_Request with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  procmgr's answer
   ---------------------------------------------------------------------------
   type Reply_Words is array (0 .. 3) of Unsigned_64;
   type Reply_Kind is (Started, Refused, Unreadable);

   --  Delivered: the completion is valid, COMPLETION_OK, with no protocol
   --  or reserved bits (the transport's part, checked by the caller).
   function Classify
     (Delivered : Boolean; Label : Unsigned_32; Length : Unsigned_8;
      Words : Reply_Words) return Reply_Kind;

   ---------------------------------------------------------------------------
   --  The bounded wait
   ---------------------------------------------------------------------------
   --  procmgr parks a launch while a provider it needs is starting, for at
   --  most that provider's ready deadline (CCL.Configurations.
   --  MAX_READY_DEADLINE_MS, 60 s): Desktop waits that long and a margin.
   Answer_Limit_Ms : constant := 75_000;

   --  Monotonic milliseconds when a request submitted at Submitted_Ms
   --  stops being waited for.
   function Due (Submitted_Ms : Unsigned_64) return Unsigned_64 is
     (if Submitted_Ms <= Unsigned_64'Last - Answer_Limit_Ms
      then Submitted_Ms + Answer_Limit_Ms else Unsigned_64'Last);

   function Overdue (Submitted_Ms, Now_Ms : Unsigned_64) return Boolean is
     (Now_Ms >= Due (Submitted_Ms));

   ---------------------------------------------------------------------------
   --  Launches Desktop stopped waiting for
   ---------------------------------------------------------------------------
   Stale_Capacity : constant := 4;
   type Stale_Tokens is private;

   function Contains (S : Stale_Tokens; Token : Unsigned_64) return Boolean;
   --  No room: Desktop cannot stop waiting for one more launch.
   function Full (S : Stale_Tokens) return Boolean;

   --  Every token remembered in Before is still in After (but Except).
   function Keeps (Before, After : Stale_Tokens; Except : Unsigned_64 := 0) return Boolean;

   procedure Remember (S : in out Stale_Tokens; Token : Unsigned_64)
     with Pre  => Token /= 0 and then not Full (S),
          Post => Contains (S, Token) and then Keeps (S'Old, S);

   --  Its answer came: it is no longer stale.
   procedure Forget (S : in out Stale_Tokens; Token : Unsigned_64)
     with Post => not Contains (S, Token) and then Keeps (S'Old, S, Except => Token);

private
   type Slot_Index is range 1 .. Stale_Capacity;
   --  0: a free slot (tokens are never 0, Compositor_Requests.Allocate).
   type Stale_Tokens is array (Slot_Index) of Unsigned_64 with
     Default_Component_Value => 0;

   function Contains (S : Stale_Tokens; Token : Unsigned_64) return Boolean is
     (Token /= 0 and then (for some I in Slot_Index => S (I) = Token));
   function Full (S : Stale_Tokens) return Boolean is
     (for all I in Slot_Index => S (I) /= 0);
   function Keeps (Before, After : Stale_Tokens; Except : Unsigned_64 := 0) return Boolean is
     (for all I in Slot_Index =>
        (if Before (I) /= 0 and then Before (I) /= Except then After (I) = Before (I)));
end Desktop_Launch_Request;
