------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Debug sessions (DBG-001, Debug_ABI): mint a session from a target's
--  Control, and make the kernel debug-session object's Calls. A thin, typed
--  binding of the kernel interface (coordination/dbg.md) for the debug
--  engine and the kernel's tests; policy (who may debug what) lives in
--  procmgr, not here.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Debug_ABI;
with Capability_ABI;
with CuBit.Process_IDs;
with CuBit.Handles.IPC;

package CuBit.Handles.Debug is

   package ABI renames Debug_ABI;

   --  A debug handle of any of the four kinds.
   type Session is private;
   function Valid (Item : Session) return Boolean;

   type Mint_Result (Minted : Boolean := False) is record
      case Minted is
         when True =>
            Handle  : Session;
            --  For Revoke: the session's slot and generation.
            Session_Word : Unsigned_64 := 0;
         when False =>
            --  Why (a Debug_ABI outcome word), or 0 when the reply was unreadable.
            Why_Word : Unsigned_64 := 0;
      end case;
   end record;

   --  Mint a session of Kind on Target through Control (a CAP_PROCESS with
   --  RIGHT_GRANT: the target's own Control, or the wildcard root naming
   --  Target).
   function Mint
     (Control : Process; Kind : ABI.Debug_Kind;
      Target : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process)
      return Mint_Result;

   --  End a session through the Control it was minted from (or the wildcard).
   function Revoke (Control : Process; Session_Word : Unsigned_64) return Unsigned_64;

   --  One session Call: the reply's outcome word and words 1 .. 3, or an
   --  outcome word of 0 when the reply was unreadable.
   type Reply is record
      Outcome : Unsigned_64 := 0;
      W1, W2, W3 : Unsigned_64 := 0;
   end record;

   function Outcome_Is (R : Reply; What : ABI.Outcome) return Boolean is
     (R.Outcome = ABI.Outcome_Word_Of (What));

   function Call
     (Item : Session; Op : ABI.Operation; W0, W1, W2 : Unsigned_64 := 0) return Reply;

   --  Conveniences over Call.
   function Threads (Item : Session; Buffer : System.Address; Bytes : Unsigned_64)
     return Reply;
   function Registers
     (Item : Session; Selector : Unsigned_64; Buffer : System.Address; Bytes : Unsigned_64)
      return Reply;
   function Map_View (Item : Session; Target_Address, View : Unsigned_64; Pages : Unsigned_64)
     return Reply;
   function Unmap_View (Item : Session; View, Pages : Unsigned_64) return Reply;

   ---------------------------------------------------------------------------
   --  Passing a session on (procmgr mints, the debugger holds: CAP-012's
   --  launch handle and the session's debug authority).
   ---------------------------------------------------------------------------
   --  The capability kind of a session of Kind.
   function Capability_Kind (Kind : ABI.Debug_Kind) return Capability_ABI.Kind is
     (case Kind is
        when ABI.Observe    => Capability_ABI.Debug_Observe,
        when ABI.Pause      => Capability_ABI.Debug_Pause,
        when ABI.Modify     => Capability_ABI.Debug_Modify,
        when ABI.Instrument => Capability_ABI.Debug_Instrument);

   --  A copy of Item (a session of Kind) for a reply
   --  (CuBit.Handles.IPC.Reply_With), with Kept rights.
   function Copy (Item : Session; Kind : ABI.Debug_Kind; Kept : Rights) return Derivation;

   --  The session a reply carried, if it is a debug handle of Kind
   --  (checked with the kernel); otherwise Why says.
   type Accepted_Session (OK : Boolean := False) is record
      case OK is
         when True  => Value : Session;
         when False => Why   : Refusal := Capability_ABI.Wrong_Kind;
      end case;
   end record;
   function Accept_Session
     (Item : CuBit.Handles.IPC.Carried; Kind : ABI.Debug_Kind) return Accepted_Session;

   --  Let this process's copy of Item go (the session itself lives on in
   --  other copies until revoked, detached or its target or viewer ends).
   function Close (Item : Session) return Boolean;

   --  The handle word (for a typed binding that carries it as data).
   function Word (Item : Session) return Unsigned_64;
   function From_Word (Word : Unsigned_64) return Session;

private
   type Session is record
      Word : Unsigned_64 := 0;
   end record;
end CuBit.Handles.Debug;
