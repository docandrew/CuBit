------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The launch handle (CAP-012, docs/authority-model.md 7.5 item 5): what a
--  Launch-into-S launch gives its requester instead of the child's
--  authority. The pure wire encoding, shared by procmgr and its clients.
--
--  @description
--  A launch handle is procmgr's endpoint, badged by procmgr with a tag that
--  names one issuance (Tag_Of). The kernel stamps the tag on every request
--  through it, and only procmgr holds the untagged endpoint it is badged
--  from, so a tag cannot be forged. procmgr keeps, per issuance, the child
--  it names and the rights it carries:
--
--  * Observe: the child's state, and its exit outcome once it has ended;
--  * Stop: control messages (stop, interrupt, reload) and ending it;
--  * Attach: connecting to its outlets after the launch (the shape only;
--    procmgr answers Not_Yet);
--  * Introduce: a late introduction through procmgr's ledger, from what
--    the holder itself holds;
--  * Debug: procmgr mints Debug_Observe from the child's Control and hands
--    it over (DBG-001).
--
--  A handle carries none of the child's grants: none of the child's
--  capabilities is reachable through it, only procmgr's operations below.
--  It is revocable (Revoke_Operation ends an issuance and every issuance
--  narrowed from it), and it dies with the child: after the child ends,
--  only Observe (the exit outcome) and Revoke are answered.
--
--  The child's kernel parent is procmgr (its session supervisor S is a node
--  there), not the requester. procmgr tells the requester the child ended
--  with Exit_Notice_Label, an event through procmgr's own badge on the
--  requester's mailbox (CuBit.Child_Exits.Is_Exit accepts it beside the
--  kernel's EVENT_CHILD_EXIT).
--
--  Requests: label = an operation's label, words as Request_Words says;
--  replies: words (0) = an Outcome word, the rest as each operation says.
--  Debug_Op, Narrow_Op and Session_Debug_Operation answer with a capability
--  (the request must accept one: CuBit.Handles.IPC.Call_Accepting).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;

package CuBit.Launch_Handles with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Rights
   ---------------------------------------------------------------------------
   type Right is (Observe, Stop, Attach, Introduce, Debug);
   type Rights is array (Right) of Boolean;
   No_Rights  : constant Rights := [others => False];
   All_Rights : constant Rights := [others => True];

   function Covers (Outer, Inner : Rights) return Boolean is
     (for all R in Right => (if Inner (R) then Outer (R)));

   --  On the wire: one bit per right, Observe the lowest.
   Rights_Bits : constant := Right'Pos (Right'Last) + 1;
   Rights_Word_Limit : constant Unsigned_64 := 2 ** Rights_Bits;

   function Bit (R : Right) return Unsigned_64 is (Shift_Left (1, Right'Pos (R)));

   function Valid_Rights_Word (Word : Unsigned_64) return Boolean is
     (Word < Rights_Word_Limit);

   function Rights_Of (Word : Unsigned_64) return Rights is
     [for R in Right => (Word and Bit (R)) /= 0];

   function Word_Of (Set : Rights) return Unsigned_64 is
     ((if Set (Observe) then Bit (Observe) else 0)
      or (if Set (Stop) then Bit (Stop) else 0)
      or (if Set (Attach) then Bit (Attach) else 0)
      or (if Set (Introduce) then Bit (Introduce) else 0)
      or (if Set (Debug) then Bit (Debug) else 0));

   ---------------------------------------------------------------------------
   --  Tags. procmgr's badges on its own endpoint; only procmgr can set them.
   ---------------------------------------------------------------------------
   --  A launch handle: ASCII "LH" over a 48-bit issuance serial (never 0,
   --  never reused while procmgr runs).
   Handle_Tag_Prefix : constant Unsigned_64 := 16#4C48_0000_0000_0000#;
   Serial_Bits       : constant := 48;
   Serial_Limit      : constant Unsigned_64 := 2 ** Serial_Bits;
   Serial_Mask       : constant Unsigned_64 := Serial_Limit - 1;
   subtype Serial is Unsigned_64 range 1 .. Serial_Mask;

   function Is_Handle_Tag (Tag : Unsigned_64) return Boolean is
     ((Tag and not Serial_Mask) = Handle_Tag_Prefix and then (Tag and Serial_Mask) /= 0);

   function Tag_Of (S : Serial) return Unsigned_64 is (Handle_Tag_Prefix or S)
   with Post => Is_Handle_Tag (Tag_Of'Result);

   function Serial_Of (Tag : Unsigned_64) return Serial is (Tag and Serial_Mask)
   with Pre => Is_Handle_Tag (Tag), Post => Tag_Of (Serial_Of'Result) = Tag;

   --  "Debug-observe for session S" (DBG-001, docs/debugging.md 2.2): the
   --  session's debug badge on procmgr's endpoint, catalog role
   --  debug-session. Its holder (the user's Debugger, a CCL console) asks
   --  procmgr for Debug_Observe on a process by identity; procmgr decides
   --  under the session's debug envelope. ASCII "DBGSESS" 1.
   Session_Debug_Role : constant := 48;
   Session_Debug_Tag  : constant Unsigned_64 := 16#4442_4753_4553_5301#;

   --  procmgr's badge on a requester's mailbox for the exit notice: ASCII
   --  "LNOTICE" 1. Only procmgr holds an untagged endpoint to a process
   --  with Grant (its mailbox from SPAWN; manifest copies never carry
   --  Grant), so only procmgr can send through this tag.
   Notice_Tag : constant Unsigned_64 := CuBit.Child_Exits.Notice_Tag;

   ---------------------------------------------------------------------------
   --  Operations through a launch handle
   ---------------------------------------------------------------------------
   type Operation is
     (Observe_Op,     --  the child's state and exit outcome
      Control_Op,     --  words (0) = a Control_Request
      Attach_Op,      --  words (0) = the outlet's index (Not_Yet)
      Introduce_Op,   --  words (0) = the child's startup-table index
      Debug_Op,       --  words (0) = a Debug_ABI kind word; carries the session
      Narrow_Op,      --  words (0) = a rights word; carries the new handle
      Revoke_Op);     --  this issuance and every one narrowed from it
   for Operation use
     (Observe_Op => 16#0116#, Control_Op => 16#0117#, Attach_Op => 16#0118#,
      Introduce_Op => 16#0119#, Debug_Op => 16#011A#, Narrow_Op => 16#011B#,
      Revoke_Op => 16#011C#);

   function Label_Of (Op : Operation) return Unsigned_32 is
     (Unsigned_32 (Operation'Enum_Rep (Op)));

   function Is_Operation_Label (Label : Unsigned_32) return Boolean is
     (Label in Label_Of (Operation'First) .. Label_Of (Operation'Last));

   function To_Operation (Label : Unsigned_32) return Operation is
     (case Label is
        when 16#0116# => Observe_Op, when 16#0117# => Control_Op,
        when 16#0118# => Attach_Op, when 16#0119# => Introduce_Op,
        when 16#011A# => Debug_Op, when 16#011B# => Narrow_Op,
        when others => Revoke_Op)
   with Pre  => Is_Operation_Label (Label),
        Post => Label_Of (To_Operation'Result) = Label;

   Single_Word_Request : constant := 1;
   Empty_Request       : constant := 0;

   function Request_Words (Op : Operation) return Unsigned_8 is
     (case Op is
        when Observe_Op | Revoke_Op => Empty_Request,
        when Control_Op | Attach_Op | Introduce_Op | Debug_Op | Narrow_Op => Single_Word_Request);

   --  The right each operation needs; Narrow_Op and Revoke_Op need none
   --  (a holder may narrow or revoke what it holds).
   type Needed_Right (Needs : Boolean := False) is record
      case Needs is
         when True  => Of_Right : Right := Observe;
         when False => null;
      end case;
   end record;

   function Needed (Op : Operation) return Needed_Right is
     (case Op is
        when Observe_Op   => (Needs => True, Of_Right => Observe),
        when Control_Op   => (Needs => True, Of_Right => Stop),
        when Attach_Op    => (Needs => True, Of_Right => Attach),
        when Introduce_Op => (Needs => True, Of_Right => Introduce),
        when Debug_Op     => (Needs => True, Of_Right => Debug),
        when Narrow_Op | Revoke_Op => (Needs => False));

   --  Debug-observe for session S: words (0) = the target's identity word,
   --  (1) = a Debug_ABI kind word; carries the session. Only through
   --  Session_Debug_Tag.
   Session_Debug_Operation : constant Unsigned_32 := 16#011D#;
   Session_Debug_Words     : constant := 2;

   --  The debug sessions procmgr minted, one per call (the published state
   --  the Desktop reads to show "being debugged by"): words (0) = the
   --  record's index from 0. Reply (1) = the target's identity, (2) = the
   --  holder's identity, (3) = Record_Word (kind, path, more). Only through
   --  a process-observer badge.
   Debug_Records_Operation : constant Unsigned_32 := 16#011E#;
   Debug_Records_Words     : constant := 1;

   --  procmgr's notice to the requester that a child it launched ended:
   --  words as CuBit.Child_Exits (identity, kind, code), tag Notice_Tag.
   Exit_Notice_Label : constant Unsigned_32 := CuBit.Child_Exits.Notice_Label;

   ---------------------------------------------------------------------------
   --  Replies
   ---------------------------------------------------------------------------
   type Outcome is
     (Done,
      Not_A_Handle,      --  the request did not come through a launch handle
      Revoked,           --  the issuance was revoked
      Right_Missing,     --  the handle does not carry the right
      Child_Ended,       --  the child has ended: only Observe and Revoke
      Malformed,         --  wrong word count or an invalid word
      Not_Yet,           --  the shape exists; procmgr cannot do it yet
      Busy,              --  the child has unread control messages: try again
      Not_Accepted,      --  the request did not accept the capability
      Never_Debuggable,  --  secrets.svc, the trusted path, procmgr, devmgr
      Not_Held,          --  procmgr holds no Control of that process
      Kind_Withheld,     --  the envelope does not allow that debug kind
      Not_Declared,      --  no such startup entry to introduce into
      Capacity,          --  a named ceiling was reached
      Failed,            --  the kernel or a table refused
      --  The image's system policy (CCL.Interfaces.System_Policy):
      Debugging_Disabled,  --  debugging is Disabled for this image
      Approval_Needed);    --  Production: needs the owner's approval (not built)
   for Outcome use
     (Done => 1, Not_A_Handle => 2, Revoked => 3, Right_Missing => 4,
      Child_Ended => 5, Malformed => 6, Not_Yet => 7, Busy => 8,
      Not_Accepted => 9, Never_Debuggable => 10, Not_Held => 11,
      Kind_Withheld => 12, Not_Declared => 13, Capacity => 14, Failed => 15,
      Debugging_Disabled => 16, Approval_Needed => 17);

   function Outcome_Word (What : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (What)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word in Outcome_Word (Outcome'First) .. Outcome_Word (Outcome'Last));

   function To_Outcome (Word : Unsigned_64) return Outcome is
     (case Word is
        when 1 => Done, when 2 => Not_A_Handle, when 3 => Revoked,
        when 4 => Right_Missing, when 5 => Child_Ended, when 6 => Malformed,
        when 7 => Not_Yet, when 8 => Busy, when 9 => Not_Accepted,
        when 10 => Never_Debuggable, when 11 => Not_Held, when 12 => Kind_Withheld,
        when 13 => Not_Declared, when 14 => Capacity, when 15 => Failed,
        when 16 => Debugging_Disabled, when others => Approval_Needed)
   with Pre  => Is_Outcome_Word (Word),
        Post => Outcome_Word (To_Outcome'Result) = Word;

   --  Observe's reply: (1) = the child's identity word, (2) = its state,
   --  (3) = its exit code (Exited only).
   type Child_State is (Running, Exited, Stopped);
   for Child_State use (Running => 1, Exited => 2, Stopped => 3);
   function State_Word (S : Child_State) return Unsigned_64 is
     (Unsigned_64 (Child_State'Enum_Rep (S)));

   --  Control_Op's request word.
   type Control_Request is (Ask_Stop, Ask_Interrupt, Ask_Reload, End_Now);
   for Control_Request use (Ask_Stop => 1, Ask_Interrupt => 2, Ask_Reload => 3, End_Now => 4);
   function Control_Word (C : Control_Request) return Unsigned_64 is
     (Unsigned_64 (Control_Request'Enum_Rep (C)));
   function Is_Control_Word (Word : Unsigned_64) return Boolean is
     (Word in Control_Word (Control_Request'First) .. Control_Word (Control_Request'Last));
   function To_Control (Word : Unsigned_64) return Control_Request is
     (case Word is
        when 1 => Ask_Stop, when 2 => Ask_Interrupt, when 3 => Ask_Reload,
        when others => End_Now)
   with Pre => Is_Control_Word (Word), Post => Control_Word (To_Control'Result) = Word;

   --  How a debug session was minted (Debug_Records_Operation).
   type Debug_Path is (Launch_Handle_Path, Session_Authority_Path);
   for Debug_Path use (Launch_Handle_Path => 1, Session_Authority_Path => 2);

   --  Record_Word: bits 0 .. 7 the Debug_ABI kind word, 8 .. 15 the path,
   --  bit 16 set when more records follow.
   Record_Kind_Shift : constant := 0;
   Record_Path_Shift : constant := 8;
   Record_More_Bit   : constant Unsigned_64 := 2 ** 16;
   Record_Field_Mask : constant Unsigned_64 := 16#FF#;

   function Record_Word (Kind_Word : Unsigned_64; Path : Debug_Path; More : Boolean)
     return Unsigned_64 is
     ((Kind_Word and Record_Field_Mask)
      or Shift_Left (Unsigned_64 (Debug_Path'Enum_Rep (Path)), Record_Path_Shift)
      or (if More then Record_More_Bit else 0));

end CuBit.Launch_Handles;
