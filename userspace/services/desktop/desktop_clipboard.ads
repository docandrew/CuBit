------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop clipboard service glue: gestures, grant transfer, history
--
--  The clipboard lives inside Desktop (docs/clipboard.md): paste authority
--  depends on keyboard focus and on the chord Desktop itself routed, which
--  only Desktop knows. Apps call down to Desktop; Desktop never calls an app.
--  Policy is in the proved Clipboard_Store and Clipboard_Gestures; this unit
--  only maps the client's transfer window and logs metadata (never content,
--  and never a secret's length).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages;
with Clipboard_Store;

package Desktop_Clipboard with SPARK_Mode => Off is
   --  The history. Read it (through Clipboard_Store's functions) to draw the
   --  manager; change it only through the procedures below.
   History : Clipboard_Store.Store;

   --  True for the clipboard request labels.
   function Handles (Label : Unsigned_32) return Boolean;

   --  Answer one clipboard request. Focus is Desktop's keyboard-focus
   --  surface and Focus_Owner its owner: a request for any other surface, or
   --  from any other process, is denied before anything is mapped.
   function Handle
     (From        : CuBit.Messages.Process_ID;
      Request     : CuBit.Messages.Message;
      Focus       : Unsigned_64;
      Focus_Owner : CuBit.Messages.Process_ID;
      Now         : Unsigned_64) return CuBit.Messages.Message;

   --  Called for every key press Desktop routes to the focused surface;
   --  issues a single-use ticket for copy and paste chords, and a copy
   --  ticket for Enter (a menu row's activation), as Clipboard_Chords.
   procedure Note_Key
     (Surface : Unsigned_64; Code : Unsigned_8; Modifiers : Unsigned_64;
      Now : Unsigned_64);

   --  Called for every pointer press and release Desktop routes to a
   --  surface: a copy ticket (a menu's Copy, a Copy button), never a paste
   --  ticket. Redeeming it still needs that surface focused.
   procedure Note_Activation (Surface : Unsigned_64; Now : Unsigned_64);

   --  Input loss or resynchronization: no chord survives it.
   procedure Revoke_Gestures;

   --  The manager's own paste (Enter on an entry): make the entry current
   --  and give the focused surface a paste ticket, as its own chord would.
   procedure Paste_From_Manager
     (Slot : Clipboard_Store.Slot_Index; Focus : Unsigned_64;
      Now : Unsigned_64; Done : out Boolean);

   --  Housekeeping: wipe expired secrets. Returns True when something was
   --  wiped (the manager, if open, needs a repaint).
   function Expire (Now : Unsigned_64) return Boolean;
   --  Earliest secret deadline (ms), or Otherwise.
   function Next_Deadline (Otherwise : Unsigned_64) return Unsigned_64;

   procedure Toggle_Pin (Slot : Clipboard_Store.Slot_Index);
   procedure Delete (Slot : Clipboard_Store.Slot_Index);
   procedure Clear;
end Desktop_Clipboard;
