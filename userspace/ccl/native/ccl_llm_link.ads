with Interfaces;
with System;
with CuBit.Messages;
with CCL_Llm;

--  The native side of CCL_Llm: the exchanges this program follows, each
--  with the window it lends llm.svc, and the Pull completions that the
--  desktop platform's event loop delivers here (Deliver).
--
--  Nothing here is fixed-size but the number of exchanges (LIM-001): each
--  window is allocated per ask on CuAlloc, page-aligned and sized from the
--  request (CuBit.Llm_Protocol.Minimum_Window_Bytes .. Maximum_Window_Bytes),
--  and a captured exchange's content grows on the heap as its pieces
--  arrive (Reply_Content), up to CCL_Llm.MAXIMUM_CONTENT.
package CCL_Llm_Link is
   use type Interfaces.Unsigned_64;
   subtype Slot_Index is Natural range 0 .. CCL_Llm.MAXIMUM_EXCHANGES - 1;

   --  A completion this package submitted? Then it is taken (Consumed).
   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);

   type Slot_State is record
      Used : Boolean := False;
      Exchange : Interfaces.Unsigned_64 := 0;
      Pull_Pending : Boolean := False;
      --  Text_Ready bytes of text at the window's start, or the summary.
      Text_Ready : Natural := 0;
      Done : Boolean := False;
   end record;

   --  A window of Bytes (whole pages) for Slot, zeroed; Opened False when
   --  the heap has no room for it.
   procedure Open_Window (Slot : Slot_Index; Bytes : Positive; Opened : out Boolean);
   --  Wipe and free Slot's window and content (if not taken).
   procedure Close_Window (Slot : Slot_Index);
   function Window_Address (Slot : Slot_Index) return System.Address;
   function Window_Size (Slot : Slot_Index) return Natural;

   --  The content received so far (taken: the caller owns it), and
   --  whether some was lost (beyond the ceiling, or no memory).
   procedure Take_Content (Slot : Slot_Index; Content : out CCL_Llm.Text_Access; Length : out Natural);
   function Content_Lost (Slot : Slot_Index) return Boolean;

   function State (Slot : Slot_Index) return Slot_State;
   procedure Set_State (Slot : Slot_Index; Item : Slot_State);
   --  Submit the next Pull for Slot (when none is pending and nothing waits).
   procedure Pull (Slot : Slot_Index);
end CCL_Llm_Link;
