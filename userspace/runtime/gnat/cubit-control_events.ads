------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel's grant events and control messages (docs/data-plane.md,
--  "The unit is the grant" and "Control messages"), as they arrive in a
--  process's event lane. Must match kernel/src/ipc_labels.ads (the libc's
--  tests/libc-ada checks the numbers).
--
--    EVENT_GRANT_REVOKED, to a grantee: words (0) = the grant's global slot,
--      (1) = its generation, (2) = the owner's PID. Return the mapping.
--    EVENT_GRANT_RETURNED, to an owner: the same words, (2) = the grantee's
--      PID. The pages are the owner's again.
--    EVENT_CONTROL: words (0) = the kind, (1) = the sender's PID.
--
--  Events are hints: a full event lane drops them. The grant's generation
--  (or a failed acquire) is authoritative.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Control_Events with Pure, SPARK_Mode is

   Grant_Revoked_Label  : constant := 16#010C#;
   Grant_Returned_Label : constant := 16#010D#;
   Control_Label        : constant := 16#010E#;

   Grant_Event_Words : constant := 3;
   Control_Words     : constant := 2;

   --  Stop: finish up and exit (like SIGTERM). Interrupt: abandon the
   --  current operation (like SIGINT). Reload: re-read configuration
   --  (like SIGHUP).
   type Control_Kind is (Stop, Interrupt, Reload);
   for Control_Kind use (Stop => 1, Interrupt => 2, Reload => 3);

   type Event_Kind is (Grant_Revoked, Grant_Returned, Control, Not_Ours);

   type Event (Kind : Event_Kind := Not_Ours) is record
      case Kind is
         when Grant_Revoked | Grant_Returned =>
            Slot       : Unsigned_64;
            Generation : Unsigned_64;
            Peer       : Unsigned_64;
         when Control =>
            Control    : Control_Kind;
            Sender     : Unsigned_64;
         when Not_Ours =>
            null;
      end case;
   end record;

   function Is_Control_Kind (Value : Unsigned_64) return Boolean is
     (for some K in Control_Kind => Value = Control_Kind'Enum_Rep (K));

   function To_Control_Kind (Value : Unsigned_64) return Control_Kind is
     (if Value = Control_Kind'Enum_Rep (Stop) then Stop
      elsif Value = Control_Kind'Enum_Rep (Interrupt) then Interrupt
      else Reload)
   with Pre => Is_Control_Kind (Value);

   --  A message's label, length and first three words, as one of these
   --  events; Not_Ours for anything else or anything malformed.
   function Decode (Label : Unsigned_32; Length : Unsigned_8; W0, W1, W2 : Unsigned_64)
     return Event is
     (if Label = Grant_Revoked_Label and then Length = Grant_Event_Words then
         (Kind => Grant_Revoked, Slot => W0, Generation => W1, Peer => W2)
      elsif Label = Grant_Returned_Label and then Length = Grant_Event_Words then
         (Kind => Grant_Returned, Slot => W0, Generation => W1, Peer => W2)
      elsif Label = Control_Label and then Length = Control_Words
        and then Is_Control_Kind (W0)
      then
         (Kind => Control, Control => To_Control_Kind (W0), Sender => W1)
      else (Kind => Not_Ours));

end CuBit.Control_Events;
