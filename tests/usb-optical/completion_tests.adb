with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Completions; use XHCI_Completions;

procedure Completion_Tests is
   State : Mailboxes;
   Result : Route_Result;
   Item : Event;
   Found : Boolean;
   function Transfer (Slot, Endpoint : Natural; Sequence : Unsigned_32)
      return Event is
     ((ParameterLo => Sequence, ParameterHi => 0, Status => 16#0100_0000#,
       Control => Shift_Left (Unsigned_32 (Slot), 24) or
         Shift_Left (Unsigned_32 (Endpoint), 16) or Shift_Left (32, 10)));
begin
   Clear (State);
   --  Fill every endpoint independently. Both success and error completions
   --  belong to their original endpoint, not whichever class is waiting.
   for Slot in Slot_Number range 1 .. Maximum_Slots loop
      for Endpoint in Endpoint_Number range 1 .. 31 loop
         for Sequence in 1 .. Queue_Depth loop
            Route (State, Transfer (Slot, Endpoint, Unsigned_32 (Sequence)), Result);
            pragma Assert (Result = Queued);
         end loop;
         Route (State, Transfer (Slot, Endpoint, 999), Result);
         pragma Assert (Result = Queue_Full);
         pragma Assert (Pending (State, Slot, Endpoint) = Queue_Depth);
      end loop;
   end loop;
   --  A command completion must still fit while all transfer queues are full.
   Route (State, (1, 2, 3, Shift_Left (33, 10)), Result);
   pragma Assert (Result = Queued);
   Take (State, 0, 0, Item, Found);
   pragma Assert (Found and Item.ParameterLo = 1);
   for Slot in reverse Slot_Number range 1 .. Maximum_Slots loop
      for Endpoint in reverse Endpoint_Number range 1 .. 31 loop
         for Sequence in 1 .. Queue_Depth loop
            Take (State, Slot, Endpoint, Item, Found);
            pragma Assert (Found and Item =
              Transfer (Slot, Endpoint, Unsigned_32 (Sequence)));
         end loop;
         Take (State, Slot, Endpoint, Item, Found);
         pragma Assert (not Found and Item = (0, 0, 0, 0));
      end loop;
   end loop;
   --  Interleave mouse, EP0, and storage repeatedly across queue wraparound.
   for Sequence in Unsigned_32 range 1 .. 1000 loop
      Route (State, Transfer (1, 3, Sequence), Result);
      pragma Assert (Result = Queued);
      Route (State, Transfer (2, 1, Sequence + 10), Result);
      pragma Assert (Result = Queued);
      Route (State, Transfer (2, 5, Sequence + 20), Result);
      pragma Assert (Result = Queued);
      Take (State, 2, 5, Item, Found);
      pragma Assert (Found and Item.ParameterLo = Sequence + 20);
      pragma Assert (Pending (State, 1, 3) = 1 and Pending (State, 2, 1) = 1);
      Take (State, 1, 3, Item, Found);
      pragma Assert (Found and Item.ParameterLo = Sequence);
      Take (State, 2, 1, Item, Found);
      pragma Assert (Found and Item.ParameterLo = Sequence + 10);
   end loop;
   for Slot in 0 .. 255 loop
      Route (State, Transfer (Slot, 1, 0), Result);
      if Slot in 1 .. Maximum_Slots then
         pragma Assert (Result = Queued);
      else
         pragma Assert (Result = Invalid_Target);
      end if;
   end loop;
   Route (State, Transfer (1, 0, 0), Result);
   pragma Assert (Result = Invalid_Target);
   Route (State, (0, 0, 0, Shift_Left (34, 10)), Result);
   pragma Assert (Result = Not_A_Completion);
   Clear (State);
   pragma Assert (Pending (State, 1, 1) = 0);
   Ada.Text_IO.Put_Line ("XHCI-COMPLETIONS: PASS isolated bounded mailboxes");
end Completion_Tests;
