with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_Event_Queue;
-- Hosted coverage of the proved retained-G2H-event FIFO (GPU-001 step 1).
procedure GuC_Event_Queue_Tests is
   package Q renames Intel_GPU_GuC_Event_Queue;
   use type Q.Event;
   Queue : Q.Queue;
   Item : Q.Event;
   Accepted, Found : Boolean;
   function Make (N : Natural) return Q.Event is
     ((Fence => Unsigned_16 (N mod 65536), Length => 1 + N mod Q.Max_Payload_Words,
       Words => [others => Unsigned_32 (N)]));
begin
   -- Sized for one whole G2H ring of minimal frames, beyond i915's reserve.
   pragma Assert (Q.Capacity = 2048);
   pragma Assert (Q.Capacity * Q.Minimum_Frame_Words = Q.G2H_Ring_Words);
   pragma Assert (Q.Unsolicited_Reserve_Words / Q.Minimum_Frame_Words <= Q.Capacity);
   Q.Pop (Queue, Item, Found);
   pragma Assert (not Found and Q.Length (Queue) = 0);
   -- Fill exactly; the next push is refused and changes nothing.
   for N in 1 .. Q.Capacity loop
      Q.Push (Queue, Make (N), Accepted);
      pragma Assert (Accepted and Q.Length (Queue) = N);
   end loop;
   Q.Push (Queue, Make (0), Accepted);
   pragma Assert (not Accepted and Q.Length (Queue) = Q.Capacity and Q.Peak (Queue) = Q.Capacity);
   pragma Assert (Q.Element (Queue, 1) = Make (1) and Q.Element (Queue, Q.Capacity) = Make (Q.Capacity));
   -- FIFO across many wraparounds with a sliding window of 700.
   for N in 1 .. 700 loop
      Q.Pop (Queue, Item, Found);
      pragma Assert (Found and Item = Make (N));
   end loop;
   for Round in 0 .. 20_000 loop
      Q.Push (Queue, Make (Q.Capacity + 1 + Round), Accepted);
      pragma Assert (Accepted);
      Q.Pop (Queue, Item, Found);
      pragma Assert (Found and Item = Make (701 + Round));
   end loop;
   while Q.Length (Queue) > 0 loop
      Q.Pop (Queue, Item, Found);
      pragma Assert (Found);
   end loop;
   pragma Assert (Item = Make (Q.Capacity + 1 + 20_000));
   -- Truncation is explicit: frames longer than the retained words say so.
   Item := Make (0); Item.Length := Q.Retained_Words;
   pragma Assert (not Q.Truncated (Item) and Q.Kept_Words (Item) = Q.Retained_Words);
   Item.Length := Q.Retained_Words + 1;
   pragma Assert (Q.Truncated (Item) and Q.Kept_Words (Item) = Q.Retained_Words);
   Item.Length := 3;
   pragma Assert (Q.Kept_Words (Item) = 3);
   Ada.Text_IO.Put_Line ("G2H event queue PASS: capacity" & Natural'Image (Q.Capacity) &
     ", refusal without loss, FIFO over 20001 wraparound steps, explicit truncation");
end GuC_Event_Queue_Tests;
