with Ada.Text_IO; use Ada.Text_IO;
with CCL.Callback_Queues;
with Queue_Model;
procedure Queue_Tests is
   package Q renames Queue_Model;
   use Q;
   Item : Queue;
   Target, Old_Target, Invalid_Target : Reference;
   Ticket, Old_Ticket, Invalid_Ticket : Invocation;
   Opened_Status : Open_Result;
   Enqueued_Status : Enqueue_Result;
   Ready, Accepted : Boolean;
   Discarded : Pending_Count;
begin
   Enqueue (Item, Invalid_Target, Enqueued_Status);
   pragma Assert (Enqueued_Status = Stale_Target and State (Item) = Empty);
   Claim (Item, Ticket, Ready); pragma Assert (not Ready);
   Open (Item, Target, Opened_Status); pragma Assert (Opened_Status = Opened);
   Old_Target := Target;
   for I in 1 .. Capacity loop
      Enqueue (Item, Target, Enqueued_Status); pragma Assert (Enqueued_Status = Enqueued);
   end loop;
   Enqueue (Item, Target, Enqueued_Status);
   pragma Assert (Enqueued_Status = Queue_Full and Pending (Item) = Capacity);
   for I in 1 .. Capacity loop
      Claim (Item, Ticket, Ready); pragma Assert (Ready and State (Item) = Executing);
      Claim (Item, Invalid_Ticket, Ready); pragma Assert (not Ready);
      Complete (Item, Invalid_Ticket, True, Accepted, Discarded);
      pragma Assert (not Accepted and State (Item) = Executing);
      Complete (Item, Ticket, True, Accepted, Discarded);
      pragma Assert (Accepted and Discarded = 0 and State (Item) = Listening);
      Complete (Item, Ticket, True, Accepted, Discarded); pragma Assert (not Accepted);
   end loop;
   --  Ring wrap with interleaved production/consumption: no click coalescing.
   for I in 1 .. 100 loop
      Enqueue (Item, Target, Enqueued_Status); pragma Assert (Enqueued_Status = Enqueued);
      Claim (Item, Ticket, Ready); pragma Assert (Ready);
      Enqueue (Item, Target, Enqueued_Status); pragma Assert (Enqueued_Status = Enqueued);
      Complete (Item, Ticket, True, Accepted, Discarded); pragma Assert (Accepted);
      Old_Ticket := Ticket;
      Claim (Item, Ticket, Ready); pragma Assert (Ready);
      Complete (Item, Old_Ticket, True, Accepted, Discarded);
      pragma Assert (not Accepted and State (Item) = Executing);
      Complete (Item, Ticket, True, Accepted, Discarded); pragma Assert (Accepted);
   end loop;
   Enqueue (Item, Target, Enqueued_Status);
   Claim (Item, Ticket, Ready); pragma Assert (Ready);
   Enqueue (Item, Target, Enqueued_Status);
   Close (Item, Discarded);
   pragma Assert (Discarded = 1 and State (Item) = Draining and Pending (Item) = 0);
   Open (Item, Invalid_Target, Opened_Status); pragma Assert (Opened_Status = Busy);
   Enqueue (Item, Target, Enqueued_Status); pragma Assert (Enqueued_Status = Inactive);
   Complete (Item, Old_Ticket, True, Accepted, Discarded);
   pragma Assert (not Accepted and State (Item) = Draining);
   Complete (Item, Ticket, True, Accepted, Discarded);
   pragma Assert (Accepted and State (Item) = Stopped);
   Old_Ticket := Ticket;
   Open (Item, Target, Opened_Status); pragma Assert (Opened_Status = Opened);
   Enqueue (Item, Old_Target, Enqueued_Status); pragma Assert (Enqueued_Status = Stale_Target);
   Enqueue (Item, Target, Enqueued_Status);
   Claim (Item, Ticket, Ready); pragma Assert (Ready);
   Complete (Item, Old_Ticket, True, Accepted, Discarded); pragma Assert (not Accepted);
   Enqueue (Item, Target, Enqueued_Status);
   Complete (Item, Ticket, False, Accepted, Discarded);
   pragma Assert (Accepted and Discarded = 1 and State (Item) = Faulted);
   Enqueue (Item, Target, Enqueued_Status); pragma Assert (Enqueued_Status = Inactive);
   declare
      package Small is new CCL.Callback_Queues (Maximum_Identity => 2);
      use Small;
      Tiny : Small.Queue;
      R : Small.Reference;
      T : Small.Invocation;
      O : Small.Open_Result;
      E : Small.Enqueue_Result;
   begin
      for Generation in 1 .. 2 loop
         Small.Open (Tiny, R, O); pragma Assert (O = Small.Opened);
         for Sequence in 1 .. 2 loop
            Small.Enqueue (Tiny, R, E); pragma Assert (E = Small.Enqueued);
            Small.Claim (Tiny, T, Ready); pragma Assert (Ready);
            Small.Complete (Tiny, T, True, Accepted, Discarded); pragma Assert (Accepted);
         end loop;
         Small.Enqueue (Tiny, R, E); pragma Assert (E = Small.Identity_Exhausted);
         Small.Close (Tiny, Discarded);
      end loop;
      Small.Open (Tiny, R, O); pragma Assert (O = Small.Identity_Exhausted);
   end;
   Put_Line ("PASS: callback queue capacity, wrap, one active, stale/duplicate tickets, draining, failure, identity exhaustion");
end Queue_Tests;
