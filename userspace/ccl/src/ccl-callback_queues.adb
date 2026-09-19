package body CCL.Callback_Queues with SPARK_Mode is
   procedure Open (Item : in out Queue; Target : out Reference; Result : out Open_Result) is
   begin
      Target := (others => <>);
      if Item.Phase in Listening | Executing | Draining then Result := Busy;
      elsif Item.Generation = Maximum_Identity then Result := Identity_Exhausted;
      else
         Item.Generation := Item.Generation + 1;
         Item.Issued := 0;
         Item.Head := Slot'First;
         Item.Count := 0;
         Item.Active := (others => <>);
         Item.Phase := Listening;
         Target := (Generation => Item.Generation);
         Result := Opened;
      end if;
   end Open;

   procedure Enqueue
     (Item : in out Queue; Target : Reference; Result : out Enqueue_Result) is
      Tail : Slot;
   begin
      if Target.Generation = 0 or else Target.Generation /= Item.Generation then Result := Stale_Target;
      elsif Item.Phase not in Listening | Executing then Result := Inactive;
      elsif Item.Count = Capacity then Result := Queue_Full;
      elsif Item.Issued = Maximum_Identity then Result := Identity_Exhausted;
      else
         Item.Issued := Item.Issued + 1;
         Tail := 1 + (Item.Head - 1 + Item.Count) mod Capacity;
         Item.Events (Tail) := Item.Issued;
         Item.Count := Item.Count + 1;
         Result := Enqueued;
      end if;
   end Enqueue;

   procedure Claim (Item : in out Queue; Ticket : out Invocation; Ready : out Boolean) is
   begin
      Ticket := (others => <>);
      Ready := Item.Phase = Listening and then Item.Count > 0;
      if Ready then
         Ticket := (Generation => Item.Generation, Sequence => Item.Events (Item.Head));
         Item.Head := (if Item.Head = Slot'Last then Slot'First else Item.Head + 1);
         Item.Count := Item.Count - 1;
         Item.Active := Ticket;
         Item.Phase := Executing;
      end if;
   end Claim;

   procedure Close (Item : in out Queue; Discarded : out Pending_Count) is
   begin
      Discarded := Item.Count;
      Item.Count := 0;
      case Item.Phase is
         when Executing | Draining => Item.Phase := Draining;
         when Empty => null;
         when others => Item.Phase := Stopped;
      end case;
   end Close;

   procedure Complete
     (Item : in out Queue; Ticket : Invocation; Succeeded : Boolean;
      Accepted : out Boolean; Discarded : out Pending_Count) is
   begin
      Accepted := Item.Phase in Executing | Draining and then
        Ticket.Generation = Item.Generation and then Ticket.Sequence /= 0 and then Ticket = Item.Active;
      Discarded := 0;
      if not Accepted then return; end if;
      Item.Active := (others => <>);
      if Item.Phase = Draining then Item.Phase := Stopped;
      elsif not Succeeded then
         Discarded := Item.Count;
         Item.Count := 0;
         Item.Phase := Faulted;
      else Item.Phase := Listening;
      end if;
   end Complete;
end CCL.Callback_Queues;
