with XHCI_Registers; use XHCI_Registers;

package body XHCI_Completions with SPARK_Mode => On is
   function Next_Position (Position : Queue_Position) return Queue_Position is
     (if Position = Queue_Position'Last then Queue_Position'First
      else Position + 1);

   type Target_Kind is (Mailbox_Target, Not_Routed, Bad_Target);

   --  Which mailbox an event belongs to: a Transfer Event to its slot and
   --  endpoint, a Command Completion Event to (0, 0); others are not
   --  completions (the event ring's owner handles them).
   procedure Target
     (Item : Event; Kind : out Target_Kind;
      Slot : out Slot_Number; Endpoint : out Endpoint_Number)
   is
      Event_Type : constant TRB_Type := Type_Of (Item.Control);
   begin
      Slot := 0;
      Endpoint := 0;
      if Event_Type = TRB_TRANSFER_EVENT then
         if Slot_Of (Item.Control) not in 1 .. Maximum_Slots or else
            Endpoint_Of (Item.Control) = 0
         then
            Kind := Bad_Target;
         else
            Kind := Mailbox_Target;
            Slot := Slot_Number (Slot_Of (Item.Control));
            Endpoint := Endpoint_Number (Endpoint_Of (Item.Control));
         end if;
      elsif Event_Type = TRB_COMMAND_COMPLETION then
         Kind := Mailbox_Target;
      else
         Kind := Not_Routed;
      end if;
   end Target;

   procedure Clear (State : out Mailboxes) is
   begin
      State := (Queues => [others => [others => (others => <>)]]);
   end Clear;

   procedure Route
     (State : in out Mailboxes; Item : Event; Result : out Route_Result)
   is
      Kind : Target_Kind;
      Slot : Slot_Number;
      Endpoint : Endpoint_Number;
   begin
      Target (Item, Kind, Slot, Endpoint);
      case Kind is
         when Not_Routed =>
            Result := Not_A_Completion;
         when Bad_Target =>
            Result := Invalid_Target;
         when Mailbox_Target =>
            declare
               Selected_Slot : constant Slot_Number := Slot;
               Selected_Endpoint : constant Endpoint_Number := Endpoint;
               Queue : Mailbox renames State.Queues (Selected_Slot, Selected_Endpoint);
            begin
               if Queue.Count = Queue_Depth then
                  Result := Queue_Full;
                  return;
               end if;
               Queue.Items (Queue.Tail) := Item;
               Queue.Tail := Next_Position (Queue.Tail);
               Queue.Count := Queue.Count + 1;
               Result := Queued;
            end;
      end case;
   end Route;

   procedure Take
     (State : in out Mailboxes; Slot : Slot_Number;
      Endpoint : Endpoint_Number; Item : out Event; Found : out Boolean)
   is
      Queue : Mailbox renames State.Queues (Slot, Endpoint);
   begin
      Found := Queue.Count /= 0;
      Item := (others => 0);
      if Found then
         Item := Queue.Items (Queue.Head);
         Queue.Head := Next_Position (Queue.Head);
         Queue.Count := Queue.Count - 1;
      end if;
   end Take;

   function Pending
     (State : Mailboxes; Slot : Slot_Number; Endpoint : Endpoint_Number)
      return Natural is (State.Queues (Slot, Endpoint).Count);

   function Has_Room (State : Mailboxes; Item : Event) return Boolean is
      Kind : Target_Kind;
      Slot : Slot_Number;
      Endpoint : Endpoint_Number;
   begin
      Target (Item, Kind, Slot, Endpoint);
      return Kind /= Mailbox_Target or else
        State.Queues (Slot, Endpoint).Count < Queue_Depth;
   end Has_Room;

   procedure Clear_Slot (State : in out Mailboxes; Slot : Slot_Number) is
   begin
      for E in Endpoint_Number loop
         State.Queues (Slot, E) := (others => <>);
         pragma Loop_Invariant
           (for all F in Endpoint_Number'First .. E => State.Queues (Slot, F).Count = 0);
      end loop;
   end Clear_Slot;
end XHCI_Completions;
