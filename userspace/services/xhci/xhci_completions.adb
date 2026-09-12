package body XHCI_Completions with SPARK_Mode => On is
   function Next_Position (Position : Queue_Position) return Queue_Position is
     (if Position = Queue_Position'Last then Queue_Position'First
      else Position + 1);

   procedure Clear (State : out Mailboxes) is
   begin
      State := (Queues => [others => [others => (others => <>)]]);
   end Clear;

   procedure Route
     (State : in out Mailboxes; Item : Event; Result : out Route_Result)
   is
      Kind : constant Unsigned_32 := Shift_Right (Item.Control, 10) and 63;
      Slot : Slot_Number;
      Endpoint : Endpoint_Number;
   begin
      case Kind is
         when 32 =>
            if Shift_Right (Item.Control, 24) not in 1 .. Maximum_Slots or else
               (Shift_Right (Item.Control, 16) and 31) = 0
            then
               Result := Invalid_Target;
               return;
            end if;
            Slot := Slot_Number (Shift_Right (Item.Control, 24));
            Endpoint := Endpoint_Number (Shift_Right (Item.Control, 16) and 31);
         when 33 =>
            Slot := 0;
            Endpoint := 0;
         when others =>
            Result := Not_A_Completion;
            return;
      end case;
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
end XHCI_Completions;
