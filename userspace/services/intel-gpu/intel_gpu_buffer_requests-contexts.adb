package body Intel_GPU_Buffer_Requests.Contexts is
   procedure Reserve
     (Object : in out Service; Session : Unsigned_64; ID : out Ticket;
      Pages : Intel_GPU_Buffer_Backing.Page_Count) is
   begin
      ID := 0;
      if Session = 0 or else Object.Failed or else not Owner_Ready or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0
      then return; end if;
      if Object.Context_Reusable_Head /= 0 then
         declare Index : constant Natural := Object.Context_Reusable_Head; begin
         if Index > Committed_Slots (Object) then Quarantine (Object); return; end if;
         declare Item : constant Allocation_Record := Records.Get (Object.Items, Index); begin
            if not Item.Context_Parent or else not Item.Context_Closed or else
              not Item.Context_Reusable or else Item.Reusable or else Item.Private_Reusable or else
              Item.Private_Reclaimable or else Item.Issued.Handle /= 0 or else
              Item.Identity = 0 or else Ticket_Slot (Item.Identity) /= Index or else
              Item.Identity > Ticket'Last - Ticket_Stride or else Item.Charge_Bytes /= 0 or else
              Item.Next_Reusable = Index or else Item.Next_Reusable > Committed_Slots (Object)
            then Quarantine (Object); return; end if;
               if not Charge_Client (Object, Session, Unsigned_64 (Pages) * 4096) then return; end if;
               Object.Context_Reusable_Head := Item.Next_Reusable;
               ID := Item.Identity + Ticket_Stride;
               Records.Put (Object.Items, Index, (Item with delta
                 Identity => ID, Owner => Session, Context_Closed => False,
                 Context_Reusable => False, Next_Reusable => 0,
                 Charge_Bytes => Unsigned_64 (Pages) * 4096));
               Object.Private_Pending := ID;
               return;
         end;
         end;
      end if;
      Reserve_Private (Object, Session, ID, Pages => Pages);
      if ID /= 0 then
         Records.Put (Object.Items, Ticket_Slot (ID),
           (Records.Get (Object.Items, Ticket_Slot (ID)) with delta Context_Parent => True));
      end if;
   end Reserve;
   function Can_Retire
     (Object : Service; Session : Unsigned_64; ID : Ticket) return Boolean is
      Index : constant Intel_GPU_Buffer_Backing.Slot := Ticket_Slot (ID);
   begin
      if Session = 0 or else ID = 0 or else ID > Ticket'Last - Ticket_Stride or else
        Object.Failed or else not Owner_Ready or else Index > Committed_Slots (Object) or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0
      then return False; end if;
      declare Item : constant Allocation_Record := Records.Get (Object.Items, Index); begin
         return Item.Identity = ID and then Item.Owner = Session and then
           Item.Context_Parent and then Item.Context_Closed and then not Item.Context_Reusable
           and then not Item.Reusable and then not Item.Private_Reusable
           and then not Item.Private_Reclaimable and then Item.Issued.Handle = 0;
      end;
   end Can_Retire;
   procedure Acknowledge
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not References_Retired or else not Can_Retire (Object, Session, ID) then return; end if;
      if not Refund_Client (Object, Ticket_Slot (ID)) then return; end if;
      Records.Put (Object.Items, Ticket_Slot (ID),
        (Records.Get (Object.Items, Ticket_Slot (ID)) with delta Context_Reusable => True,
         Charge_Bytes => 0, Next_Reusable => Object.Context_Reusable_Head));
      Object.Context_Reusable_Head := Ticket_Slot (ID);
      Accepted := True;
   end Acknowledge;
end Intel_GPU_Buffer_Requests.Contexts;
