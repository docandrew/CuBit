with Ada.Text_IO;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Poll_Budget_Tests is
   function Ready return Boolean is (True);
   function Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is (7);
   procedure Recipient (Sender, Stamp : Unsigned_64;
     Slot : out CuBit.Messages.CapabilitySlot; Identity : out Unsigned_64) is
   begin Slot := 7; Identity := 42; end Recipient;
   package B is new Intel_GPU_Buffer_Requests (Session, Ready);
   package M is new B.Sharing (Recipient);
   package G renames CuBit.Memory_Grants;
   use type M.Mapping_ID, M.Retirement_State;
   use type Intel_GPU_Buffer_Views.View_State;
   Object : B.Service;
   Table : M.Mapping_Table;
   Reply : B.Words;
   Ticket : B.Ticket;
   OK : Boolean;
   ID, Wire : Unsigned_64;
   Maps : array (1 .. 33) of M.Mapping_ID;
   State : Intel_GPU_Buffer_Views.View_State;
   Before : Natural;
begin
   M.Poll (Object, Table);
   B.Handle (Object, 42, 99, B.Label, 4, 0, 0, [1, B.Create, 4096, 0], Reply, Ticket);
   B.Complete (Object, Ticket, Intel_GPU_Buffer_Reply.From_Linear
     (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Reply, OK);
   pragma Assert (OK and Reply (0) = B.OK);
   ID := Reply (2);
   for I in Maps'Range loop
      M.Map (Object, Table, 42, 99, ID, 0, 4096, False, Maps (I), Wire);
      pragma Assert (Maps (I) /= 0);
      M.Retire (Object, Table, 42, 99, Maps (I), OK, State);
   end loop;
   G.Gone := True;
   for Pass in 1 .. 3 loop
      Before := G.Retirement_Queries;
      M.Poll (Object, Table);
      pragma Assert (G.Retirement_Queries - Before = (if Pass < 3 then 16 else 1));
      pragma Assert (M.Observe_Retirement (Table, 7) =
        (if Pass < 3 then M.Outstanding else M.Clear));
   end loop;
   Ada.Text_IO.Put_Line ("Bounded poll PASS: empty table, 33 readers, 16/16/1 acknowledgements, eventual drain");
   declare
      Pool : B.Service;
      Queue : M.Mapping_Table;
      Last_Map : M.Mapping_ID;
   begin
      G.Gone := False;
      G.Completed_Wire := 0;
      B.Handle (Pool, 42, 99, B.Label, 4, 0, 0, [1, B.Create, 4096, 0], Reply, Ticket);
      B.Complete (Pool, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Reply, OK);
      pragma Assert (OK and Reply (0) = B.OK);
      ID := Reply (2);
      for I in 1 .. 53 loop
         M.Map (Pool, Queue, 42, 99, ID, 0, 4096, False, Last_Map, Wire);
         pragma Assert (Last_Map /= 0);
         M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
      end loop;
      G.Completed_Wire := Wire; -- only last reader has drained
      for Pass in 1 .. 4 loop
         Before := G.Retirement_Queries;
         M.Poll (Pool, Queue);
         pragma Assert (G.Retirement_Queries - Before <= 16);
      end loop;
      G.Completed_Wire := 0;
      M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
      pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retired);
      pragma Assert (M.Observe_Retirement (Queue, 7) = M.Outstanding);
      G.Gone := True;
      for Pass in 1 .. 4 loop
         Before := G.Retirement_Queries;
         M.Poll (Pool, Queue);
         pragma Assert (G.Retirement_Queries - Before <= 16);
      end loop;
      pragma Assert (M.Observe_Retirement (Queue, 7) = M.Clear);
      Ada.Text_IO.Put_Line ("Bounded poll fairness PASS: 52 stalled readers do not starve last reader, wraparound drains all");
   end;
   declare
      Pool : B.Service;
      Queue : M.Mapping_Table;
      type Storage_Array is array (1 .. 8192) of Unsigned_8;
      Storage : aliased Storage_Array := [others => 16#A5#];
      for Storage'Alignment use 4096;
      Base : constant Unsigned_64 := Unsigned_64
        (System.Storage_Elements.To_Integer (Storage'Address));
      First_Map, Last_Map : M.Mapping_ID;
      Last_Wire : Unsigned_64;
      Capacity : Positive;
   begin
      G.Gone := False;
      G.Completed_Wire := 0;
      B.Handle (Pool, 42, 99, B.Label, 4, 0, 0, [1, B.Create, 4096, 0], Reply, Ticket);
      B.Complete (Pool, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Reply, OK);
      pragma Assert (OK and Reply (0) = B.OK);
      ID := Reply (2);
      for I in 1 .. M.Initial_Capacity loop
         M.Map (Pool, Queue, 42, 99, ID, 0, 4096, False, Last_Map, Wire);
         pragma Assert (Last_Map /= 0);
         if I = 1 then First_Map := Last_Map; end if;
         M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
         pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retiring);
      end loop;
      pragma Assert (M.Needs_Growth (Queue));
      M.Poll (Pool, Queue); -- Leave the cursor partway through old storage.
      M.Extend_Storage (Queue, Base, 4096, OK);
      pragma Assert (OK and M.Record_Capacity (Queue) > M.Initial_Capacity);
      Capacity := M.Record_Capacity (Queue);
      M.Map (Pool, Queue, 42, 99, ID, 0, 4096, False, Last_Map, Last_Wire);
      pragma Assert (Last_Map /= 0 and Last_Wire /= 0);
      M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
      pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retiring);
      M.Extend_Storage (Queue, Base, 8192, OK);
      pragma Assert (OK and M.Record_Capacity (Queue) > Capacity);
      -- Neither inline nor previously initialized extension entries may reset.
      M.Retire (Pool, Queue, 42, 99, First_Map, OK, State);
      pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retiring);
      M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
      pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retiring);
      G.Completed_Wire := Last_Wire;
      for Pass in 1 .. (M.Initial_Capacity + 1 + M.Poll_Budget - 1) / M.Poll_Budget loop
         Before := G.Retirement_Queries;
         M.Poll (Pool, Queue);
         pragma Assert (G.Retirement_Queries - Before <= M.Poll_Budget);
      end loop;
      G.Completed_Wire := 0;
      M.Retire (Pool, Queue, 42, 99, Last_Map, OK, State);
      pragma Assert (OK and State = Intel_GPU_Buffer_Views.Retired);
      pragma Assert (M.Observe_Retirement (Queue, 7) = M.Outstanding);
      G.Gone := True;
      for Pass in 1 .. (M.Initial_Capacity + 1 + M.Poll_Budget - 1) / M.Poll_Budget loop
         M.Poll (Pool, Queue);
      end loop;
      pragma Assert (M.Observe_Retirement (Queue, 7) = M.Clear);
      Ada.Text_IO.Put_Line ("Bounded poll growth PASS: pending cursor, two extensions, stable grants and eventual drain");
   end;
end Poll_Budget_Tests;
