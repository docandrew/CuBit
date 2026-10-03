package body Intel_GPU_Deferred_Retirement with SPARK_Mode => Off is
   function Capacity (Object : Queue) return Positive is
     (Records.Capacity (Object.Items));
   function Item_At (Object : Queue; Index : Slot) return Candidate is
     (if Index <= Capacity (Object) then Records.Get (Object.Items, Index).Item
      else (others => 0));
   function Next_Slot (Object : Queue) return Slot is
     (if Object.Head = 0 then Slot'First else Object.Head);
   procedure Extend_Storage
     (Object : in out Queue; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Records.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend_Storage;
   procedure Remember (Object : in out Queue; Index : Slot; Item : Candidate) is
      Previous : Entry_Record;
      Accepted : Candidate;
   begin
      if Index > Capacity (Object) then return; end if;
      Previous := Records.Get (Object.Items, Index);
      Accepted := After_Remember (Previous.Item, Index, Item);
      if Accepted = Previous.Item then return; end if;
      Records.Put (Object.Items, Index, (Previous with delta Item => Accepted));
      if Previous.Item.Ticket = 0 then
         if Object.Tail = 0 then Object.Head := Index;
         else
            Records.Put (Object.Items, Object.Tail,
              (Records.Get (Object.Items, Object.Tail) with delta Next => Index));
         end if;
         Object.Tail := Index;
      end if;
   end Remember;
   procedure Poll (Object : in out Queue) is
      Index : Natural;
      Saved : Entry_Record;
      Result : Outcome;
   begin
      if Object.Head = 0 then return; end if;
      Index := Object.Head;
      Saved := Records.Get (Object.Items, Index);
      Result := Attempt (Index, Saved.Item);
      if Result = Waiting then
         if Saved.Next /= 0 then
            Object.Head := Saved.Next;
            Records.Put (Object.Items, Object.Tail,
              (Records.Get (Object.Items, Object.Tail) with delta Next => Index));
            Records.Put (Object.Items, Index, (Saved with delta Next => 0));
            Object.Tail := Index;
         end if;
      else
         Object.Head := Saved.Next;
         if Object.Head = 0 then Object.Tail := 0; end if;
         Records.Put (Object.Items, Index, (others => <>));
      end if;
   end Poll;
end Intel_GPU_Deferred_Retirement;
