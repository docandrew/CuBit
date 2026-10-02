package body Intel_GPU_Deferred_Retirement with SPARK_Mode is
   function Snapshot (Object : Queue) return Entries is (Object.Items);
   function Next_Slot (Object : Queue) return Slot is (Object.Next_Index);
   procedure Remember (Object : in out Queue; Index : Slot; Item : Candidate) is
   begin
      if not Eligible (Index, Item)
      then return; end if;
      if Object.Items (Index).Ticket /= 0 and then
        Item.Ticket <= Object.Items (Index).Ticket
      then return; end if;
      Object.Items (Index) := Item;
   end Remember;
   procedure Poll (Object : in out Queue) is
      Index : constant Slot := Object.Next_Index;
   begin
      Object.Next_Index := (if Index = Slot'Last then Slot'First else Index + 1);
      if Object.Items (Index).Ticket /= 0 and then
        Attempt (Index, Object.Items (Index)) /= Waiting
      then
         Object.Items (Index) := (others => 0);
      end if;
   end Poll;
end Intel_GPU_Deferred_Retirement;
