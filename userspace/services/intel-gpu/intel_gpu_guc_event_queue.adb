package body Intel_GPU_GuC_Event_Queue with SPARK_Mode is
   procedure Push (Object : in out Queue; Item : Event; Accepted : out Boolean) is
   begin
      Accepted := Object.Count < Capacity;
      if not Accepted then return; end if;
      Object.Items (Index (Object.First, Object.Count + 1)) := Item;
      Object.Count := Object.Count + 1;
      if Object.Count > Object.Most then Object.Most := Object.Count; end if;
   end Push;

   procedure Pop (Object : in out Queue; Item : out Event; Found : out Boolean) is
   begin
      Item := (others => <>);
      Found := Object.Count > 0;
      if not Found then return; end if;
      Item := Object.Items (Object.First);
      Object.First := (if Object.First = Slot'Last then Slot'First else Object.First + 1);
      Object.Count := Object.Count - 1;
   end Pop;
end Intel_GPU_GuC_Event_Queue;
