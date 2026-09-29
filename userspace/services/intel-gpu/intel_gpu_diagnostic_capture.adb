package body Intel_GPU_Diagnostic_Capture with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   function Count (Item : Queue) return Natural is (Item.Used);
   function Lost (Item : Queue) return Interfaces.Unsigned_64 is (Item.Dropped);
   function Latest (Item : Queue) return Captured_Record is (Item.Last);
   function Next (Index : Slot) return Slot is
     (if Index = Slot'Last then 0 else Index + 1);
   procedure Append (Item : in out Queue; Text : String) is
      Tail : Slot;
      Value : Captured_Record;
   begin
      Value.Length := Text'Length;
      Value.Text (1 .. Value.Length) := Text;
      Item.Last := Value;
      if Item.Used = Capacity then
         Item.Head := Next (Item.Head);
         Item.Used := Item.Used - 1;
         if Item.Dropped < Interfaces.Unsigned_64'Last then
            Item.Dropped := Item.Dropped + 1;
         end if;
      end if;
      -- Avoid Head+Used overflow even for a large generic capacity.
      Tail := (if Item.Used >= Capacity - Item.Head then
                 Item.Used - (Capacity - Item.Head)
               else Item.Head + Item.Used);
      Item.Data (Tail) := Value;
      Item.Used := Item.Used + 1;
   end Append;
   procedure Take (Item : in out Queue; Value : out Captured_Record; Found : out Boolean) is
   begin
      Value := (others => <>);
      Found := Item.Used > 0;
      if Found then
         Value := Item.Data (Item.Head);
         Item.Head := Next (Item.Head);
         Item.Used := Item.Used - 1;
      end if;
   end Take;
end Intel_GPU_Diagnostic_Capture;
