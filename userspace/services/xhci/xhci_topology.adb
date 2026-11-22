package body XHCI_Topology with SPARK_Mode is
   function Root (Port : Port_Number; Rate : Speed) return Path is
     ((Root => Port, Rate => Rate, others => <>));
   function Root_Port (Item : Path) return Port_Number is (Item.Root);
   function Device_Speed (Item : Path) return Speed is (Item.Rate);
   function Route_String (Item : Path) return Unsigned_32 is
     (Unsigned_32 (Item.Ports (1))
      + Unsigned_32 (Item.Ports (2)) * 16
      + Unsigned_32 (Item.Ports (3)) * 256
      + Unsigned_32 (Item.Ports (4)) * 4096
      + Unsigned_32 (Item.Ports (5)) * 65536);
   function TT_Context (Item : Path) return Unsigned_32 is
     (Unsigned_32 (Item.TT_Slot) + Unsigned_32 (Item.TT_Port) * 256);

   procedure Child
     (Parent : Path; Parent_Slot : Slot_Number; Port : Port_Number;
      Rate : Speed; Item : out Path; Result : out Attach_Result) is
   begin
      Item := Root (Parent.Root, Rate);
      if Parent.Depth = Route_Depth'Last then
         Result := Depth_Exceeded;
      elsif Parent.Rate in Low_Speed | Super_Speed or else
        Rate = Super_Speed or else
        (Parent.Rate = Full_Speed and then Rate = High_Speed)
      then
         Result := Unsupported_Speed;
      else
         Item := Parent;
         Item.Rate := Rate;
         Item.Depth := Parent.Depth + 1;
         -- xHCI route fields encode downstream ports >=15 as 15.
         -- TT port retains the actual port number, not this clamped nibble.
         Item.Ports (Item.Depth) := Natural'Min (Port, 15);
         if Parent.Rate = High_Speed and then Rate in Full_Speed | Low_Speed then
            Item.TT_Slot := Parent_Slot;
            Item.TT_Port := Port;
         end if;
         Result := Attached;
      end if;
   end Child;
end XHCI_Topology;
