package body XHCI_Rings with SPARK_Mode => On is

   procedure Advance (P : in out Producer; Placed : out Placement) is
   begin
      Placed := (Index => P.Tail, Cycle => P.Cycle,
                 Link_After => P.Tail = Enqueue_Index'Last, Link_Cycle => P.Cycle);
      if P.Tail = Enqueue_Index'Last then
         P.Tail := 0;
         P.Cycle := P.Cycle + 1;
      else
         P.Tail := P.Tail + 1;
      end if;
   end Advance;

   procedure Consume (C : in out Consumer) is
   begin
      if C.Head = Event_Index'Last then
         C.Head := 0;
         C.Cycle := C.Cycle + 1;
      else
         C.Head := C.Head + 1;
      end if;
   end Consume;

end XHCI_Rings;
