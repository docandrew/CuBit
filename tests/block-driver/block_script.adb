package body Block_Script is
   procedure Push (Request : Message; Sender : Process_ID := 42) is
   begin
      Queued := Queued + 1;
      Queue (Queued) := (Sender, Request);
   end Push;
end Block_Script;
