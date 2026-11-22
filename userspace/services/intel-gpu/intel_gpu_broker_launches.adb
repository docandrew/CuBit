package body Intel_GPU_Broker_Launches with SPARK_Mode is
   function State (Object : Ledger; ID : Ticket) return Phase is
     (if ID = 0 or else ID > Object.Used then Absent else Object.Items (ID).Current);
   function Nonce (Object : Ledger; ID : Ticket) return Unsigned_64 is
     (if ID = 0 or else ID > Object.Used then 0 else Object.Items (ID).Request_Nonce);
   function Identity (Object : Ledger; ID : Ticket) return Unsigned_64 is
     (if ID = 0 or else ID > Object.Used then 0 else Object.Items (ID).Captured);
   function Result (Object : Ledger; ID : Ticket) return Outcome is
     (if ID = 0 or else ID > Object.Used then Uncertain else Object.Items (ID).Completion);
   function Abort_Required (Object : Ledger; ID : Ticket) return Boolean is
     (ID /= 0 and then ID <= Object.Used and then Object.Items (ID).Must_Abort);
   function Count (Object : Ledger) return Ticket is (Object.Used);
   function Can_Reserve
     (Object : Ledger; Request : Intel_GPU_Broker_Request.Decoded) return Boolean is
     (Request.Valid and then Request.Nonce /= 0 and then
      Request.Identity /= 0 and then
      Object.Used < Capacity and then
      (for all I in 1 .. Object.Used =>
        Object.Items (I).Request_Nonce /= Request.Nonce and
        Object.Items (I).Captured /= Request.Identity));
   procedure Reserve
     (Object : in out Ledger; Request : Intel_GPU_Broker_Request.Decoded;
      ID : out Ticket) is
   begin
      ID := 0;
      if not Can_Reserve (Object, Request) then return; end if;
      Object.Used := Object.Used + 1;
      ID := Object.Used;
      Object.Items (ID) := (Pending, Request.Identity, Request.Nonce,
                           Uncertain, False);
   end Reserve;
   procedure Finish (Object : in out Ledger; ID : Ticket; Value : Outcome) is
   begin
      if State (Object, ID) /= Pending then return; end if;
      Object.Items (ID).Completion := Value;
      Object.Items (ID).Current := Reply_Ready;
   end Finish;
   procedure Take_Reply (Object : in out Ledger; ID : Ticket; Taken : out Boolean) is
   begin
      Taken := State (Object, ID) = Reply_Ready;
      if Taken then Object.Items (ID).Current := Reply_Taken; end if;
   end Take_Reply;
   procedure Delivered (Object : in out Ledger; ID : Ticket; Sent : Boolean) is
   begin
      if State (Object, ID) /= Reply_Taken then return; end if;
      Object.Items (ID).Must_Abort := not Sent and then
        Object.Items (ID).Completion = Admitted;
      Object.Items (ID).Current :=
        (if Sent and Object.Items (ID).Completion = Admitted then Acknowledged
         else Retained);
   end Delivered;
end Intel_GPU_Broker_Launches;
