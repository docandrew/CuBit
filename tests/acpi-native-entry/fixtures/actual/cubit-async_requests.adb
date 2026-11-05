package body CuBit.Async_Requests with SPARK_Mode is
   procedure Reserve
     (Item : in out Tracker; ID : Token; Accepted : out Boolean) is
   begin
      Accepted := Can_Reserve (Item, ID);
      if Accepted then
         Item.Last := ID;
         Item.Pending := ID;
         Item.Current := Reserved;
      end if;
   end Reserve;

   procedure Submitted (Item : in out Tracker; Accepted : Boolean) is
   begin
      Item.Current := (if Accepted then In_Flight else Idle);
      if not Accepted then
         Item.Pending := No_Token;
      end if;
   end Submitted;

   procedure Capture
     (Item : in out Tracker; ID : Token; Valid : Boolean;
      Accepted : out Boolean) is
   begin
      Accepted := Item.Current = In_Flight and Valid and ID = Item.Pending;
      if Accepted then
         Item.Current := Completion_Ready;
      end if;
   end Capture;

   procedure Release (Item : in out Tracker) is
   begin
      Item.Pending := No_Token;
      Item.Current := Idle;
   end Release;

   procedure Stop (Item : in out Tracker) is
   begin
      Item.Stopped := True;
   end Stop;
end CuBit.Async_Requests;
