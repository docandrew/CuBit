package body Queue_Wakes with SPARK_Mode is

   procedure Arrive
     (Item : in out State; Work_Waiting : Boolean; Outcome : out Arrival) is
   begin
      Outcome := (Answer_Held => Item = Held, Answer_Now => Work_Waiting);
      Item := (if Work_Waiting then Idle else Held);
   end Arrive;

   procedure Hold_Failed (Item : in out State) is
   begin
      Item := Idle;
   end Hold_Failed;

   procedure Posted (Item : in out State; Answer_Held : out Boolean) is
   begin
      Answer_Held := Item = Held;
      Item := Idle;
   end Posted;

   procedure Ended (Item : in out State; Answer_Held : out Boolean) is
   begin
      Answer_Held := Item = Held;
      Item := Idle;
   end Ended;

end Queue_Wakes;
