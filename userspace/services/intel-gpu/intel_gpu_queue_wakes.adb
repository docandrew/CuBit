package body Intel_GPU_Queue_Wakes with SPARK_Mode is

   procedure Arrive
     (Item : in out State; Context : Q.Context_Index; Target : Value;
      Holds, Slot_Free : Boolean; Outcome : out Arrival)
   is
      Was_Held : constant Boolean := Item.Current = Held;
   begin
      Outcome := (Answer_Held => Was_Held, Answer_Now => False, Result => Q.Woken);
      if Holds then
         Outcome.Answer_Now := True;
         Item.Current := Idle;
      elsif Slot_Free or else Was_Held then
         Item := (Current => Held, Context => Context, Target => Target);
      else
         Outcome.Answer_Now := True;
         Outcome.Result := Q.Not_Held;
         Item.Current := Idle;
      end if;
   end Arrive;

   procedure Hold_Failed (Item : in out State) is
   begin
      Item.Current := Idle;
   end Hold_Failed;

   procedure Step (Item : in out State; Holds : Boolean; Answer_Held : out Boolean) is
   begin
      Answer_Held := Item.Current = Held and then Holds;
      if Answer_Held then
         Item.Current := Idle;
      end if;
   end Step;

   procedure Ended (Item : in out State; Answer_Held : out Boolean) is
   begin
      Answer_Held := Item.Current = Held;
      Item.Current := Idle;
   end Ended;

end Intel_GPU_Queue_Wakes;
