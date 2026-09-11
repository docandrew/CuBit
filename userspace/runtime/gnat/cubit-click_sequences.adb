package body CuBit.Click_Sequences with SPARK_Mode is
   function Near (Left, Right : Point; Slop : Natural) return Boolean is
     ((if Left.X >= Right.X then Left.X - Right.X
       else Right.X - Left.X) <= Slop and then
      (if Left.Y >= Right.Y then Left.Y - Right.Y
       else Right.Y - Left.Y) <= Slop);

   function Timely (Item : State; Now_Ms : Unsigned_64;
                    Settings : Policy) return Boolean is
     (Now_Ms /= Unsigned_64'Last and then Now_Ms >= Item.Last_Ms and then
      Now_Ms >= Item.Press_Ms and then
      Now_Ms - Item.Press_Ms <= Settings.Interval_Ms);

   procedure Reset (Item : out State) is
   begin
      Item := (others => <>);
   end Reset;

   procedure Motion (Item : in out State; At_Point : Point;
                     Settings : Policy := Default_Policy) is
   begin
      if Item.Stage /= Idle and then
        not Near (Item.Origin, At_Point, Settings.Slop)
      then
         Reset (Item);
      end if;
   end Motion;

   procedure Press
     (Item : in out State; Target : Target_ID; At_Point : Point;
      Now_Ms : Unsigned_64; Kind : out Press_Kind;
      Settings : Policy := Default_Policy)
   is
   begin
      Kind := Single_Press;
      if Target = No_Target or else Now_Ms = Unsigned_64'Last then
         Reset (Item);
      elsif Item.Stage = Released and then Item.Target = Target and then
        Timely (Item, Now_Ms, Settings) and then
        Near (Item.Origin, At_Point, Settings.Slop)
      then
         Kind := Double_Press;
         --  Disjoint pairs: a third press cannot reuse the second press.
         Reset (Item);
      else
         Item := (Pressed, Target, At_Point, Now_Ms, Now_Ms);
      end if;
   end Press;

   procedure Release
     (Item : in out State; At_Point : Point; Now_Ms : Unsigned_64;
      Settings : Policy := Default_Policy)
   is
   begin
      if Item.Stage = Pressed and then Timely (Item, Now_Ms, Settings)
        and then Near (Item.Origin, At_Point, Settings.Slop)
      then
         Item.Stage := Released;
         Item.Last_Ms := Now_Ms;
      else
         Reset (Item);
      end if;
   end Release;
end CuBit.Click_Sequences;
