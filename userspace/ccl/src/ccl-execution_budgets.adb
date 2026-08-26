package body CCL.Execution_Budgets with
   SPARK_Mode => On
is
   procedure Initialize (Item : out Budget; Amount : Natural) is
   begin
      Item :=
        (Initial   => Interfaces.Unsigned_32 (Amount),
         Available => Interfaces.Unsigned_32 (Amount));
   end Initialize;

   procedure Consume (Item : in out Budget; Result : out Consume_Result) is
   begin
      if Item.Initial > Interfaces.Unsigned_32 (Natural'Last) or else
        Item.Available > Item.Initial
      then
         Result := Invalid_Budget;
      elsif Item.Available = 0 then
         Result := Exhausted;
      else
         Item.Available := Item.Available - 1;
         Result := Consumed;
      end if;
   end Consume;
end CCL.Execution_Budgets;
