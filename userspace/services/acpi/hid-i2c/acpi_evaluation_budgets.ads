pragma Ada_2022;
package ACPI_Evaluation_Budgets with Pure, SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Units is Natural;
   -- The limit is fixed for this context. Clients cannot edit or copy the
   -- running total; only Charge can consume it.
   type State (Limit : Units) is limited private
     with Default_Initial_Condition => Spent (State) = 0;
   function Spent (Item : State) return Units
     with Global => null, Post => Spent'Result <= Item.Limit;
   function Remaining (Item : State) return Units
     with Global => null, Post => Remaining'Result = Item.Limit - Spent (Item);
   procedure Charge (Item : in out State; Cost : Units; Accepted : out Boolean)
   with Global => null,
     Post => Item.Limit = Item.Limit'Old
       and then Accepted = (Cost <= Remaining (Item)'Old)
       and then (if Accepted then Spent (Item) = Spent (Item)'Old + Cost
                 else Spent (Item) = Spent (Item)'Old);
private
   type State (Limit : Units) is limited record
      Used : Units := 0;
   end record
     with Type_Invariant => State.Used <= State.Limit;
end ACPI_Evaluation_Budgets;
