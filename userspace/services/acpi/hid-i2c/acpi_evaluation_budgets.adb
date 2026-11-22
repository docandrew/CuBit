pragma Ada_2022;
package body ACPI_Evaluation_Budgets with SPARK_Mode is
   function Spent (Item : State) return Units is (Item.Used);
   function Remaining (Item : State) return Units is (Item.Limit - Item.Used);
   procedure Charge (Item : in out State; Cost : Units; Accepted : out Boolean) is
   begin
      Accepted := Cost <= Remaining (Item);
      if Accepted then Item.Used := Item.Used + Cost; end if;
   end Charge;
end ACPI_Evaluation_Budgets;
