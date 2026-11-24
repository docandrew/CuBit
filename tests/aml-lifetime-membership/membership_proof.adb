pragma Ada_2022;
package body Membership_Proof with SPARK_Mode is
   procedure Run_Small is
      Item : Small.State := Small.Empty;
   begin
      Small.Attach (Item, 2, 1);
      Small.Attach (Item, 3, 1);
      Small.Detach (Item, 2);
      Small.Detach (Item, 3);
   end Run_Small;
   procedure Run_Large is
      Item : Large.State := Large.Empty;
   begin
      Large.Attach (Item, 2, 1);
      Large.Attach (Item, 3, 1);
      Large.Detach (Item, 2);
      Large.Detach (Item, 3);
   end Run_Large;
end Membership_Proof;
