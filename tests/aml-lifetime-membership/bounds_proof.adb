pragma Ada_2022;
package body Bounds_Proof with SPARK_Mode is
   procedure Run_Small is
      Item : Small.State := Small.Empty;
   begin
      Small.Append (Item, 10); Small.Append (Item, 0);
      Small.Update_Last (Item, 20); Small.Pop (Item); Small.Pop (Item);
   end Run_Small;
   procedure Run_Large is
      Item : Large.State := Large.Empty;
   begin
      Large.Append (Item, 10); Large.Append (Item, 0);
      Large.Update_Last (Item, 20); Large.Pop (Item); Large.Pop (Item);
   end Run_Large;
end Bounds_Proof;
