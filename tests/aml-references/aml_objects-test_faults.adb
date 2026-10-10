pragma Ada_2022;
package body AML_Objects.Test_Faults with SPARK_Mode => Off is
   procedure Alias_Backing (Store : in out State; Target, Source : Object_ID) is
   begin
      pragma Assert (Target > 0 and then Target <= Store.Used);
      pragma Assert (Source > 0 and then Source <= Store.Used);
      pragma Assert (Store.Objects (Target).Tag = Store.Objects (Source).Tag);
      pragma Assert (Store.Objects (Target).Size = Store.Objects (Source).Size);
      Store.Objects (Target).First := Store.Objects (Source).First;
   end Alias_Backing;
end AML_Objects.Test_Faults;
