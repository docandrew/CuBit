--  Shared check counter for the rtw89 hosted tests.
package Test_Support is
   Checks : Natural := 0;
   Failed : Natural := 0;
   procedure Check (Condition : Boolean; Name : String);
end Test_Support;
