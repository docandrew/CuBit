with Interfaces; use Interfaces;
--  What the scripted CCL_Metric_IO was last asked for.
package Fake_Calls is
   Now : constant Unsigned_64 := 10_000_000;
   Last_Match : Natural := 0;
   Last_Name : String (1 .. 64) := [others => ' '];
   Last_Name_Length : Natural := 0;
   Last_Source_Length : Natural := 0;
   Last_Id, Last_From, Last_To, Last_Step : Unsigned_64 := 0;
end Fake_Calls;
