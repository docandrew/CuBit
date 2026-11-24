with Interfaces;
package Demand_Test_Control is
 Clock:Interfaces.Unsigned_64:=100;
 Clock_Known,Owner_Good,Commit_Allowed:Boolean:=True;
 Completed:Boolean:=False;
 Preview_Calls,Commit_Calls,Attempts:Natural:=0;
 Cache_Status:Interfaces.Unsigned_64:=4;
 Generation:Interfaces.Unsigned_64:=7;
 Read_Calls:Natural:=0;
end Demand_Test_Control;
