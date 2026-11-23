with ACPI_Demand_Refresh;with Demand_Test_Control;
package body ACPI_Native_Event_Domain is
 package T renames Demand_Test_Control;package L is new ACPI_Demand_Refresh(Minimum_Interval=>10,Maximum_Serial=>20);
 Item:L.State;
 procedure Preview_Battery_Refresh(Server:ACPI_Requests.State;Now:out Interfaces.Unsigned_64;Allowed:out Boolean) is
 pragma Unreferenced(Server);
 begin T.Preview_Calls:=T.Preview_Calls+1;Now:=T.Clock;Allowed:=T.Owner_Good and then L.Can_Request(Item,Now,T.Clock_Known);end;
 procedure Commit_Battery_Refresh(Server:ACPI_Requests.State;Now:Interfaces.Unsigned_64;Ticket:out Interfaces.Unsigned_64;Accepted:out Boolean) is
 pragma Unreferenced(Server);New_Attempt:Boolean;
 begin
 T.Commit_Calls:=T.Commit_Calls+1;Ticket:=0;Accepted:=False;
 if not T.Commit_Allowed then return;end if;
 L.Request(Item,Now,T.Clock_Known,Ticket,New_Attempt,Accepted);
 if New_Attempt then T.Attempts:=T.Attempts+1;end if;
 end;
 function Battery_Refresh_Ready(Ticket:Interfaces.Unsigned_64) return Boolean is
 begin if T.Completed then L.Complete(Item);end if;return L.Ready(Item,Ticket);end;
end ACPI_Native_Event_Domain;
