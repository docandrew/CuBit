pragma Ada_2022;
with Interfaces;
with Bridge_Trace; use Bridge_Trace;
with CuBit.Handles;
package body Server_Model is
 use type Interfaces.Unsigned_32;
 Handler_Calls : Natural := 0;
 procedure Selected_Handle
   (Item : in out ACPI_Requests.State; Origin : ACPI_Requests.Authority;
    Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Response) is
 begin
  Handler_Calls := Handler_Calls + 1;
  ACPI_Requests.Handle (Item, Origin, Request, Reply);
 end Selected_Handle;
 package Endpoint is new ACPI_Endpoint.With_Handler (Selected_Handle);
 package Blocks is new ACPI_Native_Blocks.With_Endpoint (Endpoint.Route);
 procedure Route(Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet) is
  Stamp : constant Interfaces.Unsigned_64 :=
   (if (Mode=Wrong_Stamp and then Request.Label=ACPI_Native_Blocks.Import_Table_Grant)
     or else (Fault=Deny_Interest and then Request.Label=ACPI_Requests.Configure_Notifications)
     then 99 else 11);
  Before : constant Natural := Handler_Calls;
 begin
  if Request.Label=ACPI_Native_Blocks.Import_Table_Grant then Import_Calls:=Import_Calls+1;
  elsif Request.Label=ACPI_Requests.Start_Snapshot then Start_Calls:=Start_Calls+1;
  elsif Request.Label=ACPI_Requests.Finish_Snapshot then Finish_Calls:=Finish_Calls+1;
  end if;
  Blocks.Route(Adapter,Server,Config,Stamp,CuBit.Handles.Provider_Endpoint,Request,Reply);
  if Handler_Calls /= Before + (if Request.Label=ACPI_Native_Blocks.Import_Table_Grant then 0 else 1) then
   raise Program_Error with "ordinary handler selection or grant bypass";
  end if;
 end Route;
 procedure Receiver_Turn is
 begin
  -- Explicit server event-loop action, never called from provider cleanup.
  ACPI_Native_Blocks.Retry_Return(Adapter);
 end Receiver_Turn;
end Server_Model;
