with Test_Storage;
procedure Queue_Service_Prepare (T : in out Service.Table) is
   Count : constant Natural :=
     Service.Session_Id'Pos (Service.Session_Id'Last) -
     Service.Session_Id'Pos (Service.Session_Id'First) + 1;
   Grown : Boolean;
begin
   Service.Attach (T, Test_Storage.Allocate (Service.Storage_Bytes (Count)));
   Service.Grow (T, Service.Session_Id'Last, Grown);
   pragma Assert (Grown);
end Queue_Service_Prepare;
