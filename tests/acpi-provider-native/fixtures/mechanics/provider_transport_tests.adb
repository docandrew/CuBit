pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Provider_Transport; use ACPI_Provider_Transport;
with ACPI_Snapshot_Provider; use ACPI_Snapshot_Provider;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with Firmware_Tables;
with Bridge_Trace; use Bridge_Trace;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Grant_References;
procedure Provider_Transport_Tests is
   -- Each case is a fresh process: production singleton has no reset escape.
   type Case_Kind is (Normal, Create_False, Hidden_Create_False, Pending_Retirement,
     Revoke_Refused, Import_Timeout, Kernel_Failure, Deadline_Start,
     Deadline_Import, Malformed_Request, Finish_Timeout, Wrong_Item);
   Mode : Case_Kind;
   Checks : Natural := 0;
   procedure Check(OK : Boolean) is
   begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   Data : Firmware_Tables.Bytes(1..40) := [others=>16#A5#];
   Items : constant Table_Slices(5..5) := [(Item=>(41,ACPI_Service.DSDT,40),Offset=>0)];
   Reply : Packet;
   Known, Accepted, Released : Boolean;
   Loan : Cleanup_State;
   Before : Natural;
   Request : Packet := (Label=>Start_Snapshot,Data=>[0,1,0,0],others=><>);
   Item : Table_Descriptor := Items(5).Item;
   use type CuBit.Messages.MessageWords;
   procedure Try_Rebind is
   begin
      Initialize(CuBit.Handles.Test_Endpoint,Items,Data,(others=><>),Accepted);
      Check(not Accepted);
   end Try_Rebind;
begin
   if Ada.Command_Line.Argument_Count/=1 then raise Program_Error with "one mandatory case"; end if;
   Mode:=Case_Kind'Value(Ada.Command_Line.Argument(1));
   Initialize(CuBit.Handles.Test_Endpoint,Items,Data,(others=><>),Accepted);
   Check(Accepted and then Used=1 and then Log(1)=Allocation);
   Data:=[others=>16#EE#]; -- bridge must have copied immutable original bytes
   Try_Rebind;
   if Mode=Deadline_Start then Now:=Unsigned_64'Last-1_000; end if;
   if Mode=Malformed_Request then Request.Flags:=1; end if;
   Exchange(Request,Reply,Known);
   if Mode in Deadline_Start | Malformed_Request then
      Check(not Known and then Calls=0 and then Creates=0);
      Now:=100; Exchange((Label=>Start_Snapshot,Data=>[0,1,0,0],others=><>),Reply,Known);
      Check(not Known and then Calls=0); -- no retry after locally unknown start
   else
      Check(Known and then Reply.Label=16#F000# and then Reply.Data(0)=1);
      Check(Last_Deadline=1_100);
      if Mode=Deadline_Import then Now:=Unsigned_64'Last-5_000; end if;
      if Mode=Wrong_Item then Item.ID:=97; end if;
      Created_OK:=Mode not in Create_False | Hidden_Create_False;
      Hidden_Creation:=Mode=Hidden_Create_False;
      Retired:=Mode not in Pending_Retirement | Revoke_Refused | Import_Timeout | Kernel_Failure;
      Revoked_OK:=Mode/=Revoke_Refused;
      Timeout:=Mode=Import_Timeout; Kernel_Error:=Mode=Kernel_Failure;
      Import_Table(1,1,Item,Reply,Known,Loan);
      if Mode in Deadline_Import | Wrong_Item then
         Check(not Known and then Creates=0 and then Calls=1 and then Loan=Clear);
      elsif Mode in Create_False | Hidden_Create_False then
         Check(not Known and then Loan=Uncertain and then Quarantined);
         Check(Creates=1 and then Calls=1 and then Revokes=0 and then Queries=0);
         Check(Hidden_Live=(Mode=Hidden_Create_False));
         Release_Provider_Grant(Released); Check(not Released and then Queries=0 and then Revokes=0);
         Try_Rebind;
         Import_Table(1,1,Items(5).Item,Reply,Known,Loan);
         Check(not Known and then Creates=1 and then Buffer(1)=16#A5#);
      else
         Check(Creates=1 and then not Last_Read_Write and then Last_Pages=1);
         Check(Last_Deadline=5_100);
         Check(Last_Message.tag.label=8 and then Last_Message.words=
           [1,CuBit.Grant_References.Encode((7,9)),41,40]);
         Check(Log(3)=Grant_Create and then Log(4)=IPC_Call and then
           Log(5)=Grant_Revoke and then Log(6)=Retirement_Query);
         Check((for all I in 1..40 => Buffer(I)=16#A5#));
         Check((for all I in 41..4096 => Buffer(I)=0));
         if Mode in Import_Timeout | Kernel_Failure then
            Check(not Known and then Loan=Pending);
         else
            Check(Known and then Reply.Label=16#F000#);
         end if;
         if not Retired then
            Check(Grant_Pending and then Loan=Pending);
            Before:=Calls; Import_Table(1,1,Items(5).Item,Reply,Known,Loan);
            Check(not Known and then Calls=Before and then Creates=1);
            Release_Provider_Grant(Released); Check(not Released);
            Check(Revoke_Accepted=Revoked_OK);
            Retired:=True; Release_Provider_Grant(Released);
            Check(Released and then not Grant_Pending);
         else
            Check(Loan=Clear and then not Grant_Pending);
         end if;
         Timeout:=Mode=Finish_Timeout; Kernel_Error:=False;
         Before:=Calls;
         Exchange((Label=>Finish_Snapshot,Data=>[2,0,0,0],others=><>),Reply,Known);
         if Mode in Import_Timeout | Kernel_Failure then Check(not Known and then Calls=Before);
         else
            Check(Calls=Before+1 and then Last_Deadline=30_100);
            Check(Known=(Mode/=Finish_Timeout));
         end if;
         Before:=Calls;
         Exchange((Label=>Finish_Snapshot,Data=>[2,0,0,0],others=><>),Reply,Known);
         Check(not Known and then Calls=Before);
         Try_Rebind;
      end if;
   end if;
   Ada.Text_IO.Put_Line(Mode'Image & " bridge checks" & Checks'Image);
end Provider_Transport_Tests;
