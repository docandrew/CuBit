pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Provider_Transport;
with ACPI_Snapshot_Provider; use ACPI_Snapshot_Provider;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Service;
with ACPI_Native_Blocks;
with CuBit.Handles;
with Firmware_Tables;
with Migration_Tables;
with Bridge_Trace; use Bridge_Trace;
with Server_Model; use Server_Model;
procedure Provider_Composition_Tests is
   use type Firmware_Tables.Bytes;
   package Transport renames ACPI_Provider_Transport;
   package Driver is new ACPI_Snapshot_Provider.Driver
     (2,Transport.Exchange,Transport.Import_Table,Transport.Release_Provider_Grant);
   First : constant Firmware_Tables.Bytes := Migration_Tables.Make([16#08#,86,65,76,48,1]);
   Second : constant Firmware_Tables.Bytes := Migration_Tables.Make([16#08#,86,65,76,49,1],True);
   Data : Firmware_Tables.Bytes := First & Second;
   Items : constant Catalog(5..6) := [(41,ACPI_Service.DSDT,First'Length),
                                     (97,ACPI_Service.SSDT,Second'Length)];
   Slices : constant Transport.Table_Slices(5..6) :=
     [(Items(5),0),(Items(6),First'Length)];
   S : Driver.State;
   O : Observation;
   R : Response;
   Accepted, Delayed : Boolean := False;
   Saved_First : Firmware_Tables.Bytes(1..First'Length);
   Before, Before_Imports, Before_Creates : Natural;
   Checks : Natural := 0;
   procedure Check(OK : Boolean) is
   begin
      Checks:=Checks+1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   if Ada.Command_Line.Argument_Count/=1 then raise Program_Error with "one scenario"; end if;
   Mode:=Scenario'Value(Ada.Command_Line.Argument(1));
   Created_OK:=Mode/=Create_False;
   Allow_Return:=Mode/=Pending_Return;
   if Mode=Bad_Table then Data(10):=Data(10)+1; end if;
   Transport.Initialize(CuBit.Handles.Test_Endpoint,Slices,Data,(others=><>),Accepted);
   Check(Accepted);
   Driver.Begin_Catalog(S,Items);
   for Step in 1..20 loop
      exit when Driver.Status(S) not in Running | Waiting_For_Cleanup;
      if Driver.Status(S)=Waiting_For_Cleanup then
         Check(Mode=Pending_Return and then not Delayed);
         Check(ACPI_Native_Blocks.Pending(Adapter) and then Acquired and then Revoked and then Active);
         Check(Driver.Accepted_Tables(S)=1 and then Import_Calls=1 and then Finish_Calls=0);
         Saved_First:=Buffer(1..First'Length);
         Before_Imports:=Import_Calls; Before_Creates:=Creates;
         Driver.Advance(S); Driver.Retry_Cleanup(S);
         Check(Driver.Status(S)=Waiting_For_Cleanup and then Import_Calls=Before_Imports and then Creates=Before_Creates);
         Check(Buffer(1..First'Length)=Saved_First and then Transport.Grant_Pending);
         Allow_Return:=True;
         Receiver_Turn;
         Check(not ACPI_Native_Blocks.Pending(Adapter) and then not Acquired and then Active);
         Driver.Retry_Cleanup(S);
         Check(Driver.Cleanup(S)=Clear and then not Active and then not Transport.Grant_Pending);
         Delayed:=True;
      else
         Driver.Advance(S);
      end if;
   end loop;
   case Mode is
      when Normal | Pending_Return =>
         Check(Driver.Status(S)=Completed and then Current(Server)=Complete);
         Check(Driver.Accepted_Tables(S)=2 and then Creates=2 and then Import_Calls=2 and then Finish_Calls=1);
         Check(Acquires=2 and then not Active and then not Acquired and then not Transport.Grant_Pending);
         Check(Delayed=(Mode=Pending_Return));
         for I in 1..2 loop
            Handle(Server,Observer,(Label=>Read_Table_Info,
              Data=>[ACPI_Requests.Revision(Server),Unsigned_64(I),0,0],others=><>),R);
            Check(R.Status=OK and then R.Data(1)=Unsigned_64(Items(I+4).ID));
         end loop;
      when Lost_Start =>
         Check(Driver.Status(S)=Unknown_Outcome and then Current(Server)=Receiving);
         Check(Start_Calls=1 and then Creates=0 and then Import_Calls=0 and then Finish_Calls=0);
      when Lost_Import =>
         Check(Driver.Status(S)=Unknown_Outcome and then Current(Server)=Receiving);
         Check(Import_Calls=1 and then Creates=1 and then Acquires=1 and then Finish_Calls=0);
         Check(Driver.Accepted_Tables(S)=0 and then not Active and then Driver.Cleanup(S)=Clear);
      when Lost_Finish =>
         Check(Driver.Status(S)=Unknown_Outcome and then Current(Server)=Complete);
         Check(Import_Calls=2 and then Finish_Calls=1 and then Driver.Accepted_Tables(S)=2);
      when Bad_Table =>
         Check(Driver.Status(S)=Rejected and then Driver.Remote_Status(S)=Table_Rejected);
         Check(Current(Server)=Failed and then Import_Calls=1 and then Finish_Calls=0 and then not Active);
      when Wrong_Stamp =>
         Check(Driver.Status(S)=Rejected and then Driver.Remote_Status(S)=Denied);
         Check(Acquires=0 and then Import_Calls=1 and then Finish_Calls=0 and then not Active);
      when Create_False =>
         Check(Driver.Status(S)=Unknown_Outcome and then Driver.Cleanup(S)=Uncertain);
         Check(Transport.Quarantined and then Creates=1 and then Import_Calls=0 and then Revokes=0 and then Queries=0);
         Driver.Retry_Cleanup(S);
         Check(Driver.Cleanup(S)=Uncertain and then Creates=1 and then Queries=0);
   end case;
   Before:=Start_Calls+Import_Calls+Finish_Calls;
   Driver.Begin_Catalog(S,Items); Driver.Advance(S);
   Check(Start_Calls+Import_Calls+Finish_Calls=Before);
   Driver.Observe_Completion(O);
   Check(O.Available);
   if Mode in Lost_Start | Lost_Import | Lost_Finish | Create_False then
      Check(Driver.Status(S)=Unknown_Outcome);
   end if;
   Check(Start_Calls+Import_Calls+Finish_Calls=Before);
   Transport.Initialize(CuBit.Handles.Test_Endpoint,Slices,Data,(others=><>),Accepted);
   Check(not Accepted);
   Ada.Text_IO.Put_Line(Mode'Image & " composition checks" & Checks'Image);
end Provider_Composition_Tests;
