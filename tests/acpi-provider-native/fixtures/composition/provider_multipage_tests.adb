pragma Ada_2022;
with Ada.Text_IO;
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
procedure Provider_Multipage_Tests is
   use type Firmware_Tables.Bytes;
   package Transport renames ACPI_Provider_Transport;
   package Driver is new ACPI_Snapshot_Provider.Driver
     (2,Transport.Exchange,Transport.Import_Table,Transport.Release_Provider_Grant);
   Page_Bytes : constant := 4096;
   Payload_Length : constant := Page_Bytes+3;
   function Description return Firmware_Tables.Bytes is
      Payload : Firmware_Tables.Bytes(1..Payload_Length);
      Sum : Unsigned_8 := 0;
   begin
      for I in Payload'Range loop Payload(I):=Unsigned_8(I mod 251); end loop;
      declare
         Data : Firmware_Tables.Bytes := Migration_Tables.Make(Payload);
      begin
         Data(1..4):=[84,69,83,84]; -- TEST description, no AML interpretation
         Data(10):=0;
         for B of Data loop Sum:=Sum+B; end loop;
         Data(10):=0-Sum;
         return Data;
      end;
   end Description;
   First : constant Firmware_Tables.Bytes := Migration_Tables.Make([16#08#,86,65,76,48,1]);
   Large : constant Firmware_Tables.Bytes := Description;
   Data : Firmware_Tables.Bytes := First & Large;
   Items : constant Catalog(5..6) := [(41,ACPI_Service.DSDT,First'Length),
                                      (97,ACPI_Service.Description,Large'Length)];
   Slices : constant Transport.Table_Slices(5..6) := [(Items(5),0),(Items(6),First'Length)];
   S : Driver.State;
   O : Observation;
   R : Response;
   Accepted, Delayed : Boolean := False;
   Before : Natural;
   Checks : Natural := 0;
   Sum : Unsigned_8 := 0;
   procedure Check(OK : Boolean) is
   begin
      Checks:=Checks+1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check(Large'Length>Page_Bytes and then Large'Length mod Page_Bytes/=0);
   for B of Large loop Sum:=Sum+B; end loop;
   Check(Sum=0);
   Check(Server.Table_Byte_Limit>=Large'Length);
   Allow_Return:=True;
   Transport.Initialize(CuBit.Handles.Test_Endpoint,Slices,Data,(others=><>),Accepted);
   Check(Accepted and then Allocated_Bytes=2*Page_Bytes);
   Data:=[others=>16#EE#]; -- only adapter's immutable copy may supply grants
   Driver.Begin_Catalog(S,Items);
   for Step in 1..20 loop
      exit when Driver.Status(S) not in Running | Waiting_For_Cleanup;
      if Driver.Status(S)=Waiting_For_Cleanup then
         Check(not Delayed and then Creates=2 and then Import_Calls=2 and then Finish_Calls=0);
         Check(Driver.Accepted_Tables(S)=2 and then Grant_Pages=2);
         Check(ACPI_Native_Blocks.Pending(Adapter) and then Acquired and then Revoked and then Active);
         Check(Buffer(1..Large'Length)=Large);
         Check((for all I in Large'Length+1..2*Page_Bytes => Buffer(I)=0));
         Check(Grant_Image(1..Large'Length)=Large);
         Driver.Advance(S); Driver.Retry_Cleanup(S);
         Check(Driver.Status(S)=Waiting_For_Cleanup and then Creates=2 and then Finish_Calls=0);
         Check(Buffer(1..Large'Length)=Large and then Active and then Transport.Grant_Pending);
         Allow_Return:=True; Receiver_Turn;
         Check(not Acquired and then Active and then not ACPI_Native_Blocks.Pending(Adapter));
         Driver.Retry_Cleanup(S);
         Check(Driver.Cleanup(S)=Clear and then not Active and then not Transport.Grant_Pending);
         Delayed:=True;
      else
         if Creates=1 and then not Active then
            Check(Grant_Pages=1 and then Buffer(1..First'Length)=First);
            Allow_Return:=False;
         end if;
         Driver.Advance(S);
      end if;
   end loop;
   Check(Delayed and then Driver.Status(S)=Completed and then Current(Server)=Complete);
   Check(Driver.Accepted_Tables(S)=2 and then Acquires=2 and then Finish_Calls=1);
   for I in 1..2 loop
      Handle(Server,Observer,(Label=>Read_Table_Info,
        Data=>[ACPI_Requests.Revision(Server),Unsigned_64(I),0,0],others=><>),R);
      Check(R.Status=OK and then R.Data(1)=Unsigned_64(Items(I+4).ID));
   end loop;
   Driver.Observe_Completion(O); Check(O.Available);
   Before:=Calls; Driver.Advance(S); Check(Calls=Before);
   Ada.Text_IO.Put_Line("MULTIPAGE composition checks" & Checks'Image);
end Provider_Multipage_Tests;
