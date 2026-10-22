pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Snapshot_Provider; use ACPI_Snapshot_Provider;
with ACPI_Requests; use ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Service;
with Firmware_Tables;
with Migration_Tables;
procedure Provider_Bootstrap_Tests is
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   type Scenario is (Normal, Lost_Import, Lost_Finish, Cleanup_Delay,
     Stale_Import, Old_Reply, Corrupt_Reply, Foreign_Completion,
     Rejected_Table, Exhausted, Duplicate_Catalog, Capacity_Tables,
     Capacity_Total, Capacity_Single, Lost_Start, Cleanup_Uncertain,
     Cleanup_Never_Returns, Interrupted_Observation, Mixed_Observation);
   First : constant Firmware_Tables.Bytes := Migration_Tables.Make([16#08#,86,65,76,48,1]);
   Second : constant Firmware_Tables.Bytes := Migration_Tables.Make([16#08#,86,65,76,49,1],True);
   Catalog_Data : constant Catalog :=
     [(41,ACPI_Service.DSDT,First'Length),(97,ACPI_Service.SSDT,Second'Length)];
   procedure Run (Mode : Scenario) is
      Server : ACPI_Requests.State
        ((if Mode=Capacity_Tables then 1 else 2),
         (if Mode=Capacity_Total then First'Length+Second'Length-1 else 2048),
         (if Mode=Capacity_Single then First'Length-1 else 2048),
        (if Mode=Exhausted then Max_Revision else 0));
      Config : constant ACPI_Endpoint.Configuration := (10,11);
      Import_Calls, Finish_Calls, Mutations, Cleanup_Calls : Natural := 0;
      Provider_Loan : Boolean := False;
      procedure Exchange (Request : Packet; Reply : out Packet; Known : out Boolean) is
      begin
         if Request.Label=Finish_Snapshot then Finish_Calls:=Finish_Calls+1; end if;
         if Request.Label in Start_Snapshot | Finish_Snapshot then Mutations:=Mutations+1; end if;
         ACPI_Endpoint.Dispatch(Server,Config,11,Request,Reply);
         Known := not ((Mode=Lost_Finish and then Request.Label=Finish_Snapshot) or else
           (Mode=Lost_Start and then Request.Label=Start_Snapshot));
         -- Synthetic transport disturbances during diagnostic observation only.
         -- No claim that these modified replies were produced by the server.
         if Request.Label=Read_Metrics and then Request.Data(0)=Metrics_Completion_Access_Page then
            if Mode=Interrupted_Observation then Known:=False;
            elsif Mode=Mixed_Observation then Reply.Data(0):=Reply.Data(0)+1;
            end if;
         end if;
      end Exchange;
      procedure Import_Table (Index : Positive; Token : ACPI_Snapshot_Provider.Revision;
         Item : Table_Descriptor; Reply : out Packet; Known : out Boolean;
         Cleanup : out Cleanup_State) is
         Bytes : Firmware_Tables.Bytes := (if Index=1 then First else Second);
         Sent : ACPI_Snapshot_Provider.Revision := Token;
      begin
         Check(not Provider_Loan); Import_Calls:=Import_Calls+1;
         Check(Item.ID=Catalog_Data(Index).ID and then Item.Length=Bytes'Length);
         -- Injected provider-side grant lifecycle. This is NOT a wire cleanup
         -- acknowledgment or observation of Native_Blocks.Pending.
         Provider_Loan:=True;
         if Mode=Stale_Import then Sent:=Token-1; end if;
         if Mode=Rejected_Table then Bytes(10):=Bytes(10)+1; end if;
         ACPI_Endpoint.Dispatch_Block(Server,Config,11,Sent,Item.ID,Item.Kind,Bytes,Reply);
         Known := Mode not in Lost_Import | Foreign_Completion;
         if Mode=Old_Reply then Reply.Data(0):=Token; end if;
         if Mode=Corrupt_Reply then Reply.Flags:=1; end if;
         -- A successful reply says nothing about retirement of the grant.
         if Mode in Cleanup_Uncertain | Cleanup_Never_Returns then Cleanup:=Uncertain;
         elsif Mode in Cleanup_Delay | Lost_Import then Cleanup:=Pending;
         else Provider_Loan:=False; Cleanup:=Clear;
         end if;
      end Import_Table;
      procedure Release_Acquisition (Released : out Boolean) is
      begin
         Cleanup_Calls:=Cleanup_Calls+1;
         Released:=Cleanup_Calls>1 and then Mode/=Cleanup_Never_Returns;
         if Released then Provider_Loan:=False; end if;
      end Release_Acquisition;
      package Driver is new ACPI_Snapshot_Provider.Driver
        (2,Exchange,Import_Table,Release_Acquisition);
      S : Driver.State;
      O : Observation;
      -- Deliberately non-1 lower bound: callback indices remain ordinal1/2.
      Items : Catalog(5..6) := Catalog_Data;
      Before : Natural;
      R : Response;
   begin
      if Mode=Duplicate_Catalog then Items(Items'Last).ID:=Items(Items'First).ID; end if;
      Driver.Begin_Catalog(S,Items);
      for I in 1 .. 12 loop
         if Driver.Status(S)=Waiting_For_Cleanup then
            Before:=Import_Calls;
            Driver.Advance(S); Check(Import_Calls=Before);
            if Mode in Cleanup_Uncertain | Cleanup_Never_Returns then
               Check(Driver.Cleanup(S)=Uncertain);
            end if;
            Driver.Retry_Cleanup(S);
         else Driver.Advance(S);
         end if;
      end loop;
      case Mode is
         when Normal | Cleanup_Delay | Cleanup_Uncertain | Interrupted_Observation | Mixed_Observation =>
            Check(Driver.Status(S)=Completed and then Import_Calls=2 and then Finish_Calls=1);
            Check(Driver.Accepted_Tables(S)=2 and then Current(Server)=Complete);
            for I in 1 .. 2 loop
               Handle(Server,Observer,(Label=>Read_Table_Info,Data=>[ACPI_Requests.Revision(Server),Unsigned_64(I),0,0],others=><>),R);
               Check(R.Status=OK and then R.Data(1)=Unsigned_64(Catalog_Data(I).ID));
            end loop;
         when Lost_Import | Foreign_Completion =>
            Check(Driver.Status(S)=Unknown_Outcome and then Import_Calls=1 and then Finish_Calls=0);
            Check(Driver.Accepted_Tables(S)=0 and then Current(Server)=Receiving);
         when Lost_Start =>
            Check(Driver.Status(S)=Unknown_Outcome and then Mutations=1 and then Import_Calls=0);
            Check(Current(Server)=Receiving and then Finish_Calls=0);
         when Cleanup_Never_Returns =>
            Check(Driver.Status(S)=Waiting_For_Cleanup and then Driver.Cleanup(S)=Uncertain);
            Check(Import_Calls=1 and then Finish_Calls=0 and then Provider_Loan);
         when Lost_Finish =>
            Check(Driver.Status(S)=Unknown_Outcome and then Finish_Calls=1 and then Current(Server)=Complete);
         when Stale_Import =>
            Check(Driver.Status(S)=Rejected and then Driver.Remote_Status(S)=Stale);
            Check(Driver.Accepted_Tables(S)=0 and then Finish_Calls=0);
         when Old_Reply | Corrupt_Reply =>
            Check(Driver.Status(S)=Invalid_Response and then Finish_Calls=0);
         when Rejected_Table =>
            Check(Driver.Status(S)=Rejected and then Driver.Remote_Status(S)=Table_Rejected and then Current(Server)=Failed);
         when Exhausted | Duplicate_Catalog | Capacity_Tables | Capacity_Total | Capacity_Single =>
            Check(Driver.Status(S)=Local_Limit and then Mutations=0 and then Import_Calls=0);
      end case;
      if Mode=Lost_Import then
         Check(Driver.Cleanup(S)=Pending);
         Driver.Retry_Cleanup(S); Check(Driver.Cleanup(S)=Pending);
         Driver.Retry_Cleanup(S); Check(Driver.Cleanup(S)=Clear);
         Check(Driver.Status(S)=Unknown_Outcome);
      end if;
      Before:=Mutations+Import_Calls;
      Driver.Begin_Catalog(S,Catalog_Data); Driver.Advance(S);
      Check(Mutations+Import_Calls=Before); -- no restart/replay, including Finish loss
      Driver.Observe_Completion(O);
      if Mode in Interrupted_Observation | Mixed_Observation then
         Check(not O.Available);
         Check(Driver.Status(S)=Completed and then Finish_Calls=1);
      else
         Check(O.Available);
         Check(O.Pages(0)(1)=Phase'Pos(Current(Server)));
      end if;
      if Mode=Lost_Finish then
         Check(Driver.Status(S)=Unknown_Outcome and then O.Pages(0)(1)=Phase'Pos(Complete));
      elsif Mode=Rejected_Table then
         -- Early admission failure: phase Failed, Finish Not_Attempted is not healthy receiving.
         Check(O.Pages(0)(1)=Phase'Pos(Failed) and then O.Pages(1)(1)=Finish_Stage_Code'Pos(Finish_Not_Attempted_Code));
      end if;
   end Run;
begin
   for Mode in Scenario loop Run(Mode); end loop;
   Ada.Text_IO.Put_Line("Provider bootstrap checks" & Checks'Image);
end Provider_Bootstrap_Tests;
