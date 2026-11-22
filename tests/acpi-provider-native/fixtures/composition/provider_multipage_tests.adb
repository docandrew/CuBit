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
with Firmware_Tables.Catalog;
with Firmware_Tables.Snapshots;
with Migration_Tables;
with Bridge_Trace; use Bridge_Trace;
with Server_Model; use Server_Model;
procedure Provider_Multipage_Tests is
   use type Firmware_Tables.Bytes;
   use type Firmware_Tables.Snapshots.Phase;
   use type Firmware_Tables.Snapshots.Metadata_Status;
   use type Firmware_Tables.Snapshots.Metadata_Result;
   use type Firmware_Tables.Snapshots.Table_ID;
   package Snapshots renames Firmware_Tables.Snapshots;
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
   Source : Firmware_Tables.Bytes := First & Large;
   Data : Firmware_Tables.Bytes (Source'Range) := [others => 16#EE#];
   Boot : Snapshots.State (2, Data'Length, Large'Length);
   First_Metadata : constant Firmware_Tables.Catalog.Descriptor :=
     (Name => "DSDT", Extent => First'Length, Revision => First (9), others => <>);
   Large_Metadata : constant Firmware_Tables.Catalog.Descriptor :=
     (Name => "TEST", Extent => Large'Length, Revision => Large (9), others => <>);
   Copied : Boolean;
   Items : Catalog(5..6) := [(41,ACPI_Service.DSDT,First'Length),
                                      (97,ACPI_Service.Description,Large'Length)];
   Slices : Transport.Table_Slices(5..6) := [(Items(5),0),(Items(6),First'Length)];
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
   -- Use the same pure owned snapshot implementation as the kernel. The raw
   -- firmware overlay and native export syscall remain outside this harness.
   Check (Snapshots.Describe (Boot, 1).Status = Snapshots.Snapshot_Not_Ready);
   Snapshots.Begin_Snapshot (Boot, 2);
   Snapshots.Append (Boot, First_Metadata, Source (1 .. First'Length), Copied);
   Check (Copied and then Snapshots.Count (Boot) = 0);
   Check (Snapshots.Describe (Boot, 1).Status = Snapshots.Snapshot_Not_Ready);
   Source (1 .. First'Length) := [others => 16#DD#];
   Snapshots.Copy (Boot, 1, Data, Copied);
   Check (not Copied and then (for all B of Data => B = 0));
   Snapshots.Append (Boot, Large_Metadata, Source (First'Length + 1 .. Source'Last), Copied);
   Check (Copied and then Snapshots.Count (Boot) = 0);
   Source := [others => 16#CC#];
   Snapshots.Seal (Boot);
   Check (Snapshots.Current (Boot) = Snapshots.Ready and then Snapshots.Count (Boot) = 2);
   Check (Snapshots.Describe (Boot, 0).Status = Snapshots.Unknown_Table
     and then Snapshots.Describe (Boot, 3).Status = Snapshots.Unknown_Table);
   for I in 1 .. 2 loop
      declare
         Public_Item : constant Snapshots.Metadata_Result := Snapshots.Describe (Boot, I);
         Expected : constant Firmware_Tables.Catalog.Descriptor :=
           (if I = 1 then First_Metadata else Large_Metadata);
      begin
         Check (Public_Item.Status = Snapshots.Metadata_Available);
         Check (Public_Item.Info.ID = Snapshots.Table_ID (I)
           and then Public_Item.Info.Name = Expected.Name
           and then Public_Item.Info.Extent = Expected.Extent
           and then Public_Item.Info.Revision = Expected.Revision);
         Items (I + 4) := (Positive (Public_Item.Info.ID),
           (if Public_Item.Info.Name = "DSDT" then ACPI_Service.DSDT
            elsif Public_Item.Info.Name = "SSDT" then ACPI_Service.SSDT
            else ACPI_Service.Description), Public_Item.Info.Extent);
      end;
   end loop;
   Slices := [(Items (5), 0), (Items (6), First'Length)];
   declare
      Relocated : Snapshots.State (2, Data'Length, Large'Length);
      Relocated_First : Firmware_Tables.Catalog.Descriptor := First_Metadata;
      Relocated_Large : Firmware_Tables.Catalog.Descriptor := Large_Metadata;
      Synthetic_Relocation : constant Firmware_Tables.Address_Value := 16#10_0000#;
   begin
      -- Synthetic metadata only: these addresses are never dereferenced.
      Relocated_First.Physical := Relocated_First.Physical + Synthetic_Relocation;
      Relocated_Large.Physical := Relocated_Large.Physical + Synthetic_Relocation;
      Snapshots.Begin_Snapshot (Relocated, 2);
      Snapshots.Append (Relocated, Relocated_First, First, Copied); Check (Copied);
      Snapshots.Append (Relocated, Relocated_Large, Large, Copied); Check (Copied);
      Snapshots.Seal (Relocated);
      Check (Snapshots.Describe (Boot, 1) = Snapshots.Describe (Relocated, 1)
        and then Snapshots.Describe (Boot, 2) = Snapshots.Describe (Relocated, 2));
   end;
   declare
      Rejected : Snapshots.State (2, Data'Length, Large'Length);
      Mutable : Firmware_Tables.Bytes := Large;
      Mutable_Metadata : Firmware_Tables.Catalog.Descriptor := Large_Metadata;
      Checksum : Unsigned_8 := 0;
      Unpublished : Firmware_Tables.Bytes (2 .. First'Length + 1) := [others => 16#AA#];
   begin
      Mutable (1 .. 4) := [70, 65, 67, 83]; -- FACS, excluded live state
      Mutable (10) := 0;
      for B of Mutable loop Checksum := Checksum + B; end loop;
      Mutable (10) := 0 - Checksum;
      Mutable_Metadata.Name := "FACS";
      Snapshots.Begin_Snapshot (Rejected, 2);
      Snapshots.Append (Rejected, First_Metadata, First, Copied); Check (Copied);
      Snapshots.Append (Rejected, Mutable_Metadata, Mutable, Copied);
      Check (not Copied and then Snapshots.Current (Rejected) = Snapshots.Failed);
      Snapshots.Seal (Rejected);
      Check (Snapshots.Count (Rejected) = 0
        and then Snapshots.Describe (Rejected, 1).Status = Snapshots.Snapshot_Not_Ready);
      Snapshots.Copy (Rejected, 1, Unpublished, Copied);
      Check (not Copied and then (for all B of Unpublished => B = 0));
   end;
   Snapshots.Copy (Boot, 1, Data (1 .. First'Length), Copied);
   Check (Copied and then Data (1 .. First'Length) = First);
   Snapshots.Copy (Boot, 2, Data (First'Length + 1 .. Data'Last), Copied);
   Check (Copied and then Data = First & Large);
   declare
      Padding_Start : constant Positive := 5;
      Padded : Firmware_Tables.Bytes (Padding_Start .. Padding_Start + 2 * Page_Bytes - 1) :=
        [others => 16#BB#];
   begin
      Snapshots.Copy (Boot, 2, Padded, Copied);
      Check (Copied and then Padded (Padding_Start .. Padding_Start + Large'Length - 1) = Large);
      Check ((for all I in Padding_Start + Large'Length .. Padded'Last => Padded (I) = 0));
      Snapshots.Copy (Boot, 2, Padded (Padding_Start .. Padding_Start + Large'Length - 2), Copied);
      Check (not Copied and then
        (for all I in Padding_Start .. Padding_Start + Large'Length - 2 => Padded (I) = 0));
   end;
   Snapshots.Begin_Snapshot (Boot, 1);
   Snapshots.Append (Boot, First_Metadata, Source (1 .. First'Length), Copied);
   Check (not Copied and then Snapshots.Current (Boot) = Snapshots.Ready
     and then Snapshots.Count (Boot) = 2);
   Snapshots.Copy (Boot, 1, Data (1 .. First'Length), Copied);
   Check (Copied and then Data = First & Large);
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
