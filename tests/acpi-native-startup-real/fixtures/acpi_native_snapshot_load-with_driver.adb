pragma Ada_2022;
with Owner_Test_Control;
with Startup_Test_Firmware;
with ACPI_Snapshot_Startup.With_Driver;
package body ACPI_Native_Snapshot_Load.With_Driver is
   Was_Consumed : Boolean := False;
   function Consumed return Boolean is (Was_Consumed);
   package T renames Owner_Test_Control;
   package R renames L.R;
   package K renames ACPI_Native_Snapshot_Read.K;
   use type T.Scenario;
   subtype Bytes is R.F.Bytes;
   package F renames Startup_Test_Firmware;
   procedure Load
     (Run : in out Device_Driver.Session; Deadline : R.Tick; Owner : out L.State_Access; Result : out Report) is
      procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
         DSDT_Size:constant Natural:=F.Image(F.DSDT_ID)'Length;
         FADT_Size:constant Natural:=F.Image(F.FADT_ID)'Length;
      begin
         Item := (Ready=>True,Tables=>L.Count(F.Table_Count),Total_Bytes=>L.Count(DSDT_Size+FADT_Size),Largest_Table=>L.Count(Natural'Max(DSDT_Size,FADT_Size)));
         Status := (if T.Selected = T.Snapshot_Refused then R.Not_Authorized else R.Copy_OK);
         Result.Catalog_Status := (if T.Selected = T.Snapshot_Refused then K.Not_Authorized else K.Ok);
      end Info;
      procedure Describe (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
      begin
         declare Raw:constant Bytes:=F.Image(ID);Signature:String(1..4);begin
            for I in Signature'Range loop Signature(I):=Character'Val(Raw(I));end loop;
            Item:=(Signature,Raw(9),Raw'Length);Status:=R.Copy_OK;
         end;
         Result.Descriptor_Status := K.Ok;
      end Describe;
      procedure Copy_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status) is
      begin

         T.Copies := T.Copies + 1;
         Into := [others => 0]; Copied := 0;
         Status := (if T.Selected = T.Copy_Refused then R.Copy_Failed else R.Copy_OK);
         if T.Selected /= T.Copy_Refused then
            declare Raw:constant Bytes:=F.Image(Positive(ID));begin
               if Offset>Raw'Length then Status:=R.Invalid_Request;return;end if;
               Copied:=R.Byte_Count(Natural'Min(Into'Length,Raw'Length-Offset));
               Into(Into'First..Into'First+Natural(Copied)-1):=Raw(Offset+1..Offset+Natural(Copied));
            end;
         end if;
         Result.Last_Copy := (Available => True, Table => K.Table_ID (ID),
           Offset => K.Byte_Count (Offset), Capacity => K.Byte_Count (Into'Length),
           Copied => K.Byte_Count (Copied),
           Status => (if T.Selected = T.Copy_Refused then K.Copy_Failed else K.Ok));
      end Copy_Part;
      procedure Clock (Now : out R.Tick; Available : out Boolean) is
      begin Now := 10; Available := True; end Clock;
      package Actual_Load is new L.With_Driver
        (Device_Driver, Info, Describe, 4_096, Copy_Part, Clock, Execute_Module_Startup);
      Context : Actual_Load.Session;
   begin
      Owner := null; Result := (others => <>);
      if Was_Consumed then Result.Startup.Status := L.Startup_Already_Consumed; return; end if;
      Was_Consumed := True;
      T.Loads := T.Loads + 1; T.Passed_Deadline := Deadline;
      if T.Selected = T.Load_Exception then raise Program_Error with "modeled loader exception"; end if;
      Actual_Load.Load (Context, Run, Deadline, Owner, Result.Startup);
      T.Last_Load := Result.Startup;
      T.Sequence := T.Sequence + 1; T.Load_Completed_At := T.Sequence;
   end Load;
end ACPI_Native_Snapshot_Load.With_Driver;
