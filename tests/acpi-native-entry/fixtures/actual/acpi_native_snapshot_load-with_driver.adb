pragma Ada_2022;
with ACPI_Snapshot_Startup.With_Driver;
package body ACPI_Native_Snapshot_Load.With_Driver is
   Saved : Report;
   Active : Boolean := False;
   procedure Info (Item : out L.Inventory; Status : out L.R.Copy_Status) is
   begin Read_Catalog (Saved, Item, Status); end Info;
   procedure Describe
     (ID : Positive; Item : out L.Description; Status : out L.R.Copy_Status) is
   begin Read_Description (Saved, ID, Item, Status); end Describe;
   procedure Read_Part
     (ID : L.R.S.Table_ID; Offset : Natural; Into : out L.R.F.Bytes;
      Copied : out L.R.Byte_Count; Status : out L.R.Copy_Status) is
   begin Read_Chunk (Saved, ID, Offset, Into, Copied, Status); end Read_Part;
   package Adapter is new L.With_Driver
     (Device_Driver, Info, Describe, ACPI_Native_Snapshot_Read.K.Chunk_Max,
      Read_Part, Read_Clock, Execute_Module_Startup);
   Context : Adapter.Session;
   function Consumed return Boolean is (Adapter.Consumed (Context));
   procedure Load
     (Run : in out Device_Driver.Session; Deadline : L.R.Tick;
      Owner : out L.State_Access; Result : out Report) is
   begin
      if Active then
         Owner := null;
         Result := (others => <>);
         Result.Startup.Status := L.Startup_Already_Consumed;
         return;
      end if;
      Active := True;
      Adapter.Load (Context, Run, Deadline, Owner, Saved.Startup);
      Result := Saved;
      Active := False;
   end Load;
end ACPI_Native_Snapshot_Load.With_Driver;
