pragma Ada_2022;
with Interfaces;
with AML_Decode;
with ACPI_Clock_Source;
with Startup;
package body ACPI_Native_Snapshot_Load is
   package K renames ACPI_Native_Snapshot_Read.K;
   package R renames L.R;
   use type K.Outcome;
   function Translate (Status : K.Outcome) return R.Copy_Status is
     (case Status is
       when K.Ok => R.Copy_OK,
       when K.Not_Ready => R.Not_Ready,
       when K.Not_Authorized => R.Not_Authorized,
       when K.Copy_Failed => R.Copy_Failed,
       when others => R.Invalid_Request);
   procedure Load
     (Deadline : R.Tick; Owner : out L.State_Access; Result : out Report) is
      procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
         Catalog : K.Catalog_Info;
      begin
         K.Info (Startup.Tables, Catalog, Result.Catalog_Status);
         Status := Translate (Result.Catalog_Status);
         Item := (Catalog.Ready, Interfaces.Unsigned_64 (Catalog.Table_Count),
           Interfaces.Unsigned_64 (Catalog.Total_Bytes),
           Interfaces.Unsigned_64 (Catalog.Largest_Table));
      end Info;
      procedure Describe
        (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
         Raw : K.Table_Description;
      begin
         Item := (others => <>);
         K.Describe (Startup.Tables, K.Table_ID (ID), Raw, Result.Descriptor_Status);
         Status := Translate (Result.Descriptor_Status);
         if Result.Descriptor_Status /= K.Ok then return; end if;
         for I in K.Signature_Index loop
            Item.Name (I) := Character'Val (Raw.Signature (I));
         end loop;
         Item.Revision := R.F.Byte (Raw.Revision);
         Item.Extent := Interfaces.Unsigned_64 (Raw.Extent);
      end Describe;
      procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status) is
      begin
         ACPI_Native_Snapshot_Read.Copy_Part
           (ID, Offset, Into, Copied, Status, Result.Last_Copy);
      end Read_Part;
      procedure Clock (Now : out R.Tick; Available : out Boolean) is
         Value : AML_Decode.Integer_Value;
      begin
         ACPI_Clock_Source.Read (Value, Available);
         Now := R.Tick (Value);
      end Clock;
      procedure Direct_Load is new L.Load (Info, Describe, K.Chunk_Max, Read_Part, Clock);
   begin
      Result := (others => <>);
      Direct_Load (Deadline, Owner, Result.Startup);
   end Load;
end ACPI_Native_Snapshot_Load;
