pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Binding_Types;
package body ACPI_Backend_FADT_Native is
   package R renames Source.R;
   use type K.Outcome, K.Byte_Count, Source.Outcome;
   type Table_Access is access R.F.Bytes;
   Backing : Table_Access;
   Started : Boolean := False;
   Saved : Report;
   procedure Clock (Now : out R.Tick; Available : out Boolean) is
      Sample : ACPI_Backend_Binding_Types.Tick;
   begin
      ACPI_Backend_Kernel_Handles.Read_Clock (Sample, Available);
      Now := R.Tick (Sample); -- Both are monotonic microseconds, not IPC ms.
   end Clock;
   function Translate (Status : K.Outcome) return R.Copy_Status is
     (case Status is
       when K.Ok => R.Copy_OK,
       when K.Not_Ready => R.Not_Ready,
       when K.Not_Authorized => R.Not_Authorized,
       when K.Copy_Failed => R.Copy_Failed,
       when others => R.Invalid_Request);
   procedure Read
     (Authority : K.Authority; Deadline : Source.R.Tick; Budget : Natural;
      Item : out ACPI_FADT.Description; Result : out Report)
   is
      Run : Source.Context;
      procedure Info (Value : out Source.Inventory; Status : out R.Copy_Status) is
         Raw : K.Catalog_Info;
      begin
         K.Info (Authority, Raw, Saved.Last_Kernel_Status);
         Status := Translate (Saved.Last_Kernel_Status);
         Value := (Raw.Ready, Interfaces.Unsigned_64 (Raw.Table_Count),
           Interfaces.Unsigned_64 (Raw.Total_Bytes), Interfaces.Unsigned_64 (Raw.Largest_Table));
      end Info;
      procedure Describe (ID : Positive; Value : out Source.Description; Status : out R.Copy_Status) is
         Raw : K.Table_Description;
      begin
         Value := (others => <>);
         K.Describe (Authority, K.Table_ID (ID), Raw, Saved.Last_Kernel_Status);
         Status := Translate (Saved.Last_Kernel_Status);
         if Saved.Last_Kernel_Status /= K.Ok then return; end if;
         for I in K.Signature_Index loop Value.Name (I) := Character'Val (Raw.Signature (I)); end loop;
         Value.Revision := R.F.Byte (Raw.Revision); Value.Extent := Interfaces.Unsigned_64 (Raw.Extent);
      end Describe;
      procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status) is
         Chunk : K.Byte_Array (1 .. K.Chunk_Max) := [others => 0];
         Count : K.Byte_Count;
         Raw_Status : K.Outcome;
      begin
         Into := [others => 0];
         K.Copy_Part (Authority, K.Table_ID (ID), K.Byte_Count (Offset), Chunk, Count, Raw_Status);
         Saved.Last_Kernel_Status := Raw_Status;
         Saved.Last_Copy := (True, K.Table_ID (ID), K.Byte_Count (Offset), K.Chunk_Max, Count, Raw_Status);
         Status := Translate (Raw_Status); Copied := Interfaces.Unsigned_64 (Count);
         if Raw_Status /= K.Ok or else Count /= K.Byte_Count (Into'Length) then return; end if;
         if Into'Length = 0 then return; end if;
         for I in 0 .. Into'Length - 1 loop Into (Into'First + I) := R.F.Byte (Chunk (Chunk'First + I)); end loop;
      end Read_Part;
      package Adapter is new Source.Using_Source (Info, Describe, Read_Part);
   begin
      Item := (others => <>);
      if Started then
         Result := Saved; Result.Source_Result.Status := Source.Already_Consumed; return;
      end if;
      Started := True;
      Adapter.Inspect (Run, Deadline, Budget, Saved.Source_Result);
      if Saved.Source_Result.Status = Source.Metadata_Ready then
         Backing := new R.F.Bytes (1 .. Positive (Saved.Source_Result.Required_Bytes));
         Adapter.Read (Run, Backing.all, Item, Saved.Source_Result);
      end if;
      Result := Saved;
   end Read;
end ACPI_Backend_FADT_Native;
