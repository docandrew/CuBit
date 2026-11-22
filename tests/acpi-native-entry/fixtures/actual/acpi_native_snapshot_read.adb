pragma Ada_2022;
with Interfaces;
with AML_Decode;
with ACPI_Clock_Source;
with Startup;
package body ACPI_Native_Snapshot_Read is
   use type K.Outcome;
   use type K.Byte_Count;
   procedure Copy_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status;
      Observation : out Kernel_Observation) is
      Chunk : K.Byte_Array (1 .. K.Chunk_Max) := [others => 0];
      Read_Bytes : K.Byte_Count;
      Kernel_Status : K.Outcome;
      At_Byte : Positive := Into'First;
   begin
      Into := [others => 0];
      K.Copy_Part (Startup.Tables, K.Table_ID (ID), K.Byte_Count (Offset),
                   Chunk, Read_Bytes, Kernel_Status);
      Observation := (Available => True, Table => K.Table_ID (ID),
        Offset => K.Byte_Count (Offset), Capacity => K.Byte_Count (K.Chunk_Max),
        Copied => Read_Bytes, Status => Kernel_Status);
      Copied := Interfaces.Unsigned_64 (Read_Bytes);
      Status := (case Kernel_Status is
        when K.Ok => R.Copy_OK,
        when K.Not_Ready => R.Not_Ready,
        when K.Not_Authorized => R.Not_Authorized,
        when K.Copy_Failed => R.Copy_Failed,
        when others => R.Invalid_Request);
      if Kernel_Status /= K.Ok or else Read_Bytes /= K.Byte_Count (Into'Length) then return; end if;
      if Into'Length = 0 then return; end if;
      for Byte of Chunk loop
         Into (At_Byte) := R.F.Byte (Byte);
         exit when At_Byte = Into'Last;
         At_Byte := At_Byte + 1;
      end loop;
   end Copy_Part;
   procedure Read
     (Item : R.S.Public_Descriptor; Deadline : R.Tick;
      Data : out R.F.Bytes; Result : out Report) is
      Last_Call : Kernel_Observation;
      procedure Clock (Now : out R.Tick; Available : out Boolean) is
         Value : AML_Decode.Integer_Value;
      begin
         ACPI_Clock_Source.Read (Value, Available);
         Now := R.Tick (Value);
      end Clock;
      procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status) is
      begin
         Copy_Part (ID, Offset, Into, Copied, Status, Last_Call);
      end Read_Part;
      procedure Read_Table is new R.Read_Table (K.Chunk_Max, Read_Part, Clock);
   begin
      Read_Table (Item, Deadline, Data, Result.Copy);
      Result.Last_Kernel_Call := Last_Call;
   end Read;
end ACPI_Native_Snapshot_Read;
