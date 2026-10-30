pragma Ada_2022;
with Interfaces;
with Firmware_Tables.Snapshots;
-- Direct snapshot copying into service-owned storage, before local admission.
-- No IPC/provider/grant state, allocator, source address or hardware access.
package ACPI_Snapshot_Reader with SPARK_Mode is
   package F renames Firmware_Tables;
   package S renames F.Snapshots;
   use type F.Byte;
   type Tick is new Interfaces.Unsigned_64;
   subtype Byte_Count is Interfaces.Unsigned_64;
   type Copy_Status is (Copy_OK, Not_Ready, Not_Authorized, Copy_Failed,
                        Invalid_Request);
   type Outcome is (Copied_Table, Wrong_Capacity, Invalid_Descriptor,
      Clock_Unavailable, Clock_Reversed, Deadline_Expired, Source_Refused,
      Short_Copy, Excess_Copy, Table_Refused, Metadata_Mismatch);
   type Report is record
      Status : Outcome := Wrong_Capacity;
      Requests, Offset, Wanted, Total_Copied : Natural := 0;
      Returned : Byte_Count := 0;
      Copy_Attempted : Boolean := False;
      Source_Status : Copy_Status := Copy_OK;
      Header_Status : F.Admission := F.Truncated;
   end record;
   generic
      -- Native instantiation takes CuBit.Firmware_Snapshot.Chunk_Max,4096.
      Chunk_Max : Positive := 4_096;
      -- Trusted bounded adapters capture Startup.Tables, not wire authority.
      -- They do not reenter, mutate Item, or retain/modify Into after return.
      -- A failed copy may have written a prefix: the ENTIRE table is discarded.
      with procedure Read_Part
        (ID : S.Table_ID; Offset : Natural; Into : out F.Bytes;
         Copied : out Byte_Count; Status : out Copy_Status);
      with procedure Clock (Now : out Tick; Available : out Boolean);
   procedure Read_Table
     (Item : S.Public_Descriptor; Deadline : Tick; Data : out F.Bytes;
      Result : out Report)
     with Post => (if Result.Status /= Copied_Table then
       (for all B of Data => B = 0));
   -- Data is exactly descriptor-sized; allocate it from Extent after checking
   -- runtime full-width values against representability/allocation policy.
   -- No64KiB/1MiB table ceiling is imposed here. Header signature, exact extent,
   -- revision and checksum must match before Copied_Table. That outcome means
   -- copied/validated bytes, not AML execution, service readiness or authority.
   -- Every chunk uses the same absolute monotonic deadline, with no retries.
end ACPI_Snapshot_Reader;
