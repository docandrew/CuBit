with Interfaces;
with CBOR;
with CCL.Objects.Persistence;

--  Private in-process database adapter ABI, not an IPC protocol. All output
--  scalars admit arbitrary bit patterns; the worker validates before use.
package Config_Worker_Storage with SPARK_Mode is
   Reply_Bytes : constant := 16 + CCL.Objects.Persistence.Maximum_Encoded_Bytes;
   type Reply is record
      Code : Interfaces.Unsigned_32 := 0;
      Length : Interfaces.Unsigned_32 := 0;
      Revision : Interfaces.Unsigned_64 := 0;
      Data : CBOR.Byte_Array (1 .. CCL.Objects.Persistence.Maximum_Encoded_Bytes) := [others => 0];
   end record with Size => Reply_Bytes * 8, Alignment => 8;
   for Reply use record
      Code at 0 range 0 .. 31;
      Length at 4 range 0 .. 31;
      Revision at 8 range 0 .. 63;
      Data at 16 range 0 .. CCL.Objects.Persistence.Maximum_Encoded_Bytes * 8 - 1;
   end record;
end Config_Worker_Storage;
