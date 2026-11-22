with Interfaces;
with Intel_GPU_Firmware;
package Intel_GPU_Firmware_Reader is
   use Interfaces;
   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Read_Status is
     (Loaded, Invalid_Size, Read_Failed, Invalid_Reply, Truncated, Invalid_Layout);
   --  Bring-up resource budget, not a hardware or firmware-format maximum.
   Maximum_Blob_Bytes : constant := 1024 * 1024;
   Chunk_Bytes : constant := 4096;

   generic
      --  Read at most Destination'Length bytes at Offset from an already-open
      --  read-only file. Complete/revoke any IPC loan BEFORE returning.
      --  Success with Count=0 means EOF. Do not retain Destination's address.
      --  The adapter must impose a deadline on IPC; this routine cannot
      --  interrupt a callback. Positive short reads guarantee loop progress.
      with procedure Read_At
        (Offset : Unsigned_64; Destination : out Byte_Array;
         Count : out Unsigned_64; Success : out Boolean);
   procedure Load
     (File_Bytes : Unsigned_64; Buffer : out Byte_Array;
      Status : out Read_Status; Plan : out Intel_GPU_Firmware.Layout);
   --  Buffer must remain private to the caller throughout the operation.
   --  Loaded means bytes were read and CSS layout fits, NOT authentication,
   --  compatibility, version admission, or permission to upload. Only the
   --  first File_Bytes bytes are meaningful on success; none on failure.
end Intel_GPU_Firmware_Reader;
