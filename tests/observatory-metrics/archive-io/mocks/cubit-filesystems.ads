with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
package CuBit.Filesystems is
   type File_Handle is new Unsigned_64;
   INVALID_FILE_HANDLE : constant File_Handle := 0;
   REPLY_OK : constant Unsigned_32 := 16#F000#;
   OPEN_READ_ONLY : constant Unsigned_64 := 0;
   function Open_Request (Loan : CuBit.Memory_Grants.Grant_Reference; Count : Natural; Options : Unsigned_64) return CuBit.Messages.Message;
   function Read_At_Request (Handle : File_Handle; Loan : CuBit.Memory_Grants.Grant_Reference; Count, Offset : Unsigned_64) return CuBit.Messages.Message;
   function Close_Request (Handle : File_Handle) return CuBit.Messages.Message;
end CuBit.Filesystems;
