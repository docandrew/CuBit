------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reply label (CuBit.Filesystems REPLY_*) each Ext2 outcome is
--  answered with, proved (SPARK): success and only success is REPLY_OK.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Filesystems; use CuBit.Filesystems;
with Ext2;
with Volume_Admission; use Volume_Admission;

package Fs_Reply_Labels with SPARK_Mode is
   use type Ext2.Directory_Lookup_Status, Ext2.Write_Status, Ext2.Read_Status,
            Ext2.Remove_Status, Ext2.Truncate_Status, Ext2.Flush_Status,
            Ext2.Rename_Status, Ext2.Directory_Read_Status;

   --  A volume a request names that is not admitted (ensureVolume): one
   --  whose device is not there (no driver attached it, or it offered
   --  none) is unavailable; any other failure is the device's or its
   --  filesystem's.
   function Admission_Label (Result : Admission_Result) return Unsigned_32 is
     (case Result is
        when Admitted => REPLY_OK,
        when Provider_Not_Ready | No_Device => REPLY_VOLUME_UNAVAILABLE,
        when Insufficient_Resources | Grant_Rejected | Invalid_Session | Device_Error | Invalid_Description
           | Unsupported_Filesystem | Invalid_Filesystem => REPLY_IO_ERROR)
     with Post => (Admission_Label'Result = REPLY_OK) = (Result = Admitted);

   --  A path lookup (open, rename, unlink, mkdir, rmdir, open directory).
   function Lookup_Label (Status : Ext2.Directory_Lookup_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Lookup_Found => REPLY_OK,
        when Ext2.Lookup_Not_Found => REPLY_NOT_FOUND,
        when Ext2.Lookup_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Lookup_Device_Error => REPLY_IO_ERROR,
        when Ext2.Lookup_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Lookup_Range_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED)
     with Post => (Lookup_Label'Result = REPLY_OK) = (Status = Ext2.Lookup_Found);

   --  A child name in a directory handle (OPEN_CHILD_DIRECTORY): a missing
   --  name is a plain error there.
   function Child_Lookup_Label (Status : Ext2.Directory_Lookup_Status) return Unsigned_32 is
     (if Status = Ext2.Lookup_Not_Found then REPLY_ERR else Lookup_Label (Status))
     with Post => (Child_Lookup_Label'Result = REPLY_OK) = (Status = Ext2.Lookup_Found);

   function Write_Label (Status : Ext2.Write_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Write_Complete => REPLY_OK,
        when Ext2.Write_Read_Only => REPLY_READ_ONLY,
        when Ext2.Write_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Write_Device_Error => REPLY_IO_ERROR,
        when Ext2.Write_Recovery_Required => REPLY_RECOVERY_REQUIRED,
        when Ext2.Write_Already_Exists => REPLY_ALREADY_EXISTS,
        when Ext2.Write_No_Space => REPLY_NO_SPACE,
        when Ext2.Write_File_Range_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED,
        when Ext2.Write_Object_Unsupported => REPLY_UNSUPPORTED_OBJECT)
     with Post => (Write_Label'Result = REPLY_OK) = (Status = Ext2.Write_Complete);

   function Read_Label (Status : Ext2.Read_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Read_Complete => REPLY_OK,
        when Ext2.Read_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Read_Device_Error => REPLY_IO_ERROR,
        when Ext2.Read_File_Range_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED,
        when Ext2.Read_Object_Unsupported => REPLY_UNSUPPORTED_OBJECT)
     with Post => (Read_Label'Result = REPLY_OK) = (Status = Ext2.Read_Complete);

   --  A directory's own inode read for a directory handle: out of range or
   --  an I/O error.
   function Directory_Inode_Label (Status : Ext2.Read_Status) return Unsigned_32 is
     (if Status = Ext2.Read_Out_Of_Range then REPLY_OUT_OF_RANGE else REPLY_IO_ERROR)
     with Pre => Status /= Ext2.Read_Complete;

   function Remove_Label (Status : Ext2.Remove_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Remove_Complete => REPLY_OK,
        when Ext2.Remove_Not_Found => REPLY_NOT_FOUND,
        when Ext2.Remove_Invalid_Name => REPLY_ERR,
        when Ext2.Remove_Wrong_Type => REPLY_WRONG_OBJECT_TYPE,
        when Ext2.Remove_Not_Empty => REPLY_NOT_EMPTY,
        when Ext2.Remove_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Remove_Unsupported => REPLY_UNSUPPORTED_OBJECT,
        when Ext2.Remove_Read_Only => REPLY_READ_ONLY,
        when Ext2.Remove_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Remove_IO_Error => REPLY_IO_ERROR,
        when Ext2.Remove_Durability_Unsupported => REPLY_DURABILITY_UNSUPPORTED,
        when Ext2.Remove_Recovery_Required => REPLY_RECOVERY_REQUIRED)
     with Post => (Remove_Label'Result = REPLY_OK) = (Status = Ext2.Remove_Complete);

   --  Truncation at open and resize.
   function Truncate_Label (Status : Ext2.Truncate_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Truncate_Complete => REPLY_OK,
        when Ext2.Truncate_Recovery_Required => REPLY_RECOVERY_REQUIRED,
        when Ext2.Truncate_Read_Only => REPLY_READ_ONLY,
        when Ext2.Truncate_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED,
        when Ext2.Truncate_Durability_Unsupported => REPLY_DURABILITY_UNSUPPORTED,
        when Ext2.Truncate_Invalid | Ext2.Truncate_IO_Error => REPLY_IO_ERROR)
     with Post => (Truncate_Label'Result = REPLY_OK) = (Status = Ext2.Truncate_Complete);

   function Flush_Label (Status : Ext2.Flush_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Flush_Complete => REPLY_OK,
        when Ext2.Flush_Unsupported => REPLY_DURABILITY_UNSUPPORTED,
        when Ext2.Flush_IO_Error => REPLY_IO_ERROR,
        when Ext2.Flush_Recovery_Required => REPLY_RECOVERY_REQUIRED)
     with Post => (Flush_Label'Result = REPLY_OK) = (Status = Ext2.Flush_Complete);

   function Rename_Label (Status : Ext2.Rename_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Rename_Complete => REPLY_OK,
        when Ext2.Rename_Source_Not_Found => REPLY_NOT_FOUND,
        when Ext2.Rename_Destination_Exists => REPLY_ALREADY_EXISTS,
        when Ext2.Rename_Invalid_Name => REPLY_ERR,
        when Ext2.Rename_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Rename_Range_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED,
        when Ext2.Rename_Read_Only => REPLY_READ_ONLY,
        when Ext2.Rename_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Rename_IO_Error => REPLY_IO_ERROR,
        when Ext2.Rename_Recovery_Required => REPLY_RECOVERY_REQUIRED,
        when Ext2.Rename_Not_Directory => REPLY_WRONG_OBJECT_TYPE,
        when Ext2.Rename_Is_Directory => REPLY_IS_DIRECTORY,
        when Ext2.Rename_Not_Empty => REPLY_NOT_EMPTY,
        when Ext2.Rename_Invalid_Move => REPLY_INVALID_MOVE,
        when Ext2.Rename_No_Room => REPLY_FILE_RANGE_UNSUPPORTED)
     with Post => (Rename_Label'Result = REPLY_OK) = (Status = Ext2.Rename_Complete);

   --  A directory page: a full page and the directory's end both succeed.
   function Directory_Read_Label (Status : Ext2.Directory_Read_Status) return Unsigned_32 is
     (case Status is
        when Ext2.Directory_Page_Complete | Ext2.Directory_End => REPLY_OK,
        when Ext2.Directory_Malformed => REPLY_MALFORMED_FILESYSTEM,
        when Ext2.Directory_Device_Error => REPLY_IO_ERROR,
        when Ext2.Directory_Out_Of_Range => REPLY_OUT_OF_RANGE,
        when Ext2.Directory_Range_Unsupported => REPLY_FILE_RANGE_UNSUPPORTED)
     with Post => (Directory_Read_Label'Result = REPLY_OK) =
                  (Status in Ext2.Directory_Page_Complete | Ext2.Directory_End);

end Fs_Reply_Labels;
