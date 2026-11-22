with CuBit.Filesystems; use CuBit.Filesystems;

package body Files_Status with SPARK_Mode is

   function Kind_Of (Status : Unsigned_32) return Status_Kind is
     (case Status is
         when REPLY_OK => Succeeded,
         when REPLY_ERR => Refused,
         when REPLY_NO_SPACE => No_Space,
         when REPLY_READ_ONLY => Read_Only,
         when REPLY_OUT_OF_RANGE => Out_Of_Range,
         when REPLY_FILE_RANGE_UNSUPPORTED => Layout_Unsupported,
         when REPLY_IO_ERROR => Device_Error,
         when REPLY_ACCESS_DENIED => Not_Granted,
         when REPLY_MALFORMED_FILESYSTEM => Damaged_Filesystem,
         when REPLY_WRONG_OBJECT_TYPE => Wrong_Kind,
         when REPLY_ALREADY_EXISTS => Already_Exists,
         when REPLY_NOT_FOUND => Not_Found,
         when REPLY_RECOVERY_REQUIRED => Recovery_Required,
         when REPLY_DURABILITY_UNSUPPORTED => Durability_Unsupported,
         when REPLY_UNSUPPORTED_OBJECT => Unsupported_Object,
         when REPLY_SHARING_VIOLATION => In_Use,
         when REPLY_NOT_EMPTY => Not_Empty,
         when REPLY_IS_DIRECTORY => Is_Folder,
         when REPLY_INVALID_MOVE => Invalid_Move,
         when REPLY_CROSS_VOLUME => Cross_Volume,
         when REPLY_CANCELLED => Cancelled,
         when REPLY_DEADLINE => Deadline_Passed,
         when REPLY_BUSY => Busy,
         when REPLY_VOLUME_UNAVAILABLE => Volume_Unavailable,
         when others => Unexpected);

   function Severity_Of (Kind : Status_Kind) return Severity is
     (case Kind is
         when Succeeded | Read_Only | Not_Granted | Wrong_Kind | Already_Exists | Not_Found | Unsupported_Object
            | In_Use | Not_Empty | Is_Folder | Invalid_Move | Cross_Volume | Cancelled | Busy
            | Volume_Unavailable => Expected,
         when Refused | No_Space | Out_Of_Range | Layout_Unsupported | Device_Error | Damaged_Filesystem
            | Recovery_Required | Durability_Unsupported | Deadline_Passed | Unexpected => Problem);

   function Listing_Kind (Status : Unsigned_32; At_Root : Boolean) return Status_Kind is
     (if At_Root and then Kind_Of (Status) = Not_Found then Volume_Unavailable else Kind_Of (Status));

   function Listing_Text (Kind : Status_Kind) return String is
     (case Kind is
         when Succeeded => "This folder is listed.",
         when Refused => "The filesystem refused to list this folder.",
         when No_Space => "There is no room left to list this folder.",
         when Read_Only => "This place is read-only.",
         when Out_Of_Range => "The filesystem could not reach this folder (out of range).",
         when Layout_Unsupported => "This folder's layout is not supported yet.",
         when Device_Error => "The storage device reported an error reading this folder.",
         when Not_Granted => "Files is not granted this folder.",
         when Damaged_Filesystem => "The filesystem sent a malformed listing; showing what was valid.",
         when Wrong_Kind => "This is not a folder.",
         when Already_Exists => "A folder of this name already exists.",
         when Not_Found => "This folder does not exist.",
         when Recovery_Required => "This volume needs recovery before it can be read.",
         when Durability_Unsupported => "This volume cannot keep changes safely.",
         when Unsupported_Object => "This kind of folder is not supported.",
         when In_Use => "Another program is using this folder.",
         when Not_Empty => "This folder is not empty.",
         when Is_Folder => "This is a folder.",
         when Invalid_Move => "A folder cannot go inside itself.",
         when Cross_Volume => "This folder is on another volume.",
         when Cancelled => "Listing this folder was cancelled.",
         when Deadline_Passed => "The filesystem did not answer in time.",
         when Busy => "The filesystem is busy; refresh to try again.",
         when Volume_Unavailable => "This volume is not available.",
         when Unexpected => "The filesystem gave an answer Files does not understand.");

   function Reason (Kind : Status_Kind) return String is
     (case Kind is
         when Succeeded => "done",
         when Refused => "the filesystem refused it (an invalid name?)",
         when No_Space => "there is no space left on the volume",
         when Read_Only => "the place is read-only",
         when Out_Of_Range => "it is out of range",
         when Layout_Unsupported => "its layout is not supported yet",
         when Device_Error => "the storage device reported an error",
         when Not_Granted => "not granted to Files",
         when Damaged_Filesystem => "the filesystem is damaged",
         when Wrong_Kind => "it is not the kind of object expected",
         when Already_Exists => "the name exists",
         when Not_Found => "it does not exist",
         when Recovery_Required => "the volume needs recovery",
         when Durability_Unsupported => "the volume cannot keep changes safely",
         when Unsupported_Object => "this kind of object is not supported",
         when In_Use => "another program is using it",
         when Not_Empty => "the folder is not empty",
         when Is_Folder => "a folder is in the way",
         when Invalid_Move => "a folder cannot go inside itself",
         when Cross_Volume => "it is on another volume",
         when Cancelled => "it was cancelled",
         when Deadline_Passed => "the filesystem did not answer in time",
         when Busy => "the filesystem is busy",
         when Volume_Unavailable => "the volume is not available",
         when Unexpected => "the filesystem gave an answer Files does not understand");
end Files_Status;
