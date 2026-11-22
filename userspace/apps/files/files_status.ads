with Interfaces; use Interfaces;

--  What a filesystem answer means to the person (docs/files-app.md,
--  "Messages"): every reply label the service sends (CuBit.Filesystems
--  REPLY_*) as a typed kind, and each kind as words and a severity. No
--  number ever reaches the screen: a label this build does not know is
--  Unexpected, which has words too.
package Files_Status with SPARK_Mode is
   type Status_Kind is
     (Succeeded, Refused, No_Space, Read_Only, Out_Of_Range, Layout_Unsupported, Device_Error, Not_Granted,
      Damaged_Filesystem, Wrong_Kind, Already_Exists, Not_Found, Recovery_Required, Durability_Unsupported,
      Unsupported_Object, In_Use, Not_Empty, Is_Folder, Invalid_Move, Cross_Volume, Cancelled, Deadline_Passed,
      Busy, Volume_Unavailable, Unexpected);

   function Kind_Of (Status : Unsigned_32) return Status_Kind;

   --  How a kind is shown: an expected condition (a folder that is gone, a
   --  volume this machine lacks, a read-only place) in the muted ink; a
   --  genuine error (the device, a damaged filesystem, an answer that makes
   --  no sense) in the danger ink.
   type Severity is (Expected, Problem);
   function Severity_Of (Kind : Status_Kind) return Severity;

   --  Why a pane could not list its folder. At_Root: the folder is its
   --  place's root, so a name that is not found is a volume that is not.
   function Listing_Kind (Status : Unsigned_32; At_Root : Boolean) return Status_Kind;

   --  The pane's sentence ("This volume is not available.").
   function Listing_Text (Kind : Status_Kind) return String
     with Post => Listing_Text'Result'Length > 0;
   --  The clause after "Could not delete x: " ("the place is read-only").
   function Reason (Kind : Status_Kind) return String
     with Post => Reason'Result'Length > 0;
end Files_Status;
