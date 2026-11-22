with Interfaces;
with CuBit.QOI;
--  Reading one file of an asset package (docs/assets.md): the asset root is
--  the system setting system.assets.root (e.g. "@nvme:0/Assets/"), and a
--  file is <root><package>/<version>/<file>. The file is read through the
--  filesystem's request queue (the manifest's `filesystem` binding), in
--  chunks copied out of the shared arena, until its end, a size bound, the
--  caller's sink refusing more, or one explicit deadline for the whole read.
--  The request queue opens on the first read and stays open while reads end
--  cleanly (or with a clean "not found"/"access denied" answer).
--  Desktop's wallpapers and icons both load through here.
package Desktop_Asset_Files is

   --  Why a read ended. Only Complete and Refused read the whole stream the
   --  sink wanted; every other outcome means the asset is unavailable.
   type Read_Status is
     (Complete,        --  read to the end of the file
      Refused,         --  the sink stopped wanting more (it rejected the data)
      No_Asset_Root,   --  system.assets.root missing or malformed
      No_Filesystem,   --  the filesystem request queue could not be opened
      Open_Failed,     --  the file could not be opened (Open_Reply says why)
      Read_Failed,     --  a read request failed or passed the deadline
      Too_Large);      --  the file is longer than the caller's bound

   type Read_Result is record
      Status     : Read_Status := No_Asset_Root;
      Bytes      : Interfaces.Unsigned_64 := 0;  --  read before it ended
      Open_Reply : Interfaces.Unsigned_32 := 0;  --  the open's reply status
      Elapsed_Ms : Interfaces.Unsigned_64 := 0;
   end record;

   function Read_Whole (Result : Read_Result) return Boolean is
     (Result.Status in Complete | Refused);

   --  A reason for a log line, e.g. "open failed (not found)".
   function Describe (Result : Read_Result) return String;

   --  The longest asset path (root and package file) that is read.
   Maximum_Path : constant := 192;

   --  Read <root><File> (File: "<package>/<version>/<file>"). Path is the
   --  full path for logs (Path (1 .. Path_Length)), empty without a root.
   generic
      with procedure Feed (Input : CuBit.QOI.Byte_Array);
      with function Wants_More return Boolean;
   procedure Read
     (File        : String;
      Max_Bytes   : Interfaces.Unsigned_64;
      Budget_Ms   : Natural;
      Result      : out Read_Result;
      Path        : out String;
      Path_Length : out Natural)
     with Pre => Path'First = 1 and then Path'Length = Maximum_Path;
end Desktop_Asset_Files;
