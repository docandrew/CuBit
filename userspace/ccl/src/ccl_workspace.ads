--  Workbench storage boundary, not an implicit file-I/O facility for CCL code.
with CuBit.Failures;
with CuBit.File_Selection;
package CCL_Workspace is
   Maximum_Source_Bytes : constant := 4_096;
   subtype Source_Length is Natural range 0 .. Maximum_Source_Bytes;
   subtype Source_Buffer is String (1 .. Maximum_Source_Bytes);
   type Storage_Result is
     (Succeeded, Unavailable, Limit_Reached, Conflict,
      Access_Denied, IO_Failed, Recovery_Required, Invalid_Source,
      Invalid_Name, Not_Found);
   function Supported return Boolean;

   --  Why a storage request on Name did not succeed, for the person who made it.
   function Failure_Of (Result : Storage_Result; Name : String) return CuBit.Failures.Failure is
     (case Result is
         when Succeeded => (others => <>),
         when Unavailable => CuBit.Failures.Failed
           (CuBit.Failures.Unavailable, "no workspace is open for this program",
            "the program's manifest must request the filesystem service and declare a place, such as " &
            "(filesystem-scope (rights read write create) ""@nvme:0/work"")"),
         when Limit_Reached => CuBit.Failures.Failed
           (CuBit.Failures.Exhausted, Name & " is larger than this program reads at once"),
         when Conflict => CuBit.Failures.Failed
           (CuBit.Failures.Refused, Name & " already exists, and saving never replaces a file",
            "save under a new name"),
         when Access_Denied => CuBit.Failures.Failed
           (CuBit.Failures.Outside_Scope, Name & " is outside this program's filesystem scope",
            "the program's manifest must declare a filesystem-scope with the rights this needs"),
         when IO_Failed => CuBit.Failures.Failed
           (CuBit.Failures.Device_Error, "the filesystem service could not transfer " & Name),
         when Recovery_Required => CuBit.Failures.Failed
           (CuBit.Failures.Device_Error, "the volume holding " & Name & " needs recovery first"),
         when Invalid_Source => CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument, Name & " does not hold what was expected"),
         when Invalid_Name => CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument, """" & Name & """ is not a file name in the workspace",
            "give one name, without '/', and with the expected extension"),
         when Not_Found => CuBit.Failures.Failed
           (CuBit.Failures.Not_Found, "the workspace has no file named " & Name));
   function Location return String;
   procedure Shutdown;
   --  Names are relative to the already-authorized workspace, not arbitrary
   --  paths or a means of requesting additional authority. Save never replaces.
   function Valid_Source_Name (Name : String) return Boolean is
     (CuBit.File_Selection.Valid_Leaf (Name) and then Name'Length > 4 and then
      Name (Name'Last - 3 .. Name'Last) = ".ccl");
   procedure List_Files
     (Files : out CuBit.File_Selection.File_List; Result : out Storage_Result);
   procedure Load
     (Name : String; Text : out Source_Buffer; Length : out Source_Length;
      Result : out Storage_Result);
   procedure Save_New (Name, Text : String; Result : out Storage_Result);

   --  Image files (image.load): QOI or binary PPM, read a chunk at a time.
   function Valid_Image_Name (Name : String) return Boolean is
     (CuBit.File_Selection.Valid_Leaf (Name) and then Name'Length > 4 and then
      (Name (Name'Last - 3 .. Name'Last) = ".qoi" or else
       Name (Name'Last - 3 .. Name'Last) = ".ppm"));
   Maximum_Chunk : constant := 4_096;
   --  The most a binary read delivers: a full-size store image as PPM.
   Maximum_Binary_Bytes : constant := 1_048_576;
   generic
      --  Each chunk in order; stop by clearing Keep_Going.
      with procedure Consume (Chunk : String; Keep_Going : out Boolean);
   procedure Read_Binary (Name : String; Result : out Storage_Result);
   procedure Suggest_Name
     (Name : out CuBit.File_Selection.File_Name; Result : out Storage_Result);
end CCL_Workspace;
