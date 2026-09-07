--  Workbench storage boundary, not an implicit file-I/O facility for CCL code.
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
   procedure Suggest_Name
     (Name : out CuBit.File_Selection.File_Name; Result : out Storage_Result);
end CCL_Workspace;
