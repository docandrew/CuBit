with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  fs.* (interfaces/fs.schema): the process's places and their listings,
--  through CCL_Places (the filesystem service natively).
package CCL_File_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
   --  A child's name: one component, never "." or "..", no separators.
   function Valid_Name (Name : String) return Boolean;
end CCL_File_Bindings;
