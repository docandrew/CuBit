with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  The image interface (CCL.Interfaces.Images), answered in-process: it
--  draws into CCL.Image_Store and needs no service or authority.
package CCL_Image_Bindings is
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Image_Bindings;
