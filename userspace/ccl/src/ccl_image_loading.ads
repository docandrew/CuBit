with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  image.load: a QOI or PPM file from the host's authorized workspace
--  (CCL_Workspace), decoded into CCL.Image_Store. Separate from the drawing
--  bindings, which need no file access, so a host without a workspace
--  (ccl-control) grants drawing and not loading.
package CCL_Image_Loading is
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   --  After CCL_Image_Bindings.Install (which publishes the interface).
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Image_Loading;
