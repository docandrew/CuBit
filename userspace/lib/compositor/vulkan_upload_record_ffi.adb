with Interfaces;
package body Vulkan_Upload_Record_FFI with SPARK_Mode => Off is
   package G renames Compositor_Upload;
   subtype U32 is Interfaces.Unsigned_32;
   type Native_Region is record
      Image_Width, Image_Height, X, Y, Width, Height, Offset, Row_Pixels, Mask, Discard : U32;
   end record with Convention => C;
   function Native_Record (Context, Upload, Image : System.Address; Region : access Native_Region) return U32
     with Import, Convention => C, External_Name => "cubit_vulkan_upload_record";
   procedure Record_Transfer (Context, Upload, Image : System.Address;
      Plan : G.Plan; Discard : Boolean; Accepted : out Boolean) is
      use type U32, G.Pixel_Format;
      Region : constant G.Rectangle := G.Area (Plan);
      Native : aliased Native_Region := (U32 (G.Image_Width (Plan)), U32 (G.Image_Height (Plan)),
        U32 (Region.X), U32 (Region.Y), U32 (Region.Width), U32 (Region.Height),
        U32 (G.Buffer_Offset (Plan)), U32 (G.Row_Length (Plan)),
        (if G.Format (Plan) = G.R8 then 1 else 0), (if Discard then 1 else 0));
   begin
      Accepted := Native_Record (Context, Upload, Image, Native'Access) = 0;
   end Record_Transfer;
end Vulkan_Upload_Record_FFI;
