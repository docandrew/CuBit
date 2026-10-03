with Interfaces;

--  The process's images: immutable pixel buffers named by their content.
--  A CCL Image value (interfaces/image.schema) is (Image width height id),
--  and id is the content digest of the pixels kept here, so an Image is an
--  ordinary copyable value with equality by content. The id grants nothing:
--  it names pixels this process itself produced. The store is a bounded
--  cache; when it is full the least recently used image is replaced, and an
--  id no longer present is shown as expired, never as other pixels.
package CCL.Image_Store is
   Maximum_Side : constant := 512;
   Maximum_Pixels : constant := 65_536;   --  one image: 256 x 256, or 512 x 128
   --  All images share one pool; small ones (strips, icons) are many.
   Pool_Pixels : constant := 4 * Maximum_Pixels;
   Maximum_Images : constant := 64;
   subtype Side is Positive range 1 .. Maximum_Side;
   subtype Pixel is Interfaces.Unsigned_32;   --  16#RRGGBB#
   subtype Image_Id is Interfaces.Integer_64;
   No_Image : constant Image_Id := 0;

   --  Building one image at a time: Start, Set pixels, Finish. Finish gives
   --  the id (an existing one for identical content) and ends the build.
   function Fits (Width, Height : Side) return Boolean is
     (Width * Height <= Maximum_Pixels);
   procedure Start (Width, Height : Side; Fill : Pixel)
   with Pre => Fits (Width, Height);
   function Building return Boolean;
   function Draft_Width return Natural;
   function Draft_Height return Natural;
   procedure Set (X, Y : Natural; Value : Pixel);   --  outside: ignored
   function Draft_Pixel (X, Y : Natural) return Pixel;
   procedure Finish (Id : out Image_Id);
   --  Abandon the build: nothing is stored.
   procedure Discard;

   function Known (Id : Image_Id) return Boolean;
   --  Of a known image (0 otherwise).
   function Width (Id : Image_Id) return Natural;
   function Height (Id : Image_Id) return Natural;
   function Pixel_At (Id : Image_Id; X, Y : Natural) return Pixel;
end CCL.Image_Store;
