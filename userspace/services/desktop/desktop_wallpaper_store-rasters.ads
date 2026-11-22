with System;
--  Audited pointer bridge: the wallpaper store's two CuAlloc tables
--  (userspace/allocator, the process's one allocator), each sized to the
--  catalog by Reserve: every image's Status, and every image's raster (its
--  own CuAlloc block, taken once its QOI header has given its size). Every
--  query is total: an image the table does not track, or without a raster,
--  answers Not_Loaded, no size and a null address. The two tables are
--  separate states, so raster work provably leaves every Status alone.
private package Desktop_Wallpaper_Store.Rasters with SPARK_Mode,
  Abstract_State => ((Status_Table with Part_Of => Desktop_Wallpaper_Store.State),
                     (Raster_Table with Part_Of => Desktop_Wallpaper_Store.State)),
  Initializes => (Status_Table, Raster_Table)
is
   use type Q.Phase, Q.Failure;
   function Tracked return Image_Count with Global => Status_Table;
   --  Room for Count images, each Not_Loaded and without a raster. Done is
   --  False (and nothing is tracked) when the heap is out.
   procedure Reserve (Count : Image_Count; Done : out Boolean)
     with Global => (In_Out => (Status_Table, Raster_Table)),
          Pre  => Tracked = 0,
          Post => (if Done then Tracked = Count and then
                     (for all Asset in 1 .. Count => Status_Of (Asset) = Not_Loaded)
                   else Tracked = 0);

   function Status_Of (Asset : Image) return Status with Global => Status_Table;
   procedure Set_Status (Asset : Image; Value : Status)
     with Global => (In_Out => Status_Table),
          Pre  => Asset <= Tracked,
          Post => Tracked = Tracked'Old and then Status_Of (Asset) = Value;

   --  The raster's pixel count (0: none), size and first pixel.
   function Raster_Pixels (Asset : Image) return Q.Pixel_Count with Global => Raster_Table;
   function Width_Of (Asset : Image) return Q.Side with Global => Raster_Table;
   function Height_Of (Asset : Image) return Q.Side with Global => Raster_Table;
   function Address_Of (Asset : Image) return System.Address with Global => Raster_Table;

   --  A Width x Height raster for Asset; Done is False when the heap is out.
   procedure Allocate (Asset : Image; Width, Height : Q.Side; Done : out Boolean)
     with Global => (In_Out => Raster_Table, Proof_In => Status_Table),
          Pre  => Asset <= Tracked and then Raster_Pixels (Asset) = 0 and then
                  Width * Height <= Q.Maximum_Pixels,
          Post => (if Done then Raster_Pixels (Asset) = Width * Height and then
                     Width_Of (Asset) = Width and then Height_Of (Asset) = Height
                   else Raster_Pixels (Asset) = 0);
   --  Decode Input into Asset's raster (CuBit.QOI.Feed on it).
   procedure Feed (Asset : Image; D : in out Q.Decoder; Input : Q.Byte_Array)
     with Global => (In_Out => Raster_Table),
          Pre  => Q.Valid (D) and then Raster_Pixels (Asset) > 0 and then
                  Q.Limit (D) <= Raster_Pixels (Asset),
          Post => Raster_Pixels (Asset) = Raster_Pixels (Asset)'Old and then
                  Q.Valid (D) and then Q.Limit (D) = Q.Limit (D'Old) and then
                  Q.Written (D) >= Q.Written (D'Old) and then
                  Q.Written (D) <= Q.Total (D) and then Q.Total (D) <= Q.Limit (D) and then
                  (if Q.Current (D'Old) = Q.Failed then Q.Current (D) = Q.Failed) and then
                  (if Q.Current (D) = Q.Complete then Q.Written (D) = Q.Total (D)) and then
                  (if Q.Current (D) = Q.Failed then Q.Error (D) /= Q.No_Failure);
   --  Return Asset's raster to the heap (an image that did not decode).
   procedure Free (Asset : Image)
     with Global => (In_Out => Raster_Table),
          Post => Raster_Pixels (Asset) = 0;
end Desktop_Wallpaper_Store.Rasters;
