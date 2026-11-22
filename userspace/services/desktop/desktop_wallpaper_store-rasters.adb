with System.Storage_Elements;
with Desktop_Heap_Objects;
package body Desktop_Wallpaper_Store.Rasters with SPARK_Mode => Off,
  Refined_State => (Status_Table => (Statuses, Count_Now), Raster_Table => Rasters)
is
   use type System.Address;
   use type System.Storage_Elements.Storage_Count;

   type Raster is record
      Base : System.Address := System.Null_Address;
      Pixels : Q.Pixel_Count := 0;
      Width, Height : Q.Side := 1;
   end record;
   type Status_Array is array (Image range <>) of Status;
   type Raster_Array is array (Image range <>) of Raster;
   package Status_Heap is new Desktop_Heap_Objects (Status);
   package Raster_Heap is new Desktop_Heap_Objects (Raster);
   package Pixel_Heap is new Desktop_Heap_Objects (Q.Pixel);

   Count_Now : Image_Count := 0;
   Statuses : System.Address := System.Null_Address;
   Rasters : System.Address := System.Null_Address;

   function Tracked return Image_Count is (Count_Now);

   procedure Reserve (Count : Image_Count; Done : out Boolean) is
   begin
      Done := Count = 0;
      if Count = 0 then
         return;
      end if;
      Statuses := Status_Heap.Allocate
        (System.Storage_Elements.Storage_Count (Count) * Status'Max_Size_In_Storage_Elements);
      Rasters := Raster_Heap.Allocate
        (System.Storage_Elements.Storage_Count (Count) * Raster'Max_Size_In_Storage_Elements);
      if Statuses = System.Null_Address or else Rasters = System.Null_Address then
         Status_Heap.Free (Statuses);
         Raster_Heap.Free (Rasters);
         Statuses := System.Null_Address;
         Rasters := System.Null_Address;
         return;
      end if;
      declare
         S : Status_Array (1 .. Count) with Import, Address => Statuses;
         R : Raster_Array (1 .. Count) with Import, Address => Rasters;
      begin
         for Asset in 1 .. Count loop
            S (Asset) := Not_Loaded;
            R (Asset) := (others => <>);
         end loop;
      end;
      Count_Now := Count;
      Done := True;
   end Reserve;

   function Status_Of (Asset : Image) return Status is
      S : constant Status_Array (1 .. Count_Now) with Import, Address => Statuses;
   begin
      return (if Asset <= Count_Now then S (Asset) else Not_Loaded);
   end Status_Of;

   procedure Set_Status (Asset : Image; Value : Status) is
      S : Status_Array (1 .. Count_Now) with Import, Address => Statuses;
   begin
      S (Asset) := Value;
   end Set_Status;

   function Item (Asset : Image) return Raster is
      R : constant Raster_Array (1 .. Count_Now) with Import, Address => Rasters;
   begin
      return (if Asset <= Count_Now then R (Asset) else (others => <>));
   end Item;

   function Raster_Pixels (Asset : Image) return Q.Pixel_Count is (Item (Asset).Pixels);
   function Width_Of (Asset : Image) return Q.Side is (Item (Asset).Width);
   function Height_Of (Asset : Image) return Q.Side is (Item (Asset).Height);
   function Address_Of (Asset : Image) return System.Address is (Item (Asset).Base);

   procedure Allocate (Asset : Image; Width, Height : Q.Side; Done : out Boolean) is
      R : Raster_Array (1 .. Count_Now) with Import, Address => Rasters;
      Count : constant Q.Pixel_Count := Width * Height;
      Where : constant System.Address :=
        Pixel_Heap.Allocate (System.Storage_Elements.Storage_Count (Count) * Q.Pixel'Max_Size_In_Storage_Elements);
   begin
      Done := Where /= System.Null_Address;
      if Done then
         R (Asset) := (Base => Where, Pixels => Count, Width => Width, Height => Height);
      end if;
   end Allocate;

   procedure Feed (Asset : Image; D : in out Q.Decoder; Input : Q.Byte_Array) is
      Pixels : Q.Pixel_Buffer (0 .. Item (Asset).Pixels - 1) with Import, Address => Item (Asset).Base;
   begin
      Q.Feed (D, Input, Pixels);
   end Feed;

   procedure Free (Asset : Image) is
      R : Raster_Array (1 .. Count_Now) with Import, Address => Rasters;
   begin
      if Asset <= Count_Now then
         Pixel_Heap.Free (R (Asset).Base);
         R (Asset) := (others => <>);
      end if;
   end Free;
end Desktop_Wallpaper_Store.Rasters;
