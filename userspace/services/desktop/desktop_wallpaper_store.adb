package body Desktop_Wallpaper_Store with SPARK_Mode,
  Refined_State => (State => (Statuses, Loading_Now, Decoder, Mismatch,
                              Cubes_Raster, Cubie_Raster))
is
   use type Q.Phase;

   Cubes_Pixels : constant := P.Wallpaper_Width * P.Wallpaper_Height;
   Cubie_Pixels : constant := P.Cubie_Width * P.Cubie_Height;

   --  Indexed by every backdrop; only the Image entries ever change.
   type Status_Table is array (A.Background) of Status;
   Statuses    : Status_Table := [others => Not_Loaded];
   Loading_Now : Boolean := False;
   Decoder     : Q.Decoder;
   Mismatch    : Boolean := False;
   --  Zero-initialised, so they occupy .bss, not the executable.
   Cubes_Raster : Q.Pixel_Buffer (0 .. Cubes_Pixels - 1) := [others => 0];
   Cubie_Raster : Q.Pixel_Buffer (0 .. Cubie_Pixels - 1) := [others => 0];

   function Current (Asset : Image) return Status is (Statuses (Asset))
     with Refined_Global => Statuses;
   function Busy return Boolean is (Loading_Now) with Refined_Global => Loading_Now;

   function Capacity (Asset : Image) return Q.Pixel_Limit is
     (if Asset = A.Cubie then Cubie_Pixels else Cubes_Pixels);

   --  The decoder is state across calls: its consistency with the asset
   --  being loaded is checked once per chunk, not assumed.
   function Decoding (Asset : Image) return Boolean is
     (Q.Valid (Decoder) and then Q.Limit (Decoder) = Capacity (Asset))
     with Global => Decoder;

   function Pixels (Asset : Image) return System.Address
     with SPARK_Mode => Off
   is
   begin
      return (if Asset = A.Cubie then Cubie_Raster'Address else Cubes_Raster'Address);
   end Pixels;

   procedure Begin_Load (Asset : Image) is
   begin
      Q.Start (Decoder, Capacity (Asset));
      Mismatch := False;
      Loading_Now := True;
      Statuses (Asset) := Loading;
   end Begin_Load;

   procedure Feed (Asset : Image; Input : Q.Byte_Array) is
   begin
      if Mismatch or else not Decoding (Asset) or else Q.Current (Decoder) = Q.Failed then
         return;
      end if;
      if Asset = A.Cubie then
         Q.Feed (Decoder, Input, Cubie_Raster);
      else
         Q.Feed (Decoder, Input, Cubes_Raster);
      end if;
      --  Reject another size as soon as the header says so.
      if Q.Current (Decoder) in Q.Reading_Pixels | Q.Reading_Marker | Q.Complete and then
        (Q.Width (Decoder) /= P.Width (Asset) or else Q.Height (Decoder) /= P.Height (Asset))
      then
         Mismatch := True;
      end if;
   end Feed;

   function Wants_More (Asset : Image) return Boolean is
     (not Mismatch and then Decoding (Asset) and then Q.Current (Decoder) /= Q.Failed)
     with Refined_Global => (Input => (Mismatch, Decoder), Proof_In => (Loading_Now, Statuses));

   procedure End_Load (Asset : Image; Read_Failed : Boolean; Result : out Load_Result) is
   begin
      Result := (others => <>);
      if Decoding (Asset) then
         Q.Finish (Decoder);
         Result := (Loaded => not Read_Failed and then not Mismatch and then
                              Q.Current (Decoder) = Q.Complete,
                    Error => Q.Error (Decoder), Wrong_Size => Mismatch,
                    Width => Q.Width (Decoder), Height => Q.Height (Decoder));
      end if;
      Statuses (Asset) := (if Result.Loaded then Loaded else Unavailable);
      Loading_Now := False;
   end End_Load;

   procedure Give_Up (Asset : Image) is
   begin
      Statuses (Asset) := Unavailable;
   end Give_Up;
end Desktop_Wallpaper_Store;
