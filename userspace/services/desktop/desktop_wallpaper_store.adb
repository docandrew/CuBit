with Desktop_Wallpaper_Store.Rasters;
package body Desktop_Wallpaper_Store with SPARK_Mode,
  Refined_State => (State => (Loading_Now, Decoder, Header, Have, Sized, Stopped,
                              Header_Error, Refused_Now, Demand_Now, Used_Bytes, Pending_Bytes,
                              Desktop_Wallpaper_Store.Rasters.Status_Table,
                              Desktop_Wallpaper_Store.Rasters.Raster_Table))
is
   use type Q.Phase, Q.Failure;
   package Memory renames Desktop_Wallpaper_Store.Rasters;

   subtype Header_Fill is Natural range 0 .. Q.Header_Bytes;

   Loading_Now   : Boolean := False;
   Decoder       : Q.Decoder;
   --  The file's first bytes, until its header is whole.
   Header        : Q.Byte_Array (1 .. Q.Header_Bytes) := [others => 0];
   Have          : Header_Fill := 0;
   --  The header was whole and valid, and the raster was allocated.
   Sized         : Boolean := False;
   --  The stream was rejected before its pixels (header, ceiling, memory).
   Stopped       : Boolean := False;
   Header_Error  : Q.Failure := Q.No_Failure;
   Refused_Now   : Refusal := Accepted;
   Demand_Now    : Demand_Bytes := 0;
   Used_Bytes    : Byte_Count := 0;
   --  The raster of the image being loaded, counted in Used_Bytes.
   Pending_Bytes : Byte_Count := 0;

   function Capacity return Image_Count is (Memory.Tracked)
     with Refined_Global => Memory.Status_Table;
   function Current (Asset : Image) return Status is (Memory.Status_Of (Asset))
     with Refined_Global => Memory.Status_Table;
   function Busy return Boolean is (Loading_Now) with Refined_Global => Loading_Now;
   function Used return Byte_Count is (Used_Bytes) with Refined_Global => Used_Bytes;

   function Width (Asset : Image) return Q.Side is (Memory.Width_Of (Asset))
     with Refined_Global => (Input => Memory.Raster_Table, Proof_In => Memory.Status_Table);
   function Height (Asset : Image) return Q.Side is (Memory.Height_Of (Asset))
     with Refined_Global => (Input => Memory.Raster_Table, Proof_In => Memory.Status_Table);
   function Pixels (Asset : Image) return System.Address is (Memory.Address_Of (Asset))
     with Refined_Global => (Input => Memory.Raster_Table, Proof_In => Memory.Status_Table);

   procedure Reserve (Count : Image_Count; Done : out Boolean) is
   begin
      Memory.Reserve (Count, Done);
   end Reserve;

   --  The decoder is state across calls: its consistency with the raster
   --  being filled is checked once per chunk, not assumed.
   function Decoding (Asset : Image) return Boolean is
     (Sized and then Q.Valid (Decoder) and then Memory.Raster_Pixels (Asset) > 0 and then
      Q.Limit (Decoder) <= Memory.Raster_Pixels (Asset))
     with Global => (Sized, Decoder, Memory.Raster_Table);

   procedure Begin_Load (Asset : Image) is
   begin
      Q.Start (Decoder, 1);
      Header := [others => 0];
      Have := 0;
      Sized := False;
      Stopped := False;
      Header_Error := Q.No_Failure;
      Refused_Now := Accepted;
      Demand_Now := 0;
      Pending_Bytes := 0;
      Loading_Now := True;
      Memory.Set_Status (Asset, Loading);
   end Begin_Load;

   --  The header is whole: check it, check the ceiling, take the raster and
   --  start decoding it (from the header bytes).
   procedure Size_Raster (Asset : Image)
     with Global => (In_Out => (Decoder, Sized, Stopped, Header_Error, Refused_Now, Demand_Now,
                                Used_Bytes, Pending_Bytes, Memory.Raster_Table),
                     Input => Header, Proof_In => Memory.Status_Table),
          Pre  => not Sized and then not Stopped and then Asset <= Memory.Tracked and then
                  Memory.Raster_Pixels (Asset) = 0,
          Post => Sized or else Stopped
   is
      Info : constant Q.Header_Info := Q.Inspect (Header);
      Done : Boolean;
   begin
      if Info.Error /= Q.No_Failure then
         Header_Error := Info.Error;
         Stopped := True;
         return;
      end if;
      declare
         --  Both sides are at most Q.Maximum_Side: the product fits.
         Wanted : constant Demand_Bytes :=
           Demand_Bytes (Info.Width) * Demand_Bytes (Info.Height) * Pixel_Bytes;
      begin
         if Wanted > Demand_Bytes (Decoded_Ceiling - Used_Bytes) then
            Refused_Now := Over_Ceiling;
            Demand_Now := Demand_Bytes (Used_Bytes) + Wanted;
            Stopped := True;
            return;
         end if;
         declare
            Demand : constant Byte_Count := Byte_Count (Wanted);
            Count : constant Q.Pixel_Limit := Demand / Pixel_Bytes;
         begin
            pragma Assert (Count = Info.Width * Info.Height);
            Memory.Allocate (Asset, Info.Width, Info.Height, Done);
            if not Done then
               Refused_Now := No_Memory;
               Demand_Now := Demand_Bytes (Used_Bytes) + Wanted;
               Stopped := True;
               return;
            end if;
            Pending_Bytes := Demand;
            Used_Bytes := Used_Bytes + Demand;
            Q.Start (Decoder, Count);
            Sized := True;
            Memory.Feed (Asset, Decoder, Header);
         end;
      end;
   end Size_Raster;

   procedure Feed (Asset : Image; Input : Q.Byte_Array) is
      --  How many of Input's first bytes completed the header.
      Taken : Natural := 0;
   begin
      if Stopped then
         return;
      end if;
      if not Sized then
         for Next in Input'Range loop
            exit when Have = Q.Header_Bytes;
            Have := Have + 1;
            Header (Have) := Input (Next);
            Taken := Taken + 1;
            pragma Loop_Invariant (Taken = Next - Input'First + 1 and then Taken <= Have);
         end loop;
      end if;
      if Have < Q.Header_Bytes then
         return;
      end if;
      if not Sized then
         if Memory.Raster_Pixels (Asset) /= 0 then
            --  Never: each image loads once. Refuse rather than overwrite.
            Refused_Now := No_Memory;
            Stopped := True;
            return;
         end if;
         Size_Raster (Asset);
         if Stopped then
            return;
         end if;
      end if;
      if Taken < Input'Length and then Decoding (Asset) and then Q.Current (Decoder) /= Q.Failed then
         Memory.Feed (Asset, Decoder, Input (Input'First + Taken .. Input'Last));
      end if;
   end Feed;

   function Wants_More (Asset : Image) return Boolean is
     (not Stopped and then (not Sized or else (Decoding (Asset) and then Q.Current (Decoder) /= Q.Failed)))
     with Refined_Global => (Input => (Stopped, Sized, Decoder, Memory.Raster_Table),
                             Proof_In => (Loading_Now, Memory.Status_Table));

   procedure End_Load (Asset : Image; Read_Failed : Boolean; Result : out Load_Result) is
   begin
      Result := (Refused => Refused_Now, Demand => Demand_Now, others => <>);
      if Header_Error /= Q.No_Failure then
         Result.Error := Header_Error;
      elsif Refused_Now = Accepted and then not Sized then
         --  The file ended inside its header.
         Result.Error := Q.Truncated;
      end if;
      if Sized and then Decoding (Asset) then
         Q.Finish (Decoder);
         Result.Loaded := not Read_Failed and then Q.Current (Decoder) = Q.Complete;
         Result.Error := Q.Error (Decoder);
         Result.Width := Q.Width (Decoder);
         Result.Height := Q.Height (Decoder);
      end if;
      if not Result.Loaded then
         --  Its memory goes back: an image that did not decode holds none.
         if Memory.Raster_Pixels (Asset) > 0 then
            Memory.Free (Asset);
         end if;
         Used_Bytes := (if Pending_Bytes <= Used_Bytes then Used_Bytes - Pending_Bytes else 0);
      end if;
      Pending_Bytes := 0;
      Memory.Set_Status (Asset, (if Result.Loaded then Loaded else Unavailable));
      Loading_Now := False;
   end End_Load;

   procedure Give_Up (Asset : Image) is
   begin
      Memory.Set_Status (Asset, Unavailable);
   end Give_Up;
end Desktop_Wallpaper_Store;
